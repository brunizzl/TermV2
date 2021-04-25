#include "rewrite.hpp"

#include <string>
#include <tuple>
#include <iostream>
#include <algorithm>

#include "algorithms.hpp"
#include "parseTerm.hpp"
#include "io.hpp"

namespace simp {

	simp::LiteralTerm::LiteralTerm(std::string name)
	{
		using namespace bmath::intern;
		auto parse_str = ParseString(name);
		parse_str.allow_implicit_product(token::sticky_space, ' ');
		parse_str.remove_space();
		parse::name_lookup::LiteralInfos lambda_params;
		this->head = parse::build(this->store, lambda_params, parse_str);
	}

	std::string LiteralTerm::to_string() const noexcept
	{
		std::string res;
		print::append_to_string(this->ref(), res, 0);
		return res;
	}

	std::string LiteralTerm::to_memory_layout() const noexcept
	{
		return print::to_memory_layout(this->store, {this->head});
	}

	std::string RuleRef::to_string() const noexcept
	{
		std::string res;
		print::append_to_string(this->lhs, res, 0);
		res.append(" = ");
		print::append_to_string(this->rhs, res, 0);
		return res;
	}

	RuleSet::RuleSet(std::initializer_list<std::string_view> names, RuleHead(*build)(Store&, std::string&))
	{
		Store temp_store;
		for (const std::string_view name_view : names) {
			std::string name = std::string(name_view.data(), name_view.size());
			try {
				this->rules.push_back(build(temp_store, name));
			}
			catch (bmath::ParseFailure failure) {
				std::cerr << "parse failure: " << failure.what << "\n";
				std::cerr << name << "\n";
				std::cerr << std::string(failure.where, ' ') << "^\n";
				throw std::exception();
			}
			catch (TypeError error) {
				std::cerr << "type error: " << error.what << "\n";
				std::cerr << print::to_string(error.occurence) << "\n";
				throw std::exception();
			}
		}
		this->migrate_rules(temp_store);
	} //RuleSet::RuleSet

	void RuleSet::add(std::initializer_list<const RuleSet*> sets)
	{
		Store temp_store;
		for (RuleHead& rule : this->rules) {
			rule.lhs = copy_tree(UnsaveRef(&this->store.at(rule.lhs.get_index()), rule.lhs.get_index(), rule.lhs.get_type()), temp_store);
			rule.rhs = copy_tree(UnsaveRef(&this->store.at(rule.rhs.get_index()), rule.rhs.get_index(), rule.rhs.get_type()), temp_store);
		}
		for (const RuleSet* set : sets) {
			for (const RuleHead rule : set->rules) {
				this->rules.emplace_back(
					copy_tree(UnsaveRef(&set->store.at(rule.lhs.get_index()), rule.lhs.get_index(), rule.lhs.get_type()), temp_store),
					copy_tree(UnsaveRef(&set->store.at(rule.rhs.get_index()), rule.rhs.get_index(), rule.rhs.get_type()), temp_store));
			}
		}
		this->migrate_rules(temp_store);
	} //RuleSet::add

	RuleRange RuleSet::applicable_rules(const UnsaveRef ref) const noexcept
	{
		//TODO: binary search start and end
		return { this->begin(), this->end() };
	}

	void RuleSet::migrate_rules(const Store& temp_store)
	{
		std::stable_sort(this->rules.begin(), this->rules.end(),
			[&temp_store](const RuleHead fst, const RuleHead snd) {
				return unsure_compare_tree(Ref(temp_store, fst.lhs), Ref(temp_store, snd.lhs))
					== std::partial_ordering::less;
			});
		this->store.free_all();
		//first insert all match sides -> direct succession of those
		for (std::size_t i = 0u; i < this->rules.size(); i++) {
			this->rules[i].lhs = copy_tree(Ref(temp_store, this->rules[i].lhs), this->store);
		}
		for (std::size_t i = 0u; i < this->rules.size(); i++) {
			this->rules[i].rhs = copy_tree(Ref(temp_store, this->rules[i].rhs), this->store);
		}
	} //RuleSet::migrate_rules

	RuleApplicationRes raw_shallow_apply_ruleset(const RuleSet& rules, const Ref ref, Store& dst_store, 
		const unsigned lambda_param_offset, match::MatchData& match_data)
	{
		const RuleRange applicable_rules = rules.applicable_rules(ref);
		const RuleSetIter stop = applicable_rules.end();
		for (RuleSetIter iter = applicable_rules.begin(); iter != stop; ++iter) {
			const RuleRef rule = *iter;
			if (match::match_(rule.lhs, ref, match_data)) {
				const NodeIndex res = copy_pattern_interpretation(
					rule.rhs, match_data, *ref.store, dst_store, lambda_param_offset);
				return { res, iter };
			}
		}
		return { literal_nullptr, stop };
	} //shallow_apply_ruleset

	NodeIndex shallow_apply_ruleset(const RuleSet& rules, MutRef ref)
	{
	apply_ruleset:
		match::MatchData match_data = ref.store->data();
		RuleApplicationRes result = raw_shallow_apply_ruleset(rules, ref, *ref.store, 0, match_data);
		if (result.result_term != literal_nullptr) {
			free_tree(ref);
			ref.index = result.result_term.get_index();
			ref.type = result.result_term.get_type();
			goto apply_ruleset;
		}
		return ref.typed_idx();
	} //shallow_apply_ruleset

	NodeIndex recursive_greedy_apply(const RuleSet& rules, MutRef ref, const unsigned lambda_param_offset) {
		{ //try replacing this
			match::MatchData match_data = ref.store->data();
			const RuleApplicationRes applied = raw_shallow_apply_ruleset(rules, ref, *ref.store, lambda_param_offset, match_data);
			if (applied.result_term != literal_nullptr) {
				free_tree(ref);
				return applied.result_term;
			}
		}
		if (ref.type == Literal::call) {
			bool change = false;
			const auto stop = end(ref);
			for (auto subterm = begin(ref); subterm != stop; ++subterm) {
				const NodeIndex sub_result = recursive_greedy_apply(rules, ref.new_at(*subterm), lambda_param_offset);
				if (sub_result != literal_nullptr) {
					*subterm = sub_result;
					change = true;
				}
			}
			if (change) {
				return normalize::outermost(ref, {}, lambda_param_offset).res;
			}
		}
		else if (ref.type == Literal::lambda) {
			const Lambda lambda = *ref;
			const NodeIndex sub_result = recursive_greedy_apply(
				rules, ref.new_at(lambda.definition), lambda_param_offset + lambda.param_count);
			if (sub_result != literal_nullptr) {
				ref->lambda.definition = sub_result;
				return normalize::outermost(ref, {}, lambda_param_offset).res;
			}
		}
		return literal_nullptr;
	} //recursive_greedy_apply

	NodeIndex greedy_apply_ruleset(const RuleSet& rules, MutRef ref)
	{
	apply_ruleset:
		NodeIndex result = recursive_greedy_apply(rules, ref, 0);
		if (result != literal_nullptr) {
			ref.type = result.get_type();
			ref.index = result.get_index();
			goto apply_ruleset;
		}
		return ref.typed_idx();
	} //greedy_apply_ruleset

} //namespace simp
