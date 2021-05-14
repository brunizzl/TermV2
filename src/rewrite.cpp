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
		res.append("\n   ->   ");
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
			rule.lhs = copy_tree(UnsaveRef(this->store.data(), rule.lhs.get_index(), rule.lhs.get_type()), temp_store);
			rule.rhs = copy_tree(UnsaveRef(this->store.data(), rule.rhs.get_index(), rule.rhs.get_type()), temp_store);
		}
		for (const RuleSet* set : sets) {
			for (const RuleHead rule : set->rules) {
				this->rules.emplace_back(
					copy_tree(UnsaveRef(set->store.data(), rule.lhs.get_index(), rule.lhs.get_type()), temp_store),
					copy_tree(UnsaveRef(set->store.data(), rule.rhs.get_index(), rule.rhs.get_type()), temp_store));
			}
		}
		this->migrate_rules(temp_store);
	} //RuleSet::add

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

	RuleApplicationRes raw_apply_ruleset(const RuleSet& rules, const Ref ref, Store& dst_store, 
		match::State& state, const Options options)
	{
		const auto stop = rules.end();
		RuleSetIter iter = [&] {
			const auto less = [](const RuleRef& rule, const UnsaveRef& ref) {
				return unsure_compare_tree(rule.lhs, ref) == std::partial_ordering::less;
			};
			return std::lower_bound(rules.begin(), stop, UnsaveRef(ref), less);
			//return rules.begin();
		}();

		for (; iter != stop; ++iter) {
			const RuleRef rule = *iter;
			const std::partial_ordering match_res = match::match_(rule.lhs, ref, state);
			if (match_res == std::partial_ordering::equivalent) {
				const NodeIndex res = pattern_interpretation(
					rule.rhs, state, *ref.store, dst_store, options);
				//std::cout << "from   " << print::to_string(ref) << "   to   " << print::to_string(Ref(dst_store, res)) << "\n";
				return { res, iter };
			}
			if (match_res == std::partial_ordering::greater) {
				break; //current rule is greater than ref and rules are only ever getting even greater -> no chance
			}
		}
		return { literal_nullptr, stop };
	} //shallow_apply_ruleset

	NodeIndex shallow_apply_ruleset(const RuleSet& rules, MutRef ref, const Options options)
	{
	apply_ruleset:
		match::State state = *ref.store;
		RuleApplicationRes result = raw_apply_ruleset(rules, ref, *ref.store, state, options);
		if (result.result_term != literal_nullptr) {
			free_tree(ref);
			ref.index = result.result_term.get_index();
			ref.type = result.result_term.get_type();
			goto apply_ruleset;
		}
		return ref.typed_idx();
	} //shallow_apply_ruleset

	NodeIndex recursive_greedy_apply(const RuleSet& rules, MutRef ref, const Options options) {
		{ //try replacing this
			match::State state = *ref.store;
			const RuleApplicationRes applied = raw_apply_ruleset(rules, ref, *ref.store, state, options);
			if (applied.result_term != literal_nullptr) {
				free_tree(ref);
				return applied.result_term;
			}
		}
		if (ref.type == Literal::f_app) {
			bool change = false;
			const auto stop = end(ref);
			for (auto subterm = begin(ref); subterm != stop; ++subterm) {
				const NodeIndex sub_result = recursive_greedy_apply(rules, ref.at(*subterm), options);
				if (sub_result != literal_nullptr) {
					*subterm = sub_result;
					change = true;
				}
			}
			if (change) {
				return normalize::outermost(ref, {}).res;
			}
		}
		return literal_nullptr;
	} //recursive_greedy_apply

	NodeIndex greedy_apply_ruleset(const RuleSet& rules, MutRef ref, const Options options)
	{
	apply_ruleset:
		NodeIndex result = recursive_greedy_apply(rules, ref, options);
		if (result != literal_nullptr) {
			ref.type = result.get_type();
			ref.index = result.get_index();
			goto apply_ruleset;
		}
		return ref.typed_idx();
	} //greedy_apply_ruleset

} //namespace simp
