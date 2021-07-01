#include "rewrite.hpp"

#include <string>
#include <tuple>
#include <iostream>
#include <algorithm>

#include "utility/queue.hpp"

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

	std::string LiteralTerm::to_string(const bool fancy) const noexcept
	{
		std::string res;
		print::append_to_string(this->ref(), res, 0, fancy);
		return res;
	}

	std::string LiteralTerm::to_memory_layout() const noexcept
	{
		return print::to_memory_layout(this->store, { this->head });
	}

	std::string RuleRef::to_string() const noexcept
	{
		std::string res;
		print::append_to_string(this->lhs, res, 0, false);
		res.append("\n   ->   ");
		print::append_to_string(this->rhs, res, 0, false);
		return res;
	}

	RuleSet::RuleSet(std::initializer_list<std::string_view> names, RuleHead(*build)(Store&, std::string&))
	{
		Store temp_store;
		for (const std::string_view name_view : names) {
			std::string name = std::string(name_view.data(), name_view.size());
			try {
				const RuleHead rule = build(temp_store, name);
				this->rules.emplace_back(rule.lhs, rule.rhs, 0);
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
		for (RuleSetEntry& rule : this->rules) {
			rule.lhs = copy_tree(UnsaveRef(this->store.data(), rule.lhs.get_index(), rule.lhs.get_type()), temp_store);
			rule.rhs = copy_tree(UnsaveRef(this->store.data(), rule.rhs.get_index(), rule.rhs.get_type()), temp_store);
		}
		for (const RuleSet* set : sets) {
			for (const RuleSetEntry rule : set->rules) {
				this->rules.emplace_back(
					copy_tree(UnsaveRef(set->store.data(), rule.lhs.get_index(), rule.lhs.get_type()), temp_store),
					copy_tree(UnsaveRef(set->store.data(), rule.rhs.get_index(), rule.rhs.get_type()), temp_store), 
					0);
			}
		}
		this->migrate_rules(temp_store);
	} //RuleSet::add

	void RuleSet::migrate_rules(const Store& temp_store)
	{
		std::stable_sort(this->rules.begin(), this->rules.end(),
			[&temp_store](const RuleSetEntry fst, const RuleSetEntry snd) {
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

		//set RuleSetEntry::subset_size
		using RevIter = std::vector<RuleSetEntry>::reverse_iterator;
		const auto to_lhs_ref = [store_data = this->store.data()](const RevIter iter) {
			return UnsaveRef(store_data, iter->lhs.get_index(), iter->lhs.get_type());
		};
		RevIter first_not_greater = this->rules.rbegin(); //first seen from the end, because reverse
		const RevIter reverse_end = this->rules.rend();
		for (RevIter iter = first_not_greater; iter != reverse_end; ++iter) {
			while (unsure_compare_tree(to_lhs_ref(first_not_greater), to_lhs_ref(iter)) == std::partial_ordering::greater) {
				assert(first_not_greater < iter);
				++first_not_greater;
			}
			iter->subset_size = iter - first_not_greater + 1; //+1, because we want to include first_not_greater in half open range
			assert(iter->subset_size > 0);
		}
	} //RuleSet::migrate_rules

	RuleSetIter start_point(const RuleSet& rules, const UnsaveRef ref)
	{
		const auto less = [](const RuleRef& rule, const UnsaveRef& r) {
			return unsure_compare_tree(rule.lhs, r) == std::partial_ordering::less;
		};
		assert(rules.rules.size()); //else rules.end() - 1 is illegal
		return std::lower_bound(rules.begin(), rules.end() - 1, ref, less); //always valid
	} //start_point

	RuleSetIter end_point(const RuleSetIter& start) { return start + start.iter->subset_size; }

	RuleApplicationRes raw_apply_ruleset(const RuleSet& rules, const MutRef ref, match::State& state, const Options options)
	{
		RuleSetIter iter = start_point(rules, ref);
		const RuleSetIter stop = end_point(iter); 

		for (; iter != stop; ++iter) {
			const RuleRef rule = *iter;
			if (match::matches(rule.lhs, ref, state)) {
				const NodeIndex res = pattern_interpretation(rule.rhs, state, *ref.store, options);
				return { res, iter };
			}
		}
		return { invalid_index, stop };
	} //raw_apply_ruleset

	NodeIndex shallow_apply_ruleset(const RuleSet& rules, MutRef ref, const Options options)
	{
	match::State state = *ref.store;
	apply_ruleset:
		RuleApplicationRes result = raw_apply_ruleset(rules, ref, state, options);
		if (result.result_term != invalid_index) {
			free_tree(ref);
			ref.index = result.result_term.get_index();
			ref.type = result.result_term.get_type();
			goto apply_ruleset;
		}
		return ref.typed_idx();
	} //shallow_apply_ruleset

	std::vector<NodeIndex> nondeterministic_shallow_apply_ruleset(const RuleSet& rules, const MutRef mother_ref, 
		const Options options, const std::size_t max_size)
	{
		std::vector<NodeIndex> normal_forms;
		Queue<NodeIndex, 1006> todo;
		todo.emplace_back(mother_ref.typed_idx());

		match::State state = *mother_ref.store;
		do {
			const MutRef ref = mother_ref.at(todo.pop_front());

			RuleSetIter iter = start_point(rules, ref);
			const RuleSetIter stop = end_point(iter);
			bool is_normal_form = true;
			for (; iter != stop; ++iter) {
				const RuleRef rule = *iter;
				if (match::matches(rule.lhs, ref, state)) {
					do {
						const NodeIndex res = pattern_interpretation(rule.rhs, state, *ref.store, options);
						if (res.get_type().value == NodeType::COUNT) __debugbreak();
						todo.emplace_back(res);
						is_normal_form = false;
					} while (match::rematch(rule.lhs, ref, state));
				}
			}
			if (is_normal_form) {
				normal_forms.push_back(ref.typed_idx());
				if (normal_forms.size() == max_size) {
					while (todo.size()) {
						free_tree(mother_ref.at(todo.pop_front()));
					}
					break;
				}
			}
			else {
				free_tree(ref);
			}
		} while (todo.size());
		std::cout << print::to_memory_layout(*mother_ref.store, normal_forms) << "\n\n\n";
		assert(normal_forms.size() > 0);
		return normal_forms;
	} //nondeterministic_shallow_apply_ruleset

} //namespace simp
