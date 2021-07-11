#pragma once

#include "types.hpp"
#include "algorithms.hpp"


namespace simp {

	//term without any pattern shenaniganz
	struct LiteralTerm
	{
		Store store;
		NodeIndex head;

		LiteralTerm(std::string name);

		std::string to_string(const bool fancy = true) const noexcept;

		std::string to_memory_layout() const noexcept;

		constexpr Ref ref() const noexcept { return Ref(this->store, this->head); }

		constexpr MutRef mut_ref() noexcept { return MutRef(this->store, this->head); }

		void normalize(const Options options)
		{	this->head = normalize::recursive(this->mut_ref(), options);
		}
	}; //struct LiteralTerm

	struct RuleRef
	{
		UnsaveRef lhs;
		UnsaveRef rhs;

		std::string to_string() const noexcept;
	};

	struct RuleSetEntry
	{ //lhs and rhs are same as in RuleHead
		NodeIndex lhs; //match side
		NodeIndex rhs; //replace side
		int subset_size; //size of range in need to be checked, if a literal starts matching at entry
	};

	struct RuleSetIter 
	{
		std::vector<RuleSetEntry>::const_iterator iter;
		const TermNode* const store_data;

		using value_type = RuleRef;
		using difference_type = std::ptrdiff_t;
		using pointer = void;
		using reference = void;
		using iterator_category = std::contiguous_iterator_tag;

		RuleSetIter(const RuleSetIter& snd) noexcept :iter(snd.iter), store_data(snd.store_data) {}
		RuleSetIter(const std::vector<RuleSetEntry>::const_iterator iter_, const TermNode* const store_data_) noexcept
			:iter(iter_), store_data(store_data_) {}

		RuleSetIter& operator=(const RuleSetIter& snd) noexcept
		{
			assert(this->store_data == snd.store_data);
			this->iter = snd.iter;
			return *this;
		}

		RuleSetIter& operator++() noexcept { ++this->iter; return *this; }
		RuleSetIter operator++(int) noexcept { auto result = *this; ++(*this); return result; }
		RuleSetIter& operator+=(difference_type n) noexcept { this->iter += n; return *this; }
		RuleSetIter operator+(difference_type n) const noexcept { auto result = *this; result += n; return result; }

		RuleSetIter& operator--() noexcept { --this->iter; return *this; }
		RuleSetIter operator--(int) noexcept { auto result = *this; --(*this); return result; }
		RuleSetIter& operator-=(difference_type n) noexcept { this->iter -= n; return *this; }
		RuleSetIter operator-(difference_type n) const noexcept { auto result = *this; result -= n; return result; }

		difference_type operator-(const RuleSetIter& snd) const noexcept { return this->iter - snd.iter; }

		RuleRef operator*() const 
		{
			const std::uint32_t lhs_index = this->iter->lhs.get_index();
			const std::uint32_t rhs_index = this->iter->rhs.get_index();
			return { UnsaveRef(this->store_data, lhs_index, this->iter->lhs.get_type()),
			         UnsaveRef(this->store_data, rhs_index, this->iter->rhs.get_type()) };
		}

		bool operator==(const RuleSetIter& snd) const noexcept 
		{
			assert(this->store_data == snd.store_data);
			return this->iter == snd.iter;
		}

		bool operator<(const RuleSetIter& snd) const noexcept
		{
			assert(this->store_data == snd.store_data);
			return this->iter < snd.iter;
		}
	};

	struct RuleSet
	{
		MonotonicStore store;
		std::vector<RuleSetEntry> rules;

		RuleSet(std::initializer_list<std::string_view> names_, 
			RuleHead(*build)(Store&, std::string&) = build_rule::build_everything);

		void add(std::initializer_list<const RuleSet*> sets);

		RuleSetIter begin() const noexcept { return { this->rules.begin(), this->store.data() }; }
		RuleSetIter end() const noexcept { return { this->rules.end(), this->store.data() }; }

	private:
		void migrate_rules(const Store& temp_store);
	};

	struct RuleApplicationRes
	{
		NodeIndex result_term;
		RuleSetIter rule;
	};
	//tries to match every maybe applicable rule, returns first succesfull rule application,
	//the old ref is not deleted.
	//if no match was found, { invalid_index, undefined } is returned
	[[nodiscard]] RuleApplicationRes raw_apply_ruleset(const RuleSet& rules, const MutRef ref,
		match::State& state, const Options options);

	//same as above, only handles storage, returns new ref's index.
	[[nodiscard]] NodeIndex shallow_apply_ruleset(const RuleSet& rules, MutRef ref, const Options options);

	//returns the first max_size distinct normal forms of ref
	[[nodiscard]] std::vector<NodeIndex> nondeterministic_shallow_apply_ruleset(const RuleSet& rules, 
		const MutRef ref, const Options options, const std::size_t max_size = 16);

	//returns function returning smallest (determined by weight_fun) normal form of ref found by nondeterministic_shallow_apply_ruleset
	template<std::invocable<UnsaveRef> WeightFun>
	[[nodiscard]] auto shallow_apply_and_choose(WeightFun weight_fun)
	{
		return [weight_fun](const RuleSet& rules, MutRef ref, const Options options) {
			const std::vector<NodeIndex> normal_forms = nondeterministic_shallow_apply_ruleset(rules, ref, options);
			const NodeIndex best = [&] {
				auto iter = normal_forms.begin();
				const auto stop = normal_forms.end();
				long long current_best_weight = weight_fun(UnsaveRef(ref.at(*iter)));
				auto current_best_iter = iter;
				while (++iter != stop) {
					const long long iter_weight = weight_fun(UnsaveRef(ref.at(*iter)));
					if (iter_weight < current_best_weight) {
						current_best_weight = iter_weight;
						current_best_iter = iter;
					}
				}
				return *current_best_iter;
			}();
			for (const auto normal_form : normal_forms) {
				if (normal_form != best) {
					free_tree(ref.at(normal_form));
				}
			}
			return best;
		};
	} //shallow_apply_and_choose


	//helper for apply_ruleset
	template<CallableTo<NodeIndex, const RuleSet&, MutRef, Options> ShallowApply>
	NodeIndex recursive_apply(const RuleSet& rules, const MutRef ref, const Options options, ShallowApply shallow_apply)
	{
		{ //try replacing this
			const NodeIndex old = ref.typed_idx();
			const NodeIndex new_ = shallow_apply(rules, ref, options);
			if (old != new_) return new_;
		}
		if (ref.type == Literal::f_app) { //try replacing subterms
			bool change = false;
			const auto stop = end(ref);
			for (auto subterm = begin(ref); subterm != stop; ++subterm) {
				const NodeIndex sub_result = recursive_apply(rules, ref.at(*subterm), options, shallow_apply);
				if (sub_result != invalid_index) {
					*subterm = sub_result;
					change = true;
				}
			}
			if (change) return normalize::outermost(ref, options).res;
		}
		return invalid_index;
	} //recursive_apply

	//lazyliy applies first rule applicable in depth first search in ref until no further rules can be applied
	template<CallableTo<NodeIndex, const RuleSet&, MutRef, Options> ShallowApply>
	[[nodiscard]] NodeIndex apply_ruleset(const RuleSet& rules, MutRef ref, const Options options, ShallowApply shallow_apply)
	{
	apply:
		NodeIndex result = recursive_apply(rules, ref, options, shallow_apply);
		if (result != invalid_index) {
			ref.type = result.get_type();
			ref.index = result.get_index();
			goto apply;
		}
		return ref.typed_idx();
	} //apply_ruleset


	inline [[nodiscard]] NodeIndex greedy_apply_ruleset(const RuleSet& rules, MutRef ref, const Options options)
	{
		return apply_ruleset(rules, ref, options, 
			[](const RuleSet& rs, MutRef r, Options os) { return shallow_apply_ruleset(rs, r, os); });
	}

	template<std::invocable<UnsaveRef> WeightFun>
	[[nodiscard]] NodeIndex nondeterministic_apply_ruleset(const RuleSet& rules, MutRef ref, const Options options, WeightFun weight_fun)
	{
		return apply_ruleset(rules, ref, options, shallow_apply_and_choose(weight_fun));
	}

} //namespace simp
