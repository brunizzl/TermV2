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

		std::string to_string() const noexcept;

		std::string to_memory_layout() const noexcept;

		constexpr Ref ref() const noexcept { return Ref(this->store, this->head); }

		constexpr MutRef mut_ref() noexcept { return MutRef(this->store, this->head); }

		void normalize(const normalize::Options o = {})
		{
			this->head = normalize::recursive(this->mut_ref(), o, 0);
		}
	}; //struct LiteralTerm

	struct RuleRef
	{
		UnsaveRef lhs;
		UnsaveRef rhs;

		std::string to_string() const noexcept;
	};

	struct RuleSetIter 
	{
		std::vector<RuleHead>::const_iterator iter;
		const TermNode* const store_data;

		using value_type = RuleRef;
		using difference_type = void;
		using pointer = void;
		using reference = void;
		using iterator_category = std::forward_iterator_tag; //TODO: make random access iterator

		RuleSetIter& operator++() noexcept { ++this->iter; return *this; }
		RuleSetIter operator++(int) noexcept { auto result = *this; ++(*this); return result; }

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
	};

	struct RuleRange 
	{
		RuleSetIter start, stop;
		RuleSetIter begin() const noexcept { return this->start; }
		RuleSetIter end() const noexcept { return this->stop; }
	};

	struct RuleSet
	{
		MonotonicStore store;
		std::vector<RuleHead> rules;

		RuleSet(std::initializer_list<std::string_view> names, 
			RuleHead(*build)(Store&, std::string&) = build_rule::build_everything);

		void add(std::initializer_list<const RuleSet*> sets);

		RuleSetIter begin() const noexcept { return { this->rules.begin(), this->store.data() }; }
		RuleSetIter end() const noexcept { return { this->rules.end(), this->store.data() }; }

		RuleRange applicable_rules(const UnsaveRef ref) const noexcept;

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
	//if no match was found, { literal_nullptr, undefined } is returned
	[[nodiscard]] RuleApplicationRes raw_shallow_apply_ruleset(const RuleSet& rules, const Ref ref,
		Store& dst_store, const unsigned lambda_param_offset, match::MatchData& match_data);

	//same as above, only handles storage, returns new ref's index.
	[[nodiscard]] NodeIndex shallow_apply_ruleset(const RuleSet& rules, MutRef ref);

	//lazyliy applies first rule applicable in depth first search in ref until no further rules can be applied
	[[nodiscard]] NodeIndex greedy_apply_ruleset(const RuleSet& rules, MutRef ref);

} //namespace simp
