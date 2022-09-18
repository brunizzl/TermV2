#pragma once

#include "types.hpp"
#include "algorithms.hpp"
#include "control.hpp"


namespace simp {

	enum class DebugPrintLevel
	{
		none, replacements, matched_vars, search_redex, test_rules
	};
	extern DebugPrintLevel debug_print_level;

	//term without any pattern shenaniganz
	struct LiteralTerm
	{
		Store& store;
		NodeIndex head;

		LiteralTerm(std::string name, Store& store_);

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

		std::string to_string(bool newline = true) const noexcept;
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
		std::vector<RuleSetEntry> heads;

		RuleSet(std::initializer_list<std::string_view> names_, 
			RuleHead(*build)(Store&, std::string&) = build_rule::build_everything);

		void add(std::initializer_list<const RuleSet*> sets);

		RuleSetIter begin() const noexcept { return { this->heads.begin(), this->store.data() }; }
		RuleSetIter end() const noexcept { return { this->heads.end(), this->store.data() }; }

		std::string to_string() const;

	private:
		void migrate_rules(const Store& temp_store);
	};

	//if ref is a redex, the first possible rule application is retuned, old ref is deleted.
	//assumes ref to be normalized via options
	[[nodiscard]] AlteredTerm greedy_shallow_apply_ruleset(const RuleSet& rules, MutRef ref, const Options options);

	//lazyliy applies first rule applicable in depth first search in ref until no further rules can be applied
	//assumes ref to be normalized via options
	[[nodiscard]] AlteredTerm greedy_lazy_apply_ruleset(const RuleSet& rules, MutRef ref, const Options options);

	//needs to be run between the application of two rulesets, 
	// as nodes marked final will not be searched for redexes
	inline void mark_not_final(const MutRef head) noexcept
	{
		ctrl::transform(head, [store = head.store](const MutRef& r)
			{ if (is_stored_node(r.type)) store->mark_final(r.index, false); });
	} //mark_not_final

} //namespace simp
