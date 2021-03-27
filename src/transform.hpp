#pragma once

#include <vector>
#include <array>
#include <algorithm>
#include <concepts>
#include <string>

#include <iostream>

#include "arithmeticTerm.hpp"
#include "pattern.hpp"

namespace bmath {

	namespace detail_ruleset {

		struct RuleHead { intern::pattern::PnIdx lhs, rhs; };

		struct RuleIter {
			const RuleHead* head;
			const intern::pattern::PnStore* store;

			using value_type = intern::pattern::RewriteRuleRef;
			using difference_type = std::ptrdiff_t;
			using pointer = value_type*;
			using reference = value_type&;
			using iterator_category = std::contiguous_iterator_tag;

			constexpr RuleIter& operator++() noexcept { ++this->head; return *this; }
			constexpr RuleIter operator++(int) noexcept { auto result = *this; ++(*this); return result; }
			constexpr RuleIter& operator+=(difference_type n) noexcept { this->head += n; return *this; }
			constexpr RuleIter operator+(difference_type n) const noexcept { auto result = *this; result += n; return result; }

			constexpr RuleIter& operator--() noexcept { --this->head; return *this; }
			constexpr RuleIter operator--(int) noexcept { auto result = *this; --(*this); return result; }
			constexpr RuleIter& operator-=(difference_type n) noexcept { this->head -= n; return *this; }
			constexpr RuleIter operator-(difference_type n) const noexcept { auto result = *this; result -= n; return result; }

			constexpr value_type  operator*() const noexcept { return value_type{ this->head->lhs, this->head->rhs, this->store }; }

			constexpr bool operator==(const RuleIter& snd) const noexcept { return this->head == snd.head; }
		};
	} //namespace detail_ruleset

	class RuleSet {
		intern::pattern::PnStore store = {};

		std::vector<detail_ruleset::RuleHead> heads;

	public:
		template<intern::ContainerOf<intern::pattern::RewriteRule> Rules>
		RuleSet(Rules&& intermediary_rules)
		{
			using namespace intern::pattern;
			std::stable_sort(intermediary_rules.begin(), intermediary_rules.end(),
				[](const RewriteRule& fst, const RewriteRule& snd)
					{ return fst.lhs_head.get_type() < snd.lhs_head.get_type(); }
			);

			//first insert all match sides -> direct succession of those
			for (std::size_t i = 0u; i < intermediary_rules.size(); i++) {
				const PnIdx lhs_head = pn_tree::copy(intermediary_rules[i].lhs_ref(), this->store);
				this->heads.push_back({ lhs_head, PnIdx() });
			}
			for (std::size_t i = 0u; i < intermediary_rules.size(); i++) {
				this->heads[i].rhs = pn_tree::copy(intermediary_rules[i].rhs_ref(), this->store);
			}
		}

		void apply_to(Term& term) const
		{
			using namespace bmath;
			using namespace bmath::intern;
			using namespace bmath::intern::pattern;

			term.establish_order();
		try_all_rules:
			for (std::size_t i = 0; i < this->heads.size(); i++) {
				const auto [head_match, deeper_match] =
					match::recursive_match_and_replace(
						PnRef(this->store, this->heads[i].lhs), 
						PnRef(this->store, this->heads[i].rhs), 
						term.mut_ref()
					);
				if (head_match) {
					term.head = *head_match;
				}
				if (head_match || deeper_match) {
					term.establish_order();
					//std::cout << " rule: " << intern::pattern::RewriteRuleRef{ this->heads[i].lhs, this->heads[i].rhs, &this->store }.to_string() << "\n";
					//std::cout << "  ->   " << term.to_string() << "\n";
					goto try_all_rules;
				}
			}
		}

		detail_ruleset::RuleIter begin() const noexcept { return { this->heads.data(), &this->store }; }
		detail_ruleset::RuleIter end()   const noexcept { return { this->heads.data() + this->heads.size(), &this->store }; }
	};


} //namespace bmath