#pragma once

#include <vector>
#include <array>
#include <algorithm>
#include <concepts>
#include <string>

#include "arithmeticTerm.hpp"
#include "pattern.hpp"

namespace bmath {

	template<std::size_t N>
	class RuleSet {
		intern::pattern::PnStore lhs_store = {};
		intern::pattern::PnStore rhs_store = {};

		std::array<intern::pattern::PnIdx, N> lhs_heads;
		std::array<intern::pattern::PnIdx, N> rhs_heads;

	public:
		template<typename Name>
		RuleSet(const std::array<Name, N>& rule_names)
		{
			using namespace intern::pattern;
			std::vector<RewriteRule> rules;
			rules.reserve(rule_names.size());
			for (const std::string_view rule_name : rule_names) {
				rules.emplace_back(std::string(rule_name));
			}

			std::stable_sort(rules.begin(), rules.end(),
				[](const RewriteRule& fst, const RewriteRule& snd)
					{ return fst.lhs_head.get_type() < snd.lhs_head.get_type(); }
			);

			for (std::size_t i = 0u; i < rules.size(); i++) {
				this->lhs_heads[i] = pn_tree::copy(rules[i].lhs_ref(), this->lhs_store);
			}
			for (std::size_t i = 0u; i < rules.size(); i++) {
				this->rhs_heads[i] = pn_tree::copy(rules[i].rhs_ref(), this->rhs_store);
			}
		}

		void apply_to(Term& term) const
		{
			using namespace bmath;
			using namespace bmath::intern;
			using namespace bmath::intern::pattern;

			term.establish_order();
		try_all_rules:
			for (std::size_t i = 0; i < N; i++) {
				const auto [head_match, deeper_match] =
					match::recursive_match_and_replace(
						PnRef(this->lhs_store, this->lhs_heads[i]), 
						PnRef(this->rhs_store, this->rhs_heads[i]), 
						term.mut_ref()
					);
				if (head_match) {
					term.head = *head_match;
				}
				if (head_match || deeper_match) {
					term.establish_order();
					//std::cout << " rule: " << rule->to_string() << "\n";
					//std::cout << "  ->   " << print::to_pretty_string(ref.new_at(head)) << "\n\n\n";
					goto try_all_rules;
				}
			}
		}
	};

	template<typename Name, std::size_t N>
	RuleSet(const std::array<Name, N>& rule_names) -> RuleSet<N>;


} //namespace bmath