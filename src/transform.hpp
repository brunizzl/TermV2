#pragma once

#include <vector>
#include <array>
#include <algorithm>
#include <concepts>
#include <string>

#include "arithmeticTerm.hpp"
#include "pattern.hpp"

namespace bmath {

	class RuleSet {
		intern::pattern::PnStore store = {};

		struct RuleHead { intern::pattern::PnIdx lhs, rhs; };
		std::vector<RuleHead> heads;

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
					//std::cout << " rule: " << rule->to_string() << "\n";
					//std::cout << "  ->   " << print::to_pretty_string(ref.new_at(head)) << "\n\n\n";
					goto try_all_rules;
				}
			}
		}
	};


} //namespace bmath