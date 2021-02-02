#pragma once

#include <vector>
#include <array>
#include <algorithm>

#include "arithmeticTerm.hpp"
#include "pattern.hpp"

//TODO: fill file with live

namespace bmath::intern::transform {

	template<std::size_t N>
	class RuleSet {
		pattern::PnStore lhs_store = {};
		pattern::PnStore rhs_store = {};

		std::array<pattern::PnIdx, N> lhs_heads;
		std::array<pattern::PnIdx, N> rhs_heads;

	public:
		RuleSet(std::array<pattern::RewriteRule, N>&& init)
		{
			using namespace pattern;
			std::stable_sort(init.begin(), init.end(),
				[](const RewriteRule& fst, const RewriteRule& snd)
					{ return fst.lhs_head.get_type() < snd.lhs_head.get_type(); }
			);

		}
	};

} //namespace bmath::intern::transform