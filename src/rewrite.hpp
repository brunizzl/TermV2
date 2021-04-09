#pragma once

#include "types.hpp"
#include "algorithms.hpp"

namespace simp {

	//term without any pattern shenaniganz
	struct LiteralTerm
	{
		Store store;
		TypedIdx head;

		LiteralTerm(std::string name);

		std::string to_string() const noexcept;

		std::string to_memory_layout() const noexcept;

		constexpr Ref ref() const noexcept { return Ref(this->store, this->head); }

		constexpr MutRef mut_ref() noexcept { return MutRef(this->store, this->head); }

		bool establish_order(const combine::Options o = {}) 
		{ 
			const auto res = combine::lazy(this->mut_ref(), o, 0);
			this->head = res.res;
			return res.change;
		}
	}; //struct LiteralTerm

	struct RewriteRule
	{
		Store store;
		TypedIdx lhs_head; //start of match side
		TypedIdx rhs_head; //start of replace side

		RewriteRule(std::string name);

		std::string to_string() const noexcept;

		constexpr Ref lhs_ref() const noexcept { return Ref(this->store, this->lhs_head); }
		constexpr Ref rhs_ref() const noexcept { return Ref(this->store, this->rhs_head); }

		constexpr MutRef lhs_mut_ref() noexcept { return MutRef(this->store, this->lhs_head); }
		constexpr MutRef rhs_mut_ref() noexcept { return MutRef(this->store, this->rhs_head); }
	};

} //namespace simp
