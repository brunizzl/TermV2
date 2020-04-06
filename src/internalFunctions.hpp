#pragma once	//NOLINT

#include "baseTerm.hpp"

#include <string_view>

namespace bmath::intern {

	namespace order {
		//used to sort terms by uniqueness
		//less unique is greater
		std::strong_ordering compare_uniqueness(Type fst, Type snd);

		//used to determine if parentheses are needed when displaying term with fst and snd only one layer apart
		//higher operator precedence is greater
		std::strong_ordering compare_precedence(Type fst, Type snd);

	} //namespace order

	namespace construction {

	} //namespace construction

	namespace matching {

	} //namespace matching

} //namespace bmath::intern