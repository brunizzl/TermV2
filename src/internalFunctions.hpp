#pragma once	//NOLINT

#include "baseTerm.hpp"

#include <string_view>

namespace bmath::intern {

	//array with instance of every Par_Op_Type to iterate over them
	static constexpr Par_Op_Type all_par_op_types[] = { Par_Op_Type::log10, Par_Op_Type::asinh, Par_Op_Type::acosh, Par_Op_Type::atanh,
		Par_Op_Type::asin, Par_Op_Type::acos, Par_Op_Type::atan, Par_Op_Type::sinh,
		Par_Op_Type::cosh, Par_Op_Type::tanh, Par_Op_Type::sqrt, Par_Op_Type::exp,
		Par_Op_Type::sin, Par_Op_Type::cos, Par_Op_Type::tan, Par_Op_Type::abs,
		Par_Op_Type::arg, Par_Op_Type::ln, Par_Op_Type::re, Par_Op_Type::im };

	constexpr std::string_view name_of(Par_Op_Type op_type)
	{
		switch (op_type) {
		case Par_Op_Type::log10:	return { "log10(" };
		case Par_Op_Type::asinh:	return { "asinh(" };
		case Par_Op_Type::acosh:	return { "acosh(" };
		case Par_Op_Type::atanh:	return { "atanh(" };
		case Par_Op_Type::asin:		return { "asin(" };
		case Par_Op_Type::acos:		return { "acos(" };
		case Par_Op_Type::atan:		return { "atan(" };
		case Par_Op_Type::sinh:		return { "sinh(" };
		case Par_Op_Type::cosh:		return { "cosh(" };
		case Par_Op_Type::tanh:		return { "tanh(" };
		case Par_Op_Type::sqrt:		return { "sqrt(" };
		case Par_Op_Type::exp:		return { "exp(" };
		case Par_Op_Type::sin:		return { "sin(" };
		case Par_Op_Type::cos:		return { "cos(" };
		case Par_Op_Type::tan:		return { "tan(" };
		case Par_Op_Type::abs:		return { "abs(" };
		case Par_Op_Type::arg:		return { "arg(" };
		case Par_Op_Type::ln:		return { "ln(" };
		case Par_Op_Type::re:		return { "re(" };
		case Par_Op_Type::im:		return { "im(" };
		}
		assert(false);
		return {};
	}

	constexpr std::complex<double> apply_parenthesis_operator(std::complex<double> argument, Par_Op_Type op_type)
	{
		switch (op_type) {
		case Par_Op_Type::log10:	return std::log10(argument);
		case Par_Op_Type::asinh:	return std::asinh(argument);
		case Par_Op_Type::acosh:	return std::acosh(argument);
		case Par_Op_Type::atanh:	return std::atanh(argument);
		case Par_Op_Type::asin:		return std::asin(argument);
		case Par_Op_Type::acos:		return std::acos(argument);
		case Par_Op_Type::atan:		return std::atan(argument);
		case Par_Op_Type::sinh:		return std::sinh(argument);
		case Par_Op_Type::cosh:		return std::cosh(argument);
		case Par_Op_Type::tanh:		return std::tanh(argument);
		case Par_Op_Type::sqrt:		return std::sqrt(argument);
		case Par_Op_Type::exp:		return std::exp(argument);
		case Par_Op_Type::sin:		return std::sin(argument);
		case Par_Op_Type::cos:		return std::cos(argument);
		case Par_Op_Type::tan:		return std::tan(argument);
		case Par_Op_Type::abs:		return std::abs(argument);
		case Par_Op_Type::arg:		return std::arg(argument);
		case Par_Op_Type::ln:		return std::log(argument);
		case Par_Op_Type::re:		return std::real(argument);
		case Par_Op_Type::im:		return std::imag(argument);
		}
		assert(false);
		return {};
	}

	namespace order {
		//used to sort terms by uniqueness
		//less unique is greater (to sort terms with < and end up with most unique in front)
		std::strong_ordering compare_uniqueness(Type fst, Type snd);

		//used to determine if parentheses are needed when displaying term with fst and snd only one layer apart
		//higher operator precedence is greater
		std::strong_ordering compare_precedence(Type fst, Type snd);

	} //namespace order

	namespace print {

	} //namespace print

	namespace construction {

	} //namespace construction

	namespace matching {

	} //namespace matching

} //namespace bmath::intern