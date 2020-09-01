#include "internalFunctions.hpp"
#include "visit.hpp"

#include <cassert>

namespace bmath::intern {

	//array with instance of every F1_Type to iterate over them
	static constexpr F1_Type all_par_op_types[] = { F1_Type::log10, F1_Type::asinh, F1_Type::acosh, F1_Type::atanh,
		F1_Type::asin, F1_Type::acos, F1_Type::atan, F1_Type::sinh,
		F1_Type::cosh, F1_Type::tanh, F1_Type::sqrt, F1_Type::exp,
		F1_Type::sin, F1_Type::cos, F1_Type::tan, F1_Type::abs,
		F1_Type::arg, F1_Type::ln, F1_Type::re, F1_Type::im };

	constexpr std::string_view name_of(F1_Type op_type)
	{
		switch (op_type) {
		case F1_Type::log10:	return { "log10(" };
		case F1_Type::asinh:	return { "asinh(" };
		case F1_Type::acosh:	return { "acosh(" };
		case F1_Type::atanh:	return { "atanh(" };
		case F1_Type::asin:		return { "asin(" };
		case F1_Type::acos:		return { "acos(" };
		case F1_Type::atan:		return { "atan(" };
		case F1_Type::sinh:		return { "sinh(" };
		case F1_Type::cosh:		return { "cosh(" };
		case F1_Type::tanh:		return { "tanh(" };
		case F1_Type::sqrt:		return { "sqrt(" };
		case F1_Type::exp:		return { "exp(" };
		case F1_Type::sin:		return { "sin(" };
		case F1_Type::cos:		return { "cos(" };
		case F1_Type::tan:		return { "tan(" };
		case F1_Type::abs:		return { "abs(" };
		case F1_Type::arg:		return { "arg(" };
		case F1_Type::ln:		return { "ln(" };
		case F1_Type::re:		return { "re(" };
		case F1_Type::im:		return { "im(" };
		}
		assert(false);
		return {};
	}

	constexpr std::complex<double> apply_function_operator(std::complex<double> argument, F1_Type op_type)
	{
		switch (op_type) {
		case F1_Type::log10:	return std::log10(argument);
		case F1_Type::asinh:	return std::asinh(argument);
		case F1_Type::acosh:	return std::acosh(argument);
		case F1_Type::atanh:	return std::atanh(argument);
		case F1_Type::asin:		return std::asin(argument);
		case F1_Type::acos:		return std::acos(argument);
		case F1_Type::atan:		return std::atan(argument);
		case F1_Type::sinh:		return std::sinh(argument);
		case F1_Type::cosh:		return std::cosh(argument);
		case F1_Type::tanh:		return std::tanh(argument);
		case F1_Type::sqrt:		return std::sqrt(argument);
		case F1_Type::exp:		return std::exp(argument);
		case F1_Type::sin:		return std::sin(argument);
		case F1_Type::cos:		return std::cos(argument);
		case F1_Type::tan:		return std::tan(argument);
		case F1_Type::abs:		return std::abs(argument);
		case F1_Type::arg:		return std::arg(argument);
		case F1_Type::ln:		return std::log(argument);
		case F1_Type::re:		return std::real(argument);
		case F1_Type::im:		return std::imag(argument);
		}
		assert(false);
		return {};
	}

	std::complex<double> evaluate(const Regular_Term* term)
	{
		return regular_visit(term,
			[](const Regular_F1* function) { return apply_function_operator(evaluate(function->argument), function->op_type); },
			[](const Regular_Log* log) {
				const std::complex<double> base_val = evaluate(log->base);
				const std::complex<double> arg_val = evaluate(log->argument);
				return std::log(arg_val) / std::log(base_val);
			},
			[](const Regular_Power* power) {return std::pow(evaluate(power->base), evaluate(power->exponent)); },
			[](const Regular_Product* product) {
				std::complex<double> result = 1;
				for (auto& factor : product->factors) {
					result *= evaluate(factor);
				}
				return result;
			},
			[](const Regular_Sum* sum) {
				std::complex<double> result = 0;
				for (auto& summand : sum->summands) {
					result += evaluate(summand);
				}
				return result;
			},
			[](const Regular_Value* val) {return val->number; },
			[](const Regular_Variable* var) {assert(false); return std::complex<double>{0.0, 0.0}; });
	}

	namespace order {

		std::strong_ordering compare_uniqueness(Type fst, Type snd)
		{
			//note: although often more types share their level of uniqueness, to guarantee two sorted terms with same subterms to be in the same order,
			//there are no two different types with same uniqueness_rank.
			const auto uniqueness_rank = [](Type type) -> std::size_t {
				switch (type) {
				case Type::function_1:			   return 0;  //most unique (likely different op_type and op_type is always given)
				case Type::logarithm:			       return 1;  //second most unique (base is always base, argument always argument)
				case Type::power:					   return 2;  //also second most unique (base is always base, exponent always exponent)
				case Type::product:				       return 4;  //third most unique (operands can vary in the positioning relative to each other)
				case Type::sum:					       return 5;  //also third most unique
				case Type::variadic_comprehension:     return 6;  //not tecnically part of order, but stands for sum or product -> also third most unique
				case Type::value:					   return 8;  //fourth most unique (value can take a practically infinite amount of states)
				case Type::variable:				   return 9;  //for regular: as unique as value, for pattern: least unique, as it may represent any other type
				default:
					assert(false);	//all types need to be listed above
					return -1;
				}
			};
			return uniqueness_rank(fst) <=> uniqueness_rank(snd);
		}

		std::strong_ordering compare_precedence(Type fst, Type snd)
		{
			const auto operator_precedence = [](Type type) -> std::size_t {
				switch (type) {
				case Type::function_1:			   return 1;	//brings own parentheses -> low precedence
				case Type::logarithm:			       return 1;	//brings own parentheses -> low precedence
				case Type::variadic_comprehension:     return 1;	//brings own curly braces -> low precedence
				case Type::sum:					       return 3;
				case Type::value:					   return 3;	//complex number is sum itself -> same precedence as sum
				case Type::variable:				   return 3;
				case Type::product:				       return 4;
				case Type::power:					   return 5;
				default:
					assert(false);	//all types may be listed above
					return -1;
				}
			};
			return operator_precedence(fst) <=> operator_precedence(snd);
		}

	} //namespace order

	namespace print {

	} //namespace print

	namespace construction {

	} //namespace construction

	namespace operators {

	} //namespace operators

	namespace matching {

	} //namespace matching

} //namespace bmath::intern