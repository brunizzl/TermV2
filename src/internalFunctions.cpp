#include "internalFunctions.hpp"

#include <cassert>

namespace bmath::intern {

	namespace order {

		std::strong_ordering compare_uniqueness(Type fst, Type snd)
		{
			//note: although often more types share their level of uniqueness, to guarantee two sorted terms with same subterms to be in the same order,
			//there are no two different types with same uniqueness_rank.
			const auto uniqueness_rank = [](Type type) -> std::size_t {
				switch (type) {
				case Type::par_operator:			   return 0;  //most unique (likely different op_type and op_type is always given)
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
				case Type::par_operator:			   return 1;	//brings own parentheses -> low precedence
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