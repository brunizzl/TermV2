#include "internalFunctions.hpp"

#include <cassert>

namespace bmath::intern {

	template <Base_Type base_type>
	Derived_Type type_of(const Base_Term<base_type>& term) { return term.get_type(); }
	template <Base_Type base_type>
	Derived_Type type_of(const Base_Term<base_type>* term) { return term->get_type(); }

	std::strong_ordering compare_uniqueness(Derived_Type fst, Derived_Type snd)
	{
		//note: although often more types share their level of uniqueness, to guarantee two sorted terms with same subterms to be in the same order,
		//there are no two different types with same uniqueness_rank.
		const auto uniqueness_rank = [](Derived_Type type) -> std::size_t {
			switch (type) {
			case Derived_Type::par_operator:			   return 9;   //most unique (likely different op_type and op_type is always given)
			case Derived_Type::generic_log:			       return 8;   //second most unique (base is always base, argument always argument)
			case Derived_Type::power:					   return 7;   //also second most unique (base is always base, exponent always exponent)
			case Derived_Type::product:				       return 5;   //third most unique (operands can vary in the positioning relative to each other)
			case Derived_Type::sum:					       return 4;   //also third most unique
			case Derived_Type::variadic_comprehension:     return 3;   //not tecnically part of order, but stands for sum or product -> also third most unique
			case Derived_Type::value:					   return 1;   //fourth most unique (value can take a practically infinite amount of states)
			case Derived_Type::variable:				   return 0;   //for regular: as unique as value, for pattern: least unique, as it may represent any other type
			default:
				assert(false);	//all types need to be listed above
				return -1;
			}
		};
		return uniqueness_rank(fst) <=> uniqueness_rank(snd);
	}

	std::strong_ordering compare_precedence(Derived_Type fst, Derived_Type snd)
	{
		const auto operator_precedence = [](Derived_Type type) -> std::size_t {
			switch (type) {
			case Derived_Type::par_operator:			   return 1;	//brings own parentheses -> low precedence
			case Derived_Type::generic_log:			       return 1;	//brings own parentheses -> low precedence
			case Derived_Type::variadic_comprehension:     return 1;	//brings own curly braces -> low precedence
			case Derived_Type::sum:					       return 3;
			case Derived_Type::value:					   return 3;	//complex number is sum itself -> same precedence as sum
			case Derived_Type::variable:				   return 3;
			case Derived_Type::product:				       return 4;
			case Derived_Type::power:					   return 5;
			default:
				assert(false);	//all types may be listed above
				return -1;
			}
		};
		return operator_precedence(fst) <=> operator_precedence(snd);
	}

	namespace construction {

	} //namespace construction

	namespace operators {

	} //namespace operators

	namespace matching {

	} //namespace matching

} //namespace bmath::intern