#pragma once	//NOLINT

#include <type_traits>
#include <complex>
#include <string>
#include <compare>
#include <functional>

namespace bmath::intern {
	//basic structure: 
	//there are two Interface types: Pattern_Term and Regular_Term, both template instantiations of Base_Term.
	//the classes implementing these interfaces also implement both for the most time and represent algebraic operations, values or variables.

	//currently there is not planned to add any functionality beyond type differentiation of terms for this
	enum class Modifier
	{
		regular,
		pattern,
	};

	//specifies actual type of any Base_Term 
	enum class Type
	{
		par_operator,          
		logarithm,           
		power,		           
		product,	           
		sum,		           
		variadic_comprehension,	//may only occur in pattern
		value,		           
		variable,	           
	};

	//this is a template to make the regular term and the pattern term two different types.
	//it is hoped, that this measure removes any ambiguity surrounding terms beeing part of a pattern
	template <Modifier modifier>
	class Base_Term
	{
	public:

		virtual ~Base_Term() {}	//tree is cleaned up in derived classes -> nothing to do here

		//appends this to str. caller_operator_precedence tells callee, whether to put parentheses around string or not
		virtual void to_str(std::string& str, Type parent_type) const = 0;

		//returns actual type if only pointer to Base_Term is held
		virtual /*constexpr*/ Type get_type() const = 0;

		//compares two terms for their uniqueness (see internalFunctions order::compare_uniqueness()) and returnes less if this is more unique
		//order class std::partial_ordering is nessessary due to double beeing in that class.
		virtual /*constexpr*/ std::partial_ordering lexicographical_compare(const Base_Term& snd) const = 0;

		//equality compare on the type and subterms of term
		virtual /*constexpr*/ bool equals(const Base_Term& snd) const = 0;

		//alternative for virtual functions if only some terms need to implement behavior.
		//for any operation for_each is first applied to all held subterms, then to itself. (e.g. product first calls for_each for all factors, then on itself)
		//function func takes this, this->get_type(), and a pointer to the location in the parent where this is held. that last parameter allows to substitute specific subterms.
		//as no term knows anything about its parent, this_storage_key needs to also be given by the caller of for_each()
		virtual /*constexpr*/ void for_each(std::function<void(Base_Term* this_ptr, Type this_type, Base_Term** this_storage_key)> func, Base_Term** this_storage_key) = 0;

	};

	using Regular_Term = Base_Term<Modifier::regular>;
	using Pattern_Term = Base_Term<Modifier::pattern>;
	static_assert(!std::is_same<Regular_Term, Pattern_Term>::value);	//the whole reason to use templates with Modifier is to create different types.

} //namespace bmath::intern