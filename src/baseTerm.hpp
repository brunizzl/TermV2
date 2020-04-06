#pragma once	//NOLINT

#include <type_traits>
#include <complex>
#include <string>
#include <compare>

namespace bmath::intern {

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

		virtual /*constexpr*/ std::partial_ordering lexicographical_compare(const Base_Term& snd) const = 0;

		virtual /*constexpr*/ bool equals(const Base_Term& snd) const = 0;

	};

	using Regular_Term = Base_Term<Modifier::regular>;
	using Pattern_Term = Base_Term<Modifier::pattern>;
	static_assert(!std::is_same<Regular_Term, Pattern_Term>::value);	//the whole reason to use templates with Modifier is to create different types.

} //namespace bmath::intern