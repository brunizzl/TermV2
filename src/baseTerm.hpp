#pragma once	//NOLINT

#include <type_traits>
#include <complex>

namespace bmath::intern {

	enum class Base_Type
	{
		regular,
		pattern,
	};

	//specifies actual type of any Base_Term 
	//the here listed types are ordered by their uniqueness (more unique means smaller)
	//this order is used to sort, as it makes it easyer to match patterns
	enum class Derived_Type
	{
		par_operator,  //most unique (likely different op_type and op_type is always given)
		generic_log,   //second most unique (base is always base, argument always argument)
		power,		   //also second most unique (base is always base, exponent always exponent)
		product,	   //third most unique (operands can vary in the positioning relative to each other)
		sum,		   //also third most unique
		value,		   //fourth most unique (value can take a practically infinite amount of states)
		variable,	   //for regular: as unique as value, for pattern: least unique, as it may represent any other type
	};

	//this is a template to make The regular term and the pattern term two different types.
	//it is hoped, that this measure removes any ambiguity surrounding term beeing parts of a pattern
	template <Base_Type base_type>
	class Base_Term
	{
	public:

		virtual ~Base_Term() {}	//tree is cleaned up in derived classes -> nothing to do here

		//appends this to str. caller_operator_precedence tells callee, whether to put parentheses around string or not
		virtual void to_str(std::string& str, int caller_operator_precedence) const = 0;

		//returns actual type if only pointer to Base_Term is held
		virtual /*constexpr*/ Derived_Type get_type() const = 0;

	};

	using Regular_Term = Base_Term<Base_Type::regular>;
	using Pattern_Term = Base_Term<Base_Type::pattern>;
	static_assert(!std::is_same<Regular_Term, Pattern_Term>::value);	//the whole reason to use templates with Base_Type is to create different types.

} //namespace bmath::intern