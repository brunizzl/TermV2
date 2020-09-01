#pragma once	//NOLINT

#include <type_traits>
#include <complex>
#include <string>
#include <compare>
#include <functional>
#include <cassert>
#include <vector>

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
		function_1,
		logarithm,
		power,		           
		product,	           
		sum,		           
		variadic_comprehension,	//may only occur in pattern
		value,		           
		variable,	
		COUNT
	};

	//the classic object oriented way of creating the Base_Term type would mean most functions in "internalFunctions" would not be in that file, 
	//but virtual members of Base_Term. i however, dislike to spread around logic in the derived classes like that. 
	//the need to always create a new member function for any new functionality, where as the types are clear from the beginning and are never expanded 
	//(or infrequent enough to tolerate the hussle of updating all functions implementing behavior for term) is another reason for going this way.

	//because all i need to know from the base class is what type is actually held by it, this type is stored as the member variable of same name.
	//the (probably less janky) other way would be to make Base_Term a virtual class. this would also benefit the destructor greatly, 
	//as it enables the usual way to handle memory management of each derived class in this derived class only.
	//the second virtual function would be the one returning the real term type (something like "virtual Type get_type() const").
	//the size of the base is not different in both cases: eighter a vtable or the type member is held.
	//side note: this is a fun projekt and as such only relaxed savety concerns may apply.

	//this is a template to make the regular term and the pattern term two different types.
	//it is hoped, that this measure removes any ambiguity surrounding terms beeing part of a pattern and therefore better enables programming while beeing sleepy.
	template <Modifier modifier>
	struct Base_Term
	{
	protected:
		Base_Term(Type type_) : type(type_) {}

	public:
		const Type type;
		~Base_Term();
	};
	using Regular_Term = Base_Term<Modifier::regular>;
	using Pattern_Term = Base_Term<Modifier::pattern>;
	static_assert(!std::is_same<Regular_Term, Pattern_Term>::value);


	template<Modifier modifier>
	struct Sum final : Base_Term<modifier>
	{
		std::vector<Base_Term<modifier>*> summands;

		Sum(std::vector<Base_Term<modifier>*>&& summands_)
			:Base_Term<modifier>(Type::sum), summands(summands_) {}

		void destructor() { for (auto summand : this->summands) delete summand; }
	};
	using Regular_Sum = Sum<Modifier::regular>;
	using Pattern_Sum = Sum<Modifier::pattern>;


	template<Modifier modifier>
	struct Product final : Base_Term<modifier>
	{
		std::vector<Base_Term<modifier>*> factors;

		Product(std::vector<Base_Term<modifier>*>&& factors_)
			:Base_Term<modifier>(Type::product), factors(factors_) {}

		void destructor() { for (auto factor : this->factors) delete factor; }
	};
	using Regular_Product = Product<Modifier::regular>;
	using Pattern_Product = Product<Modifier::pattern>;


	template<Modifier modifier>
	struct Power final : Base_Term<modifier>
	{
		Base_Term<modifier>* base;
		Base_Term<modifier>* exponent;

		Power(Base_Term<modifier>* base_, Base_Term<modifier>* exponent_) 
			:Base_Term<modifier>(Type::power), base(base_), exponent(exponent_) {}

		void destructor() { delete this->base; delete this->exponent; }
	};
	using Regular_Power = Power<Modifier::regular>;
	using Pattern_Power = Power<Modifier::pattern>;


	template<Modifier modifier>
	struct Logarithm final : Base_Term<modifier>
	{
		Base_Term<modifier>* base;
		Base_Term<modifier>* argument;

		Logarithm(Base_Term<modifier>* base_, Base_Term<modifier>* argument_) 
			:Base_Term<modifier>(Type::logarithm), base(base_), argument(argument_) {}

		void destructor() { delete this->base; delete this->argument; }
	};
	using Regular_Log = Logarithm<Modifier::regular>;
	using Pattern_Log = Logarithm<Modifier::pattern>;


	//types names are sorted by length (used to be required, as the type_subterm() function searched for par_op at not only the beginning of the name string)
	//(comments are corresponding std::complex functions)
	enum class F1_Type
	{
		log10,			//log10()
		asinh,			//asinh()
		acosh,			//acosh()
		atanh,			//atanh()
		asin,			//asin()
		acos,			//acos()
		atan,			//atan()
		sinh,			//sinh()
		cosh,			//cosh()
		tanh,			//tanh()
		sqrt,			//sqrt()
		exp,			//exp()
		sin,			//sin()
		cos,			//cos()
		tan,			//tan()
		abs,			//abs()
		arg,			//arg()
		ln,				//log()
		re,				//real()
		im,				//imag()
	};


	template<Modifier modifier>
	struct Function_1 final : Base_Term<modifier>
	{
		F1_Type op_type;
		Base_Term<modifier>* argument;

		Function_1(F1_Type op_type_, Base_Term<modifier>* argument_) 
			:Base_Term<modifier>(Type::function_1), op_type(op_type_), argument(argument_) {}

		void destructor() { delete this->argument; }
	};
	using Regular_F1 = Function_1<Modifier::regular>;
	using Pattern_F1 = Function_1<Modifier::pattern>;


	template<Modifier modifier>
	struct Value final : Base_Term<modifier>
	{
		std::complex<double> number;

		Value(std::complex<double> number_)
			:Base_Term<modifier>(Type::value), number(number_) {}

		void destructor() {}
	};
	using Regular_Value = Value<Modifier::regular>;
	using Pattern_Value = Value<Modifier::pattern>;


	struct Regular_Variable final : Regular_Term
	{
		std::string name;

		Regular_Variable(std::string_view name_) 
			:Regular_Term(Type::variable), name(name_) {}

		void destructor() {}
	};

	//Pattern_Term is meant to be compared against Regular_Term. A Pattern_Variable can stand for any arbitrary term and its subterms, but it -
	//always represents this same thing for the (possibly) multiple times it occurs in a Pattern_Term. This behavior breakes the tree structure of a pattern,
	//as one all parts of a pattern holding Pattern_Variable "x" need to point to the same instance.
	struct Pattern_Variable final : Pattern_Term
	{
		std::string name;
		Regular_Term* matched_term;

		Pattern_Variable(std::string_view name_) 
			:Pattern_Term(Type::variable), name(name_), matched_term(nullptr) {}

		void destructor() {} //matched term is not owned by this.
	};

	struct Variadic_Comprehension final : Pattern_Term
	{
		void destructor() {}
	};

	template<Modifier modifier>
	inline Base_Term<modifier>::~Base_Term()
	{
		switch (this->type) {
		case Type::sum:        static_cast<Sum<modifier>*       >(this)->destructor(); break;
		case Type::product:    static_cast<Product<modifier>*   >(this)->destructor(); break;
		case Type::power:      static_cast<Power<modifier>*     >(this)->destructor(); break;
		case Type::logarithm:  static_cast<Logarithm<modifier>* >(this)->destructor(); break;
		case Type::function_1: static_cast<Function_1<modifier>*>(this)->destructor(); break;
		case Type::value:      static_cast<Value<modifier>*     >(this)->destructor(); break;
		case Type::variable: 
			if constexpr (modifier == Modifier::regular)
				static_cast<Regular_Value*>(this)->destructor(); break;
			if constexpr (modifier == Modifier::pattern)
				static_cast<Pattern_Value*>(this)->destructor(); break;
		case Type::variadic_comprehension: 
			if constexpr (modifier == Modifier::regular)
				assert(false);
			if constexpr (modifier == Modifier::pattern)
				static_cast<Variadic_Comprehension*>(this)->destructor(); break;

		default: assert(false);	//not hitting the actual type means not deleting the actual type and creating a memory leak.
		}
	}

} //namespace bmath::intern