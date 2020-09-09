#pragma once

#include <complex>

#include "termStore.hpp"
#include "termColony.hpp"

namespace bmath::intern::arithmetic {

	enum class Type {
		sum,
		product,
		known_function,
		unknown_function,
		power,
		variable,
		complex,
		COUNT	//has to be last element
	};

	using TypedRef = BasicTypedRef<Type, Type::COUNT, std::uint32_t>;
	using TypedRefColony = TermSLC<std::uint32_t, TypedRef, 3>;



	struct Sum : TypedRefColony
	{};

	struct Product : TypedRefColony
	{};

	enum class FunctionType : std::uint32_t
	{
		asinh,
		acosh,
		atanh,
		asin,
		acos,
		atan,
		sinh,
		cosh,
		tanh,
		sqrt,
		log,	//explicit base
		exp,
		sin,
		cos,
		tan,
		abs,
		arg,
		ln,	
		re,	
		im,	
		COUNT	//has to be last element
	};

	struct KnownFunction
	{
		FunctionType type;

		//if any buildin funtion exeeds a parameter count of 3, a more involved structure needs to replace this.
		TypedRef parameters[3];
	};

	struct UnknownFunction
	{
		enum class ParamCount :char { one, two, more };
		enum class NameSize :char { small, longer };

		ParamCount param_count : 4;
		NameSize name_size : 4;
		char short_name[3];	//can be used as short_name[7], if short_name_extension is active member
		union
		{
			char short_name_extension[4];
			std::uint32_t long_name_index;	//points to TermString128 containing name (if active)
		};
		union
		{
			std::uint32_t long_param_colony_index; //points to TypedRefColony containing all parameters (if active)
			TypedRef short_parameters[2];
		};
	};

	struct Power
	{
		TypedRef base;
		TypedRef expo;
	};

	struct Variable :TermString128
	{
	};

	struct Complex :std::complex<double>
	{};

	union TypesUnion
	{
		Sum sum;
		Product product;
		KnownFunction known_function;
		UnknownFunction	unknown_function;
		Power power;
		Variable variable;
		Complex complex;
		TermString128 string;	//Variable and UnknownFunction may allocate additional string nodes

		TypesUnion() :complex() {}

		TypesUnion(const Sum &            val) :sum(val)              {}
		TypesUnion(const Product&         val) :product(val)          {}
		TypesUnion(const KnownFunction&   val) :known_function(val)   {}
		TypesUnion(const UnknownFunction& val) :unknown_function(val) {}
		TypesUnion(const Power&           val) :power(val)            {}
		TypesUnion(const Variable&        val) :variable(val)         {}
		TypesUnion(const Complex&         val) :complex(val)          {}
		TypesUnion(const TermString128&   val) :string(val)           {} 
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);

	//everything using the TermSLC needs to have a way to get the right union member from the union
	struct ToSum     { static Sum&           apply(TypesUnion& val) { return val.sum;     } };
	struct ToProduct { static Product&       apply(TypesUnion& val) { return val.product; } };
	struct ToString  { static TermString128& apply(TypesUnion& val) { return val.string;  } };
	struct ToConstSum     { static const Sum&           apply(const TypesUnion& val) { return val.sum;     } };
	struct ToConstProduct { static const Product&       apply(const TypesUnion& val) { return val.product; } };
	struct ToConstString  { static const TermString128& apply(const TypesUnion& val) { return val.string;  } };

	//evaluates tree if possible else throws (if variables of unknown value /unknown_functions are present)
	std::complex<double> eval(const TermStore<TypesUnion>& store, TypedRef ref);

	//iff caller is outside tree, parent_precedence may be -1 
	void to_string(const TermStore<TypesUnion>& store, TypedRef ref, std::string& str, const int parent_precedence = -1);

}	//namespace bmath::intern::arithmetic

namespace bmath {

	struct ArithmeticTerm
	{
		intern::arithmetic::TypedRef head;
		intern::TermStore<intern::arithmetic::TypesUnion> values;
	};	//class ArithmeticTerm

}	//namespace bmath



