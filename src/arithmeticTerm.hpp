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
		integer,
		COUNT	//has to be last element
	};

	using TypedRef = IndexTypePair<Type, Type::COUNT, std::uint32_t>;
	using TypedRefColony = TermSLC<std::uint32_t, 0, TypedRef, 0, 3>;



	struct Sum
	{
		TypedRefColony summands;
	};

	struct Product
	{
		TypedRefColony factors;
	};

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

		union
		{
			struct
			{
				//if == ParamCount::one or == ParamCount::two, short_parameters are used, long_param_colony_index otherwise
				//if == NameSize::small, short_name is used, long_name_index otherwise
				ParamCount param_count : 4;
				NameSize name_size : 4;
				std::uint32_t long_name_index;	//points to TermString128 containing name (if active)
			};

			struct
			{
				ParamCount param_count_ : 4; //also here to guarantee existence independent of active member
				NameSize name_size_ : 4;	 //also here to guarantee existence independent of active member
				char short_name[7];
			};
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

	struct Variable
	{
		TermString128 name;
	};

	struct Complex
	{
		std::complex<double> val;
	};

	struct Integer
	{
		//if value fits into std::int32_t, only parts.values[0] is used, else also parts.values[1] ...
		TermSLC<std::uint32_t, 0, std::int32_t, 0, 3> parts;
	};

	union TypesUnion
	{
		Sum sum;
		Product product;
		KnownFunction known_function;
		UnknownFunction	unknown_function;
		Power power;
		Variable variable;
		Complex complex;
		Integer integer;

		TypesUnion() :integer() {}

		TypesUnion(const Sum &            val) :sum(val)              {}
		TypesUnion(const Product&         val) :product(val)          {}
		TypesUnion(const KnownFunction&   val) :known_function(val)   {}
		TypesUnion(const UnknownFunction& val) :unknown_function(val) {}
		TypesUnion(const Power&           val) :power(val)            {}
		TypesUnion(const Variable&        val) :variable(val)         {}
		TypesUnion(const Complex&         val) :complex(val)          {}
		TypesUnion(const Integer&         val) :integer(val)          {} 
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);

	Sum&             get_sum             (TypesUnion& val) { return val.sum; }
	Product&         get_product         (TypesUnion& val) { return val.product; }
	KnownFunction&   get_known_function  (TypesUnion& val) { return val.known_function; }
	UnknownFunction& get_unknown_function(TypesUnion& val) { return val.unknown_function; }
	Power&           get_power           (TypesUnion& val) { return val.power; }
	Variable&        get_variable        (TypesUnion& val) { return val.variable; }
	Complex&         get_Complex         (TypesUnion& val) { return val.complex; }
	Integer&         get_integer         (TypesUnion& val) { return val.integer; }

	const Sum&             get_sum             (const TypesUnion& val) { return val.sum; }
	const Product&         get_product         (const TypesUnion& val) { return val.product; }
	const KnownFunction&   get_known_function  (const TypesUnion& val) { return val.known_function; }
	const UnknownFunction& get_unknown_function(const TypesUnion& val) { return val.unknown_function; }
	const Power&           get_power           (const TypesUnion& val) { return val.power; }
	const Variable&        get_variable        (const TypesUnion& val) { return val.variable; }
	const Complex&         get_Complex         (const TypesUnion& val) { return val.complex; }
	const Integer&         get_integer         (const TypesUnion& val) { return val.integer; }



}	//namespace bmath::intern::arithmetic

namespace bmath {

	class ArithmeticTerm
	{
		intern::arithmetic::TypedRef head;
		intern::TermStore<intern::arithmetic::TypesUnion> values;
	};	//class ArithmeticTerm

}	//namespace bmath



