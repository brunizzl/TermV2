#pragma once

#include <complex>
#include <variant>

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
	using TypedRefColony = TermSLC<std::uint32_t, TypedRef, 3>;



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
		TermSLC<std::uint32_t, std::int32_t, 3> parts;
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
		TermString128 string;	//Variable and UnknownFunction may allocate additional string nodes

		TypesUnion() :integer() {}

		TypesUnion(const Sum &            val) :sum(val)              {}
		TypesUnion(const Product&         val) :product(val)          {}
		TypesUnion(const KnownFunction&   val) :known_function(val)   {}
		TypesUnion(const UnknownFunction& val) :unknown_function(val) {}
		TypesUnion(const Power&           val) :power(val)            {}
		TypesUnion(const Variable&        val) :variable(val)         {}
		TypesUnion(const Complex&         val) :complex(val)          {}
		TypesUnion(const Integer&         val) :integer(val)          {} 
		TypesUnion(const TermString128&   val) :string(val)           {} 
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);

	Sum&             get_sum             (TypesUnion& val) { return val.sum; }
	Product&         get_product         (TypesUnion& val) { return val.product; }
	KnownFunction&   get_known_function  (TypesUnion& val) { return val.known_function; }
	UnknownFunction& get_unknown_function(TypesUnion& val) { return val.unknown_function; }
	Power&           get_power           (TypesUnion& val) { return val.power; }
	Variable&        get_variable        (TypesUnion& val) { return val.variable; }
	Complex&         get_complex         (TypesUnion& val) { return val.complex; }
	Integer&         get_integer         (TypesUnion& val) { return val.integer; }

	const Sum&             get_sum             (const TypesUnion& val) { return val.sum; }
	const Product&         get_product         (const TypesUnion& val) { return val.product; }
	const KnownFunction&   get_known_function  (const TypesUnion& val) { return val.known_function; }
	const UnknownFunction& get_unknown_function(const TypesUnion& val) { return val.unknown_function; }
	const Power&           get_power           (const TypesUnion& val) { return val.power; }
	const Variable&        get_variable        (const TypesUnion& val) { return val.variable; }
	const Complex&         get_complex         (const TypesUnion& val) { return val.complex; }
	const Integer&         get_integer         (const TypesUnion& val) { return val.integer; }


	//using VarTerm = std::variant<Sum, Product, KnownFunction, UnknownFunction, Power, Variable, Complex, Integer>;

	template<typename SumLambda, typename ProductLambda, typename KnownFunctionLambda, typename UnknownFunctionLambda, 
		typename PowerLambda, typename VariableLambda, typename ComplexLambda, typename IntegerLambda>
		auto visit(TermStore<TypesUnion>& store, TypedRef ref, SumLambda& sum_lambda, ProductLambda& product_lambda,
			KnownFunctionLambda& known_function_lambda, UnknownFunctionLambda& unknown_function_lambda, PowerLambda& power_lambda,
			VariableLambda& variable_lambda, ComplexLambda& complex_lambda, IntegerLambda& integer_lambda)
	{
		const std::size_t index = ref.get_index();
		switch (ref.get_type()) {
		case Type::sum: return sum_lambda(store, get_sum(store.at(index)));
		case Type::product: return product_lambda(store, get_product(store.at(index)));
		case Type::known_function: return known_function_lambda(store, get_known_function(store.at(index)));
		case Type::unknown_function: return unknown_function_lambda(store, get_unknown_function(store.at(index)));
		case Type::power: return power_lambda(store, get_power(store.at(index)));
		case Type::variable: return variable_lambda(store, get_variable(store.at(index)));
		case Type::complex: return complex_lambda(store, get_complex(store.at(index)));
		case Type::integer: return integer_lambda(store, get_integer(store.at(index)));
		}
		assert(false);	//if this assert hits, the switch needs more cases.
		return integer_lambda(store, get_integer(store.at(index)));
	}

	double eval(TermStore<TypesUnion>& store, TypedRef ref)
	{
		const auto unimplemented = [](auto& x, auto& y) { return 0.0; };
		return visit(store, ref,
			[](auto& store, Sum& sum) {
				double value = 0;
				for (auto i = 0; i < 3; i++) {
					if (sum.summands.values[i] != TypedRefColony::null_value) {
						value += eval(store, sum.summands.values[i]);
					}
				}
				return value;
			},
			[](auto& store, Product& product) {
				double value = 1;
				for (auto i = 0; i < 3; i++) {
					if (product.factors.values[i] != TypedRefColony::null_value) {
						value *= eval(store, product.factors.values[i]);
					}
				}
				return value;
			},
			unimplemented,
			unimplemented,
			[](auto& store, Power& power) {
				double base = eval(store, power.base);
				double expo = eval(store, power.expo);
				return std::pow(base, expo);
			},
			unimplemented,
			[](auto& store, Complex& complex) {
				return complex.val.real();
			},
			[](auto& store, Integer& integer) {
				return static_cast<double>(integer.parts.values[0]);
			}
			);
	}

}	//namespace bmath::intern::arithmetic

namespace bmath {

	struct ArithmeticTerm
	{
		intern::arithmetic::TypedRef head;
		intern::TermStore<intern::arithmetic::TypesUnion> values;
	};	//class ArithmeticTerm

}	//namespace bmath



