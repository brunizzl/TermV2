#pragma once

#include <complex>

#include "typedIndex.hpp"
#include "termStore.hpp"
#include "termColony.hpp"

namespace bmath::intern::arithmetic {

	enum class Type {
		sum,
		product,
		known_function,
		unknown_function,
		variable,
		complex,
		COUNT	//has to be last element
	};

	using TypedIdx = BasicTypedIdx<Type, Type::COUNT, std::uint32_t>;
	using TypedIdxColony = TermSLC<std::uint32_t, TypedIdx, 3>;


	struct Sum : TypedIdxColony
	{
		using TypedIdxColony::TypedIdxColony;
	};

	struct Product : TypedIdxColony
	{
		using TypedIdxColony::TypedIdxColony;
	};

	enum class FunctionType : std::uint32_t
	{
		asinh,	//argument := param_1
		acosh,	//argument := param_1
		atanh,	//argument := param_1
		asin,	//argument := param_1
		acos,	//argument := param_1
		atan,	//argument := param_1
		sinh,	//argument := param_1
		cosh,	//argument := param_1
		tanh,	//argument := param_1
		sqrt,	//argument := param_1
		pow,    //base     := param_1  expo     := param_2
		log,	//base     := param_1  argument := param_2
		exp,	//argument := param_1
		sin,	//argument := param_1
		cos,	//argument := param_1
		tan,	//argument := param_1
		abs,	//argument := param_1
		arg,	//argument := param_1
		ln,		//argument := param_1
		re,		//argument := param_1
		im,		//argument := param_1
		COUNT	//has to be last element
	};

	struct KnownFunction
	{
		FunctionType type;

		//if any buildin funtion exeeds a parameter count of 3, a more involved structure needs to replace this.
		TypedIdx param_1;	
		TypedIdx param_2;
		TypedIdx param_3;
	};

	struct UnknownFunction
	{
		enum class ParamCount :char { one, two, more } param_count : 4;
		enum class NameSize :char { small, longer } name_size : 4;

		char short_name[3];	//can be used as short_name[7], if short_name_extension is active member
		union
		{
			char short_name_extension[4];
			std::uint32_t long_name_index;	//points to TermString128 containing name (if active)
		};
		union
		{
			std::uint32_t long_param_colony_index; //points to UnknownFunction_Parameters containing ALL parameters (if active)
			TypedIdx short_parameters[2];
		};
	};

	struct UnknownFunction_Parameters :TypedIdxColony
	{
		using TypedIdxColony::TypedIdxColony;
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
		UnknownFunction unknown_function;
		Variable variable;
		Complex complex;
		TermString128 string;	//Variable and UnknownFunction may allocate additional string nodes
		TypedIdxColony typedidx_col;	//UnknownFunction may allocate additional colony nodes. 

		TypesUnion() :complex() {}

		TypesUnion(const Sum &            val) :sum(val)              {}
		TypesUnion(const Product&         val) :product(val)          {}
		TypesUnion(const KnownFunction&   val) :known_function(val)   {}
		TypesUnion(const UnknownFunction& val) :unknown_function(val) {}
		TypesUnion(const Variable&        val) :variable(val)         {}
		TypesUnion(const Complex&         val) :complex(val)          {}
		TypesUnion(const TermString128&   val) :string(val)           {} 
		TypesUnion(const TypedIdxColony&  val) :typedidx_col(val)     {}
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);
	using ArithmeticStore = TermStore<TypesUnion>;

	//everything using the TermSLC needs to have a way to get the right union member from the union
	struct ToSum     { using Result = Sum;	         static Result& apply(TypesUnion& val) { return val.sum;     } };
	struct ToProduct { using Result = Product;       static Result& apply(TypesUnion& val) { return val.product; } };
	struct ToString  { using Result = TermString128; static Result& apply(TypesUnion& val) { return val.string;  } };
	struct ToConstSum     { using Result = const Sum;           static const Result& apply(const TypesUnion& val) { return val.sum;     } };
	struct ToConstProduct { using Result = const Product;       static const Result& apply(const TypesUnion& val) { return val.product; } };
	struct ToConstString  { using Result = const TermString128;	static const Result& apply(const TypesUnion& val) { return val.string;  } };

	//evaluates tree if possible, throws if variables of unknown value /unknown_functions are present
	std::complex<double> eval(const ArithmeticStore& store, TypedIdx ref);

	void to_string(const ArithmeticStore& store, TypedIdx ref, std::string& str, const int parent_precedence = -1);

	void to_debug_view_(const ArithmeticStore& store, TypedIdx ref, std::vector<std::string>& content);

}	//namespace bmath::intern::arithmetic

namespace bmath {

	struct ArithmeticTerm
	{
		intern::arithmetic::TypedIdx head;
		intern::arithmetic::ArithmeticStore values;
		std::string to_debug_view() const;
	};	//class ArithmeticTerm

}	//namespace bmath



