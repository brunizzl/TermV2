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
		asinh,	//param_1 := argument
		acosh,	//param_1 := argument
		atanh,	//param_1 := argument
		asin,	//param_1 := argument
		acos,	//param_1 := argument
		atan,	//param_1 := argument
		sinh,	//param_1 := argument
		cosh,	//param_1 := argument
		tanh,	//param_1 := argument
		sqrt,	//param_1 := argument
		pow,    //param_1 := base      param_2 := expo    
		log,	//param_1 := base      param_2 := argument
		exp,	//param_1 := argument
		sin,	//param_1 := argument
		cos,	//param_1 := argument
		tan,	//param_1 := argument
		abs,	//param_1 := argument
		arg,	//param_1 := argument
		ln,		//param_1 := argument
		re,		//param_1 := argument
		im,		//param_1 := argument
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
		enum class ParamCount :unsigned char { one, two, more } param_count : 4;
		enum class NameSize :unsigned char { small, longer } name_size : 4;

		char short_name[3];	//can be used as short_name[7], if short_name_extension is active member
		union
		{
			char short_name_extension[4];
			std::uint32_t long_name_index;	//points to TermString128 containing name (if active)
		};
		union
		{
			std::uint32_t long_param_colony_index; //points to TypedIdxColony containing ALL parameters (if active)
			TypedIdx short_parameters[2];
		};
	};

	struct Variable :TermString128
	{
	};

	struct Complex :std::complex<double>
	{};

	union TypesUnion
	{
		KnownFunction known_function;
		UnknownFunction unknown_function;
		Variable variable;
		Complex complex;
		TermString128 string;	//Variable and UnknownFunction may allocate additional string nodes
		TypedIdxColony index_slc; //representing Sum, Product or UnknownFunction's extra parameters 

		TypesUnion() :complex() {}

		TypesUnion(const KnownFunction&   val) :known_function(val)   {}
		TypesUnion(const UnknownFunction& val) :unknown_function(val) {}
		TypesUnion(const Variable&        val) :variable(val)         {}
		TypesUnion(const Complex&         val) :complex(val)          {}
		TypesUnion(const TermString128&   val) :string(val)           {} 
		TypesUnion(const TypedIdxColony&  val) :index_slc(val)        {}
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);
	using ArithmeticStore = TermStore<TypesUnion>;

	//everything using the TermSLC needs to have a way to get the right union member from the union
	struct ToString   { using Result = TermString128;  static Result& apply(TypesUnion& val) { return val.string;    } };
	struct ToIndexSLC { using Result = TypedIdxColony; static Result& apply(TypesUnion& val) { return val.index_slc; } };
	struct ToConstString   { using Result = const TermString128;  static Result& apply(const TypesUnion& val) { return val.string;    } };
	struct ToConstIndexSLC { using Result = const TypedIdxColony; static Result& apply(const TypesUnion& val) { return val.index_slc; } };
	using ToSum     = ToIndexSLC;
	using ToProduct = ToIndexSLC;
	using ToConstSum     = ToConstIndexSLC;
	using ToConstProduct = ToConstIndexSLC;

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



