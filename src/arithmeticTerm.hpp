#pragma once

#include <complex>
#include <span>

#include "typedIndex.hpp"
#include "termStore.hpp"
#include "termColony.hpp"
#include "parseTerm.hpp"

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

	using Sum            = TypedIdxColony;
	using Product        = TypedIdxColony;

	enum class FunctionType : std::uint32_t
	{
		asinh,	//params[0] := argument
		acosh,	//params[0] := argument
		atanh,	//params[0] := argument
		asin,	//params[0] := argument
		acos,	//params[0] := argument
		atan,	//params[0] := argument
		sinh,	//params[0] := argument
		cosh,	//params[0] := argument
		tanh,	//params[0] := argument
		sqrt,	//params[0] := argument
		pow,    //params[0] := base      params[1] := expo    
		log,	//params[0] := base      params[1] := argument
		exp,	//params[0] := argument
		sin,	//params[0] := argument
		cos,	//params[0] := argument
		tan,	//params[0] := argument
		abs,	//params[0] := argument
		arg,	//params[0] := argument
		ln,		//params[0] := argument
		re,		//params[0] := argument
		im,		//params[0] := argument
		UNKNOWN //has to be last element, doubles as count
	};

	struct KnownFunction
	{
		FunctionType type;

		//if any buildin funtion exeeds a parameter count of 3, a more involved structure needs to replace this.
		TypedIdx params[3];
	};

	struct UnknownFunction
	{
		static constexpr std::size_t short_name_max = 7;

		//param count can be used directly as number, if more is not active member
		enum class ParamCount :unsigned char { zero = 0u, one = 1u, two = 2u, more } param_count : 4;
		enum class NameSize :unsigned char { small, longer } name_size : 4;

		char short_name[3];	//can be used as short_name[7], if short_name_extension is active member
		union
		{
			char short_name_extension[4];
			std::uint32_t name_idx;	//points to TermString128 containing name (if active)
		};
		union
		{
			std::uint32_t params_idx; //points to TypedIdxColony containing the parameters (if active)
			TypedIdx short_params[2];
		};

		UnknownFunction() :param_count(ParamCount::zero), name_size(NameSize::small), 
			short_name{ '\0', '\0', '\0' }, short_name_extension{ '\0', '\0', '\0', '\0' }, 
			short_params{ TypedIdx(), TypedIdx() } {}
	};

	using  Variable = TermString128;
	using Complex = std::complex<double>;

	union TypesUnion
	{
		KnownFunction known_function;
		UnknownFunction unknown_function;
		Complex complex;
		TermString128 string;	//Variable and UnknownFunction may allocate additional string nodes
		TypedIdxColony index_slc; //representing Sum, Product or UnknownFunction's extra parameters 
		Variable variable; //alias for string
		Sum sum;         //alias for index_slc
		Product product; //alias for index_slc

		TypesUnion() :complex() {}

		TypesUnion(const KnownFunction&   val) :known_function(val)   {}
		TypesUnion(const UnknownFunction& val) :unknown_function(val) {}
		TypesUnion(const Complex&         val) :complex(val)          {}
		TypesUnion(const TermString128&   val) :string(val)           {} 
		TypesUnion(const TypedIdxColony&  val) :index_slc(val)        {}
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);
	using Store = TermStore<TypesUnion>;

	//everything using the TermSLC needs to have a way to get the right union member from the union
	struct ToString   { using Result = TermString128;  static Result& apply(TypesUnion& val) { return val.string;    } };
	struct ToIndexSLC { using Result = TypedIdxColony; static Result& apply(TypesUnion& val) { return val.index_slc; } };
	struct ToConstString   { using Result = const TermString128;  static Result& apply(const TypesUnion& val) { return val.string;    } };
	struct ToConstIndexSLC { using Result = const TypedIdxColony; static Result& apply(const TypesUnion& val) { return val.index_slc; } };
	using ToSum     = ToIndexSLC;
	using ToProduct = ToIndexSLC;
	using ToConstSum     = ToConstIndexSLC;
	using ToConstProduct = ToConstIndexSLC;

	//utility for both KnownFunction and UnknownFunction
	namespace function {

		std::string_view name_of(FunctionType type) noexcept;

		//appends only name, no parentheses or anything fancy
		void append_name(const Store& store, const UnknownFunction& func, std::string& str);

		//only expects actual name part of function, e.g. "asin", NOT "asin(...)"
		//if name is one of FunctionType, that is returned, else FunctionType::UNKNOWN
		FunctionType type_of(const ParseView input) noexcept;

		std::size_t param_count(FunctionType type) noexcept;

		std::span<TypedIdx> range(KnownFunction& func) noexcept;
		std::span<const TypedIdx> range(const KnownFunction& func) noexcept;

		template<typename Modifier>
		void for_each(Store& store, UnknownFunction& func, Modifier modifier)
		{
			switch (func.param_count) {
			case UnknownFunction::ParamCount::one:
				modifier(store, func.short_params[0]);
				break;
			case UnknownFunction::ParamCount::two:
				modifier(store, func.short_params[0]);
				modifier(store, func.short_params[1]);
				break;
			case UnknownFunction::ParamCount::more:
				for (auto param : range<ToIndexSLC>(store, func.params_idx)) {
					modifier(store, param);
				}
				break;
			}
		}

		template<typename Modifier>
		void for_each(const Store& store, const UnknownFunction& func, Modifier modifier)
		{
			switch (func.param_count) {
			case UnknownFunction::ParamCount::one:
				modifier(store, func.short_params[0]);
				break;
			case UnknownFunction::ParamCount::two:
				modifier(store, func.short_params[0]);
				modifier(store, func.short_params[1]);
				break;
			case UnknownFunction::ParamCount::more:
				for (auto param : range<ToConstIndexSLC>(store, func.params_idx)) {
					modifier(store, param);
				}
				break;
			}
		}
	}

	//evaluates tree if possible, throws if variables of unknown value /unknown_functions are present
	std::complex<double> eval(const Store& store, TypedIdx ref);

	void to_string(const Store& store, TypedIdx ref, std::string& str, const int parent_precedence = -1);

	void to_memory_layout(const Store& store, TypedIdx ref, std::vector<std::string>& content);

}	//namespace bmath::intern::arithmetic

namespace bmath {

	struct ArithmeticTerm
	{
		intern::arithmetic::TypedIdx head;
		intern::arithmetic::Store values;
		std::string show_memory_layout(const bool show_first_table = true) const;
	};	//class ArithmeticTerm

}	//namespace bmath



