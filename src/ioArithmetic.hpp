#pragma once

#include "parseTerm.hpp"
#include "arithmeticTerm.hpp"

namespace bmath::intern {

	struct Head
	{
		std::size_t where;
		enum class Type
		{
			sum,	     //where specifies position of found operator
			negate,	     //where specifies position of found operator
			product,     //where specifies position of found operator
			power,       //where specifies position of found operator
			real_value,  //where is unspecified
			imag_value,  //where is unspecified
			variable,    //where is unspecified
			group,       //where is unspecified
			function,    //where specifies position of opening parenthesis
			natural_computable,  //where is unspecified
			complex_computable,  //where is unspecified
		} type;
	};

	namespace compute {

		enum class Result { not_exactly_computable, natural, complex };

		//quite conservative in what operations are allowed, but still sometimes returns allowed if eval is unexact.
		// (happens if numbers are too large to be stored exactly)
		Result exactly_computable(const ParseView view) noexcept;

		//only expects operations '+', '*', '-' on numbers in { a + bi | a, b in Z }
		std::complex<double> eval_complex(ParseView view);

		//only expects operations '+', '*', '^' on numbers in N
		double eval_natural(ParseView view);

		//expects number in engineering notation
		double parse_value(const ParseView view);

	} //namespace compute

	//returns position of first non-arithmetic token in view
	std::size_t find_first_not_arithmetic(const TokenView view) noexcept;

	//decides what type the outhermost element has
	//offset is used to determine error position relative to begin of whole term
	[[nodiscard]] Head find_head_type(const ParseView view);

	//returns head
	[[nodiscard]] TypedIdx build(Store& store, ParseView view);


	template<typename TypedIdx_T, typename Store_T>
	[[nodiscard]] TypedIdx_T build_value(Store_T& store, const std::complex<double> complex) noexcept
	{
		return TypedIdx_T(store.insert(complex), Type(Leaf::complex));
	}

	template<typename Store_T, typename TypedIdx_T>
	[[nodiscard]] TypedIdx_T build_negated(Store_T& store, const TypedIdx_T to_negate) noexcept
	{
		using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
		const TypedIdx_T minus_1 = build_value<TypedIdx_T>(store, -1.0);
		return TypedIdx_T(store.insert(TypedIdxSLC_T({ minus_1, to_negate })), Type(Op::product));
	}

	template<typename Store_T, typename TypedIdx_T>
	[[nodiscard]] TypedIdx_T build_inverted(Store_T& store, const TypedIdx_T to_invert) noexcept
	{
		const TypedIdx_T minus_1 = build_value<TypedIdx_T>(store, -1.0);
		return TypedIdx_T(store.insert(FnParams<TypedIdx_T>{ to_invert, minus_1 }), Type(Fn::pow));
	}

	namespace pattern {

		struct [[nodiscard]] PatternParts
		{
			ParseView declarations;
			ParseView lhs;
			ParseView rhs;
		};

		//input is assumed to be of form "<declarations> | <lhs> = <rhs>"
		//or, if no MatchVariables occur, of restr "<lhs> = <rhs>"
		PatternParts split(const ParseView input);

		using ParseRestriction = SumEnum<Restriction, Form>;
		//data belonging to one TreeMatchVariable relevant while constructing pattern
		struct [[nodiscard]] MultiNameLookup 
		{
			std::string_view name;
			Restriction restr;
			StupidBufferVector<PnTypedIdx, 3u> lhs_instances;
			StupidBufferVector<PnTypedIdx, 3u> rhs_instances;

			MultiNameLookup(std::string_view new_name, Restriction new_restr) noexcept
				:name(new_name), restr(new_restr) {}
		};
		
		//data belonging to one ValueMatchVariable relevant while constructing pattern
		struct [[nodiscard]] ValueNameLookup 
		{
			std::string_view name;
			Form form;
			StupidBufferVector<PnTypedIdx, 3u> lhs_instances;
			StupidBufferVector<PnTypedIdx, 3u> rhs_instances;

			ValueNameLookup(std::string_view new_name, Form new_form) noexcept
				:name(new_name), form(new_form) {}
		};

		//only exists during construction of pattern
		struct NameLookupTable
		{
			std::vector<MultiNameLookup> tree_table;
			std::vector<ValueNameLookup> value_table;
			bool build_lhs = true; //false -> currently building rhs

			PnTypedIdx insert_instance(PnStore& store, const ParseView input);
		};

		NameLookupTable parse_declarations(ParseView declarations);

		struct PatternBuildFunction
		{
			//the index of name in table is also match_data_idx of TreeMatchVariable
			NameLookupTable& table;

			//equivalent to build() for pattern
			PnTypedIdx operator()(PnStore& store, ParseView input);
		};

	} //namespace pattern

	namespace print {

		template<typename Store_T, typename TypedIdx_T>
		void append_to_string(const Store_T& store, const TypedIdx_T ref, std::string& str, const int parent_infixr = 0);

		//prettier, but also slower
		std::string to_pretty_string(const Store& store, const TypedIdx ref, const int parent_infixr = 0);

		template<typename Store_T, typename TypedIdx_T>
		std::string to_memory_layout(const Store_T& store, const std::initializer_list<const TypedIdx_T> heads);

	} //namespace print

} //namespace bmath::intern