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
			value,       //where is unspecidied
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

		//expects view.tokens to eighter consisting of only token::number, 
		//  or have single non-number-token at end beeing token::imag_unit
		std::complex<double> parse_value(ParseView view);

	} //namespace compute

	//returns position of first non-arithmetic token in view
	std::size_t find_first_not_arithmetic(const TokenView view);

	//decides what type the outhermost element has
	//offset is used to determine error position relative to begin of whole term
	[[nodiscard]] Head find_head_type(const ParseView view);

	//returns head
	[[nodiscard]] TypedIdx build(Store& store, ParseView view);

	namespace pattern {

		struct PatternParts
		{
			ParseView declarations;
			ParseView lhs;
			ParseView rhs;
		};

		//input is assumed to be of restr "<declarations> | <lhs> = <rhs>"
		//or, if no MatchVariables occur, of restr "<lhs> = <rhs>"
		PatternParts split(const ParseView input);

		//lookup if new MatchVariable with name "name" is parsed, to get the Restriction and shared_data_idx
		struct NameLookup 
		{
			std::string_view name;
			Restriction restr;
		};

		std::vector<NameLookup> parse_declarations(ParseView declarations);

		struct PatternBuildFunction
		{
			//the index of name in name_map is also shared_data_idx of MatchVariable
			const std::vector<NameLookup>& name_map;

			//equivalent to build() for pattern
			PnTypedIdx operator()(PnStore& store, ParseView input) const;
		};

	} //namespace pattern

	namespace print {

		void append_complex(const std::complex<double> val, std::string& dest, int parent_operator_precedence);

		void append_real(double val, std::string& dest);

		template<typename Store_T, typename TypedIdx_T>
		void append_to_string(const Store_T& store, const TypedIdx_T ref, std::string& str, const int parent_infixr = 0);

		//prettier, but also slower
		std::string to_pretty_string(const Store& store, const TypedIdx ref, const int parent_infixr = 0);

		template<typename Store_T, typename TypedIdx_T>
		std::string show_memory_layout(const Store_T& store, const TypedIdx_T head);

	} //namespace print

} //namespace bmath::intern