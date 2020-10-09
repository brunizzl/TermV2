#pragma once

#include "parseTerm.hpp"
#include "arithmeticTerm.hpp"

namespace bmath::intern {

	//returns position of first non_arithmetic token in view
	std::size_t find_first_not_arithmetic(const TokenView view);

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
		} type;
	};

	//decides what type the outhermost element has
	//offset is used to determine error position relative to begin of whole term
	[[nodiscard]] Head find_head_type(const TokenView token_view, std::size_t offset);

	//returns head
	[[nodiscard]] TypedIdx build(Store& store, ParseView view);

	namespace pattern {

		struct PatternParts
		{
			ParseView declarations;
			ParseView lhs;
			ParseView rhs;
		};

		//input is assumed to be of form "<declarations> | <lhs> = <rhs>"
		//or, if no MatchVariables occur, of form "<lhs> = <rhs>"
		PatternParts split(const ParseView input);

		//lookup if new MatchVariable with name "name" is parsed, to get the Restriction and shared_data_idx
		struct NameLookup 
		{
			std::string_view name;
			std::uint32_t shared_data_idx;
			Restriction restriction;
		};

		std::vector<NameLookup> parse_declarations(ParseView declarations);

		struct PatternBuildFunction
		{
			const decltype(PnTerm::shared_match_data)& shared_match_data;

			std::vector<NameLookup>& name_map;

			PnTypedIdx operator()(PnStore& store, ParseView view);
		};

	} //namespace pattern

	namespace print {

		void append_complex(const std::complex<double> val, std::string& dest, int parent_operator_precedence);

		void append_real(double val, std::string& dest);

		void append_to_string(const Store& store, const TypedIdx ref, std::string& str, const int parent_infixr = 0);

		//prettier, but also slower
		std::string to_pretty_string(const Store& store, const TypedIdx ref, const int parent_infixr = 0);

		void to_memory_layout(const Store& store, const TypedIdx ref, std::vector<std::string>& content);

	} //namespace print

} //namespace bmath::intern