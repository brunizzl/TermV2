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
	Head find_head_type(const TokenView token_view, std::size_t offset);

	//utility for build
	TypedIdx build_value(Store& store, double re, double im = 0.0);

	//returns head
	TypedIdx build(Store& store, ParseView view);

	namespace print {

		void append_complex(const std::complex<double> val, std::string& dest, int parent_operator_precedence);

		void append_real(double val, std::string& dest);

		void append_to_string(const Store& store, const TypedIdx ref, std::string& str, const int parent_infixr = 0);

		//prettier, but also slower
		std::string to_pretty_string(const Store& store, const TypedIdx ref, const int parent_infixr = 0);

		void to_memory_layout(const Store& store, const TypedIdx ref, std::vector<std::string>& content);

	} //namespace print

} //namespace bmath::intern