#pragma once

#include "parseTerm.hpp"
#include "arithmeticTerm.hpp"

namespace bmath::intern::arithmetic {

	//returns position of first non_arithmetic token in view
	std::size_t find_first_not_arithmetic(const TokenView view);

	//allows a number to be directly followed by a variable or function name
	//e.g. inserts multiplication operator in between
	void allow_implicit_product(TokenString& tokens, std::string& name);

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

	//utility for build (as build_variadic and build_function)
	TypedIdx build_value(Store& store, double re, double im = 0.0);

	//returns head, offset is used to determine error position relative begin of whole term
	TypedIdx build(Store& store, ParseView view);

	struct SumTraits
	{
		static constexpr Type type_name = Type::sum;
		static constexpr double neutral_element = 0.0;
		static constexpr char operator_char = '+';
		static constexpr char inverse_operator_char = '-';
		static constexpr Token operator_token = token::sum;
	};

	struct ProductTraits
	{
		static constexpr Type type_name = Type::product;
		static constexpr double neutral_element = 1.0;
		static constexpr char operator_char = '*';
		static constexpr char inverse_operator_char = '/';
		static constexpr Token operator_token = token::product;
	};

	//VariadicTraits must include:
	//<Enum type> type_name: name of operation in enum representing all types in store
	//double neutral_element: the value not changing any other value if both are combined using operation e.g. 0.0
	//char operator_char: the character symbolizing the operation, e.g. '+'
	//char inverse_operator_char: the character symbolizing the inverse operation, e.g. '-'
	//Token operator_token: the token symbolizing both normal and inverse operation, e.g. token::sum

	//BuildInverse recieves an already build term (by TypedIdx_T) and returns the inverse (by TypedIdx_T)
	//  e.g. for sum, it should turn "a" -> "a*(-1)", for product "a" -> "a^(-1)"
	//BuildAny can buld any type of term, this function will very likely already call build_variadic.
	template<typename VariadicTraits, typename UnionToSLC, typename TypedIdx_T, typename TermStore_T,
		typename BuildInverse, typename BuildAny>
	TypedIdx_T build_variadic(TermStore_T& store, ParseView input, std::size_t op_idx,
			BuildInverse build_inverse, BuildAny build_any)
	{
		std::size_t variadic_idx;
		{
			const auto subterm_view = input.substr(0, op_idx);
			const TypedIdx_T subterm = build_any(store, subterm_view);
			input.remove_prefix(op_idx);
			variadic_idx = store.insert(UnionToSLC::Result(subterm));
		}
		while (input.size()) {
			const char current_operator = input.chars[0];
			input.remove_prefix(1); //remove current_operator;
			op_idx = find_first_of_skip_pars(input.tokens, VariadicTraits::operator_token);
			const auto subterm_view = input.substr(0, op_idx);
			const TypedIdx_T subterm = build_any(store, subterm_view);
			input.remove_prefix(op_idx == TokenView::npos ? input.size() : op_idx);
			switch (current_operator) {
			case VariadicTraits::operator_char:
				insert_new<UnionToSLC>(store, variadic_idx, subterm);
				break;
			case VariadicTraits::inverse_operator_char:
				insert_new<UnionToSLC>(store, variadic_idx, build_inverse(store, subterm));
				break;
			default: assert(false);
			}
		}
		return TypedIdx_T(variadic_idx, VariadicTraits::type_name);
	} //build_variadic


	TypedIdx build_function(Store& store, ParseView input, const std::size_t open_par);


} //namespace bmath::intern::arithmetic