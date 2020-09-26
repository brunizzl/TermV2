#pragma once

#include <string>
#include <string_view>

#include "arithmeticTerm.hpp"

namespace bmath::intern {

	struct TokenString :std::string
	{
		using std::string::basic_string;
		explicit TokenString(const std::string& other) :std::string(other) {}
		explicit TokenString(std::string&& other) :std::string(other) {}
	};

	struct TokenView :std::string_view
	{
		using std::string_view::basic_string_view;
		explicit TokenView(const std::string_view& other) :std::string_view(other) {}
		explicit TokenView(std::string_view&& other) :std::string_view(other) {}
	};

	//TokenString is intended to be used along the string to be parsed to associate every char with what it represents 
	//A TokenString may only hold a combination of the following chars:
	//names for characters standing for not (only) themselves:
	namespace token {
		//'c' representing any character in a closer sense: "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
		//'c' might also represent (non-leading) digits if occuring in a name: "0123456789" (note: no '.' allowed)
		constexpr char character = 'c';
		//'n' representing any char composing a number literal: "0123456789."
		//'n' might also represent "+-e" if used to specify numbers as by engeneering notation: <base>'e'<optional '+' or '-'><exponent>
		constexpr char number = 'n';
		constexpr char open_grouping = '('; //representing "([{"
		constexpr char clse_grouping = ')'; //representing ")]}"
		constexpr char unary_minus = '-';
		constexpr char sum = 'A';     //representing '+' and '-' as binary operators
		constexpr char product = 'M'; //representing '*' and '/' as binary operators
		constexpr char smaller_equal = 's'; //may only occur in pairs to represent "<="
		constexpr char larger_equal = 'l';  //may only occur in pairs to represent ">="
		constexpr char equal = 'e';         //may only occur in pairs to represent "=="
	}
	//characters representing themselfs (thus not needing an alias in token namespace):
	//',' 
	//'^' 		
	//'!' 
	//'?' 
	//'=' representing '=', but only used as single char, not to compare
	//'<' representing only "smaller than, not part of "<="
	//'>' representing only "larger than, not part of ">="
	//'|' representing '|'
	//'&' representing '&'


	struct ParseFailure
	{
		std::size_t where;	//index of invalid token
		enum class What
		{
			illegal_char,
			poor_grouping,
			illegal_ops,	//short for illegal operators
			illformed_val,
		} what;
	};

	void remove_whitspace(std::string& str);

	TokenString tokenize(const std::string_view name);

	//searches from clsd_par to front, as term is constructed from the right.
	std::size_t find_open_par(const std::size_t clsd_par, const TokenView name);

	//searches from open_par to back
	std::size_t find_closed_par(const std::size_t open_par, const TokenView name);

	//searches all of name not enclosed by parentheses for character
	std::size_t find_last_of_skip_pars(const TokenView name, const char character);
	std::size_t find_first_of_skip_pars(const TokenView name, const char character);

	namespace arithmetic {

		//only allows tokens used to describe arithmetic term in view
		bool is_arithmetic(const TokenView view);

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

		TypedIdx build_number(ArithmeticStore& store, double re, double im = 0);

		//returns head, offset is used to determine error position relative begin of whole term
		TypedIdx build(ArithmeticStore& store, TokenView token_view, std::string_view name_view, const std::size_t offset);

	} //namespace arithmetic

} //namespace bmath::intern