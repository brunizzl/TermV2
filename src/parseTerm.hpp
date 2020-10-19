#pragma once

#include <string>
#include <string_view>
#include <cassert>

#include "termUtility.hpp"

namespace bmath {

	struct [[nodiscard]] ParseFailure
	{
		std::size_t where;	//index of invalid token /character
		const char* what;
	}; //struct ParseFailure

} //namespace bmath

namespace bmath::intern {

	using Token = char;
	//TokenString is intended to be used along the string to be parsed to associate every char with what it represents 
	//A TokenString may only hold a combination of the following chars:
	//names for characters standing for not (only) themselves:
	namespace token {
		//'c' representing any character in a closer sense: "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
		//'c' might also represent (non-leading) digits if occuring in a name: "0123456789" (note: no '.' allowed)
		//'c' also represents '\''. this has two meanings: 
		//    1. if variable name starts with '\'', it has to end with '\'', then the name is quoted (only in pattern context)
		//    2. if variable -/ function name has '\'' somewhere in between or at end, it is interpreted as normal character.
		constexpr Token character = 'c';
		//'n' representing any char composing a number literal: "0123456789."
		//'n' might also represent "+-eE" if used to specify numbers 
		//  as by engeneering notation: "<base>'e'<optional '+' or '-'><exponent>"
		constexpr Token number = 'n';
		constexpr Token open_grouping = '('; //representing "([{"
		constexpr Token clse_grouping = ')'; //representing ")]}"
		constexpr Token unary_minus = '-';
		constexpr Token sum = 'A';     //representing '+' and '-' as binary operators
		constexpr Token product = 'M'; //representing '*' and '/' as binary operators

									   //tokens representing themselfs as chars:
		constexpr Token comma = ',';
		constexpr Token hat = '^';
		constexpr Token equals = '='; //only used as single char, not to compare
		constexpr Token bar = '|';
		constexpr Token colon = ':';
		constexpr Token space = ' ';
		constexpr Token imag_unit = 'i';
	}

	struct [[nodiscard]] TokenString :std::string
	{
		using std::string::basic_string;
		explicit TokenString(const std::string& other) noexcept :std::string(other) {}
		explicit TokenString(std::string&& other) noexcept :std::string(other) {}
	};

	struct [[nodiscard]] TokenView :std::string_view
	{
		using std::string_view::basic_string_view;
		constexpr explicit TokenView(const std::string_view& other) noexcept :std::string_view(other) {}
		constexpr explicit TokenView(std::string_view&& other) noexcept :std::string_view(other) {}
	};

	struct [[nodiscard]] ParseString
	{
		TokenString tokens;
		std::string& name;

		ParseString(std::string& new_name) noexcept;

		//changes spaces out to multiplication operator if appropriate
		void allow_implicit_product() noexcept;

		//will not remove '\n' and the like, only ' ' -> assumes standardize_whitespace already run
		void remove_space() noexcept;

		std::size_t size() const noexcept { assert(this->tokens.size() == this->name.size()); return this->tokens.size(); }
	};

	//as parsing always needs both a string_view to the actual input and a TokenView to the tokenized input, 
	//  this struct packs both together (and also the offset from the beginning)
	struct [[nodiscard]] ParseView
	{
		TokenView tokens;
		const char* chars;
		std::size_t offset; //distance to actual beginning of string(s)

		ParseView(const ParseString& str) noexcept 
			:tokens(str.tokens), chars(str.name.data()), offset(0)
		{
			throw_if(str.name.size() != str.tokens.size(), 
				"expected both views to represent same data -> have same length");
		}

		constexpr ParseView(const TokenView& new_tokens, const char* new_chars, std::size_t offset_) noexcept
			:tokens(new_tokens), chars(new_chars), offset(offset_) {}

		//never produces undefinded behavior, unlike unlike std::string_view's version >:(
		constexpr void remove_prefix(const std::size_t count) noexcept
		{
			const std::size_t remove = std::min(count, this->tokens.size());
			this->tokens.remove_prefix(remove);
			this->chars += remove;
			this->offset += remove;
		}

		constexpr void remove_suffix(const std::size_t count) noexcept 
		{
			this->tokens.remove_suffix(count);
		}

		constexpr std::size_t size() const noexcept { return this->tokens.size(); }

		constexpr ParseView substr(std::size_t offset_, std::size_t count = TokenView::npos) const noexcept 
		{ 
			return ParseView(TokenView(this->tokens.substr(offset_, count)), 
				this->chars + offset_, this->offset + offset_); 
		}

		constexpr ParseView steal_prefix(const std::size_t count) noexcept
		{
			const std::size_t remove = std::min(count, this->tokens.size());
			const ParseView to_steal(TokenView(this->tokens.substr(0, remove)), this->chars, this->offset);
			this->tokens.remove_prefix(remove);
			this->chars += remove;
			this->offset += remove;
			return to_steal;
		}

		constexpr std::string_view to_string_view(const std::size_t start = 0u, const std::size_t end = std::string_view::npos) 
			const noexcept 
		{ 
			assert(start <= end);
			return { this->chars + start, end > this->size() ? this->size() - start : end - start }; 
		}
	};

	TokenString tokenize(const std::string_view name);

	//searches from clsd_par to front, as term is constructed from the right.
	std::size_t find_open_par(std::size_t clsd_par, const TokenView name) noexcept;

	//searches from open_par to back
	std::size_t find_closed_par(std::size_t open_par, const TokenView name) noexcept;

	//search all of name not enclosed by parentheses for token
	std::size_t find_first_of_skip_pars(const TokenView name, const Token token) noexcept;

	//counts occurences of token in all of name not enclosed by parentheses
	std::size_t count_skip_pars(const TokenView name, const Token token) noexcept;

} //namespace bmath::intern