
#include <cassert>
#include <algorithm>

#include "utility/misc.hpp"

#include "parseTerm.hpp"

namespace simp {

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////local definitions//////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	constexpr bool is_number_literal(const Token t) { return is_one_of<token::number, token::imag_unit>(t); }
	constexpr bool is_operator(const Token t) { return is_one_of<token::sum, token::product, token::hat>(t); }
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////exported in header/////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	ParseString::ParseString(std::string& new_name) :name(new_name)
	{
		//all groups of whitespaces are shortened /changed to only a single space
		const auto standardize_whitespace = [](std::string& str) {
			std::size_t last_correct_idx = -1;
			for (const char c : str) {
				if (!std::isspace(c)) {
					str[++last_correct_idx] = c; //no whitespace -> just insert it as is
				}
				else if (last_correct_idx != -1 && str[last_correct_idx] != ' ') {
					str[++last_correct_idx] = ' '; //last was no whitespace -> insert single whitespace
				}
			}
			str.erase(last_correct_idx + 1);
		}; //standardize_whitespace

		standardize_whitespace(new_name);
		this->tokens = tokenize(name);
	}

	void ParseString::allow_implicit_product(const Token replace_tk, const char replace_ch) noexcept
	{
		assert(this->tokens.length() == this->name.length());
		for (std::size_t prev_idx = 0; prev_idx + 2 < this->tokens.length(); prev_idx++) {
			const std::size_t curr_idx = prev_idx + 1;
			const Token prev = this->tokens[prev_idx];
			const Token curr = this->tokens[prev_idx + 1];
			const Token next = this->tokens[prev_idx + 2];
			if (is_one_of<token::character, token::number, token::imag_unit, token::clse_grouping, token::dollar>(prev) && 
				is_one_of<token::character, token::number, token::imag_unit, token::open_grouping, token::dollar>(next) && 
				curr == token::space) 
			{
				this->tokens[curr_idx] = replace_tk;
				this->name[curr_idx] =  replace_ch;
			}
		}
	} //allow_implicit_product

	void ParseString::mark_char_space() noexcept
	{
		for (std::size_t i = 0; i + 2 < this->size(); i++) {
			if (this->tokens[i] == token::character &&
				this->tokens[i + 1] == token::space &&
				this->tokens[i + 2] == token::character)
			{
				this->tokens[i + 1] = token::character;
			}
		}
	} //mark_char_space

	void ParseString::remove_space() noexcept
	{
		assert(this->tokens.size() == this->name.size());
		std::size_t next_insert_pos = 0;
		for (std::size_t i = 0; i < tokens.size(); i++) {
			if (this->tokens[i] != token::space) {
				this->name[next_insert_pos] = this->name[i];
				this->tokens[next_insert_pos] = this->tokens[i];
				next_insert_pos++;
			}
		}
		this->name.erase(next_insert_pos);
		this->tokens.erase(next_insert_pos);
		assert(this->tokens.size() == this->name.size());
	} //remove_space


	TokenString tokenize(const std::string_view name)
	{
		const auto in_interval = [](const char test, const char lower, const char upper) {
			return test >= lower && test <= upper;
		};

		TokenString tokenized(name.length(), '\0');
		{
			int nr_paren = 0;	//counts number of '(' minus number of ')', (may never be negative in valid string) 
			int nr_brack = 0;	//counts number of '[' minus number of ']', (may never be negative in valid string)
			int nr_brace = 0;	//counts number of '{' minus number of '}', (may never be negative in valid string)
			for (std::size_t i = 0u; i < name.length(); i++) {
				const char current = name[i];
				if (current == 'i') {
					tokenized[i] = token::imag_unit;
					continue;
				}
				if (in_interval(current, 'a', 'z') || in_interval(current, 'A', 'Z') || is_one_of<'_', '\''>(current)) {
					tokenized[i] = token::character;
					continue;
				}
				if (in_interval(current, '0', '9')) {
					tokenized[i] = token::number;
					continue;
				}

				switch (current) {
				case  ',': tokenized[i] = token::comma;     continue;
				case  '.': tokenized[i] = token::dot;       continue;
				case  '^': tokenized[i] = token::hat;       continue;
				case  '=': tokenized[i] = token::equals;    continue;
				case  '|': tokenized[i] = token::bar;       continue;
				case  ':': tokenized[i] = token::colon;     continue;
				case  '$': tokenized[i] = token::dollar;    continue;
				case  '!': tokenized[i] = token::bang;      continue;
				case  '&': tokenized[i] = token::ampersand; continue;
				case  '<': tokenized[i] = token::relation;  continue;
				case  '>': tokenized[i] = token::relation;  continue;
				case '\\': tokenized[i] = token::backslash; continue;
				case  ' ': tokenized[i] = token::space;     continue;

				case '+': [[fallthrough]];
				case '-': 
					tokenized[i] = token::sum;
					continue;
				case '*': [[fallthrough]];
				case '/': 
					tokenized[i] = token::product;
					continue;

				case '(': 
					nr_paren++;
					tokenized[i] = token::open_grouping;
					continue;
				case '[': 
					nr_brack++;
					tokenized[i] = token::open_grouping;
					continue;
				case '{': 
					nr_brace++;
					tokenized[i] = token::open_grouping;
					continue;
				case ')': 
					if (--nr_paren < 0) [[unlikely]] throw ParseFailure{ i, "poor grouping, expected matching open parenthesis" };
					if (name[find_open_par(i, TokenView(tokenized))] != '(') [[unlikely]] throw ParseFailure{ i, "poor grouping, can't close parenthesis yet" };
					tokenized[i] = token::clse_grouping;
					continue;
				case ']':
					if (--nr_brack < 0) [[unlikely]] throw ParseFailure{ i, "poor grouping, expected matching open bracket" };
					if (name[find_open_par(i, TokenView(tokenized))] != '[') [[unlikely]] throw ParseFailure{ i, "poor grouping, can't close bracket yet" };
					tokenized[i] = token::clse_grouping;
					continue;
				case '}':
					if (--nr_brace < 0) [[unlikely]] throw ParseFailure{ i, "poor grouping, expected matching open brace" };
					if (name[find_open_par(i, TokenView(tokenized))] != '{') [[unlikely]] throw ParseFailure{ i, "poor grouping, can't close brace yet" };
					tokenized[i] = token::clse_grouping;
					continue;
				}
				throw ParseFailure{ i, "unexpected character" };
			}
			if (nr_paren != 0) [[unlikely]] throw ParseFailure{ name.length() - 1, "poor grouping, not all parenteses where closed" };
			if (nr_brack != 0) [[unlikely]] throw ParseFailure{ name.length() - 1, "poor grouping, not all brackets where closed" };
			if (nr_brace != 0) [[unlikely]] throw ParseFailure{ name.length() - 1, "poor grouping, not all braces where closed" };
		}

		if (name.starts_with('-')) {
			tokenized.front() = token::unary_minus;
		} 
		//optimize token choice by not looking at single Token / char, but at two at once
		Token last_nonspace_tn = '\0';
		for (std::size_t prev_idx = 0u; prev_idx + 1u < name.length(); prev_idx++) {
			const std::size_t curr_idx = prev_idx + 1u;
			Token& prev_tn = tokenized[prev_idx];
			Token& curr_tn = tokenized[curr_idx];
			const char prev_ch = name[prev_idx];
			const char curr_ch = name[curr_idx];

			if (prev_tn != token::space) {
				last_nonspace_tn = prev_tn;
			}

			if (is_operator(prev_tn) && is_operator(curr_tn)) [[unlikely]] throw ParseFailure{ curr_idx, "illegal operator sequence" };

			//change 'i' occuring at start of characters belonging to variables / function names to token::character
			if (prev_tn == token::imag_unit && curr_tn == token::character) {
				prev_tn = token::character;
			}
			if (prev_tn == token::imag_unit && curr_tn == token::imag_unit) {
				prev_tn = token::character;
				curr_tn = token::character;
			}
			//change token representing digits or 'i' occuring in names to token::character
			else if (prev_tn == token::character && is_one_of<token::number, token::imag_unit>(curr_tn)) {
				curr_tn = token::character;
			}
			//change decimal dot to token::number
			else if (prev_tn == token::number && curr_tn == token::dot) {
				curr_tn = token::number;
			}
			//change ellipses to token::character
			else if (prev_tn == token::dot && curr_tn == token::dot) {
				const std::size_t next_idx = curr_idx + 1u;
				if (tokenized.size() <= next_idx || tokenized[next_idx] != token::dot) 
					[[unlikely]] throw ParseFailure{ curr_idx, "dots are expected to come alone or in groups of three" };
				prev_tn = token::character;
				curr_tn = token::character;
				tokenized[next_idx] = token::character;
			}
			//change token representing any of "e+-" occuring in numbers to token::number
			else if (prev_tn == token::number && curr_ch == 'e' ||
			         prev_tn == token::number && prev_ch == 'e' && curr_tn == token::sum) 
			{
				curr_tn = token::number;
			}
			//change unary minus to token::unary_minus
			else if (is_one_of<token::open_grouping, token::equals, token::comma, token::bar, token::dot>(last_nonspace_tn) && curr_ch == '-') {
				curr_tn = token::unary_minus;
			}
			// if it is, change token::unary_minus to part of number
			else if (prev_tn == token::unary_minus && curr_tn == token::number) {
				prev_tn = token::number;
			}
			//change token::number occuring after token::dollar to token:dollar
			else if (prev_tn == token::dollar && curr_tn == token::number) {
				curr_tn = token::dollar;
			}
			//change token::dollar occuring in front of token::character to token::character
			else if (prev_tn == token::dollar && curr_tn == token::character) {
				prev_tn = token::character;
			}

			//replace relations and operators composed of two characters
			else if (is_one_of<'=', '!', '>', '<'>(prev_ch) && curr_ch == '=') {
				prev_tn = token::relation;
				curr_tn = token::relation;
			}
			else if (prev_ch == '&' && curr_ch == '&') {
				prev_tn = token::and_;
				curr_tn = token::and_;
			}
			else if (prev_ch == '|' && curr_ch == '|') {
				prev_tn = token::or_;
				curr_tn = token::or_;
			}
		}

		return tokenized;
	} //tokenize

	std::size_t find_open_par(std::size_t clsd_par, const TokenView name) noexcept
	{
		std::size_t open_par = name.find_last_of(token::open_grouping, clsd_par - 1u);
		clsd_par = name.find_last_of(token::clse_grouping, clsd_par - 1u);
		while (clsd_par > open_par && clsd_par != TokenView::npos) {
			open_par = name.find_last_of(token::open_grouping, open_par - 1u);
			clsd_par = name.find_last_of(token::clse_grouping, clsd_par - 1u);
		}
		return open_par;
	} //find_open_par

	std::size_t find_closed_par(std::size_t open_par, const TokenView name) noexcept
	{
		std::size_t clsd_par = name.find_first_of(token::clse_grouping, open_par + 1u);
		open_par = name.find_first_of(token::open_grouping, open_par + 1u);
		while (open_par < clsd_par) {
			clsd_par = name.find_first_of(token::clse_grouping, clsd_par + 1u);
			open_par = name.find_first_of(token::open_grouping, open_par + 1u);
		}
		return clsd_par;
	} //find_closed_par

	std::size_t find_first_of_skip_pars(const TokenView name, const Token token) noexcept
	{
		std::size_t open_par = name.find_first_of(token::open_grouping);
		std::size_t after_clsd_par = 0u;
		while (open_par != TokenView::npos) {
			const TokenView search_view(name.substr(after_clsd_par, open_par - after_clsd_par)); //only search "...)here(..."			
			if (const std::size_t found = search_view.find_first_of(token); found != TokenView::npos) {
				return found + after_clsd_par;
			}
			after_clsd_par = find_closed_par(open_par, name) + 1u;
			open_par = name.find_first_of(token::open_grouping, after_clsd_par);
		}
		return name.find_first_of(token, after_clsd_par);
	} //find_first_of_skip_pars

	std::size_t find_last_of_skip_pars(const TokenView name, const Token token) noexcept
	{
		std::size_t clsd_par = name.find_last_of(token::clse_grouping);
		std::size_t open_par = name.size();
		while (clsd_par != TokenView::npos) {
			const TokenView search_view(name.substr(clsd_par + 1u, open_par - clsd_par - 1u)); //only search "...)here(..."
			if (const std::size_t found = search_view.find_last_of(token); found != TokenView::npos) {
				return found + clsd_par + 1u;
			}
			open_par = find_open_par(clsd_par, name);
			clsd_par = name.find_last_of(token::clse_grouping, open_par);
		}
		return name.find_last_of(token, open_par);
	} //find_last_of_skip_pars

	std::size_t count_skip_pars(const TokenView name, const Token token) noexcept
	{
		std::size_t count = 0u;
		std::size_t open_par = name.find_first_of(token::open_grouping);
		std::size_t after_clsd_par = 0u;
		while (open_par != TokenView::npos) {
			const Token* begin = name.data() + after_clsd_par;
			const Token* end = name.data() + open_par; //only search "...)here(..."
			count += std::count(begin, end, token);

			after_clsd_par = find_closed_par(open_par, name) + 1u;
			open_par = name.find_first_of(token::open_grouping, after_clsd_par);
		}
		const Token* begin = name.data() + after_clsd_par;
		const Token* end = name.data() + name.length();
		count += std::count(begin, end, token);
		return count;
	} //count_skip_pars

} //namespace simp