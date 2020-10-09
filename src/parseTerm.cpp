
#include <cassert>
#include <algorithm>

#include "parseTerm.hpp"
#include "termUtility.hpp"

namespace bmath::intern {

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////local definitions//////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
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

	void ParseString::allow_implicit_product() noexcept
	{
		const auto is_literal = [](const Token token) { return token == token::character || token == token::number; };

		assert(this->tokens.length() == this->name.length());
		for (std::size_t prev_idx = 0; prev_idx + 2 < this->tokens.length(); prev_idx++) {
			const std::size_t curr_idx = prev_idx + 1;
			const Token prev = this->tokens[prev_idx];
			const Token curr = this->tokens[prev_idx + 1];
			const Token next = this->tokens[prev_idx + 2];
			if (is_literal(prev) && curr == token::space && is_literal(next)) {
				this->tokens[curr_idx] = token::product;
				this->name[curr_idx] = '*';
			}
		}
	} //allow_implicit_product

	void ParseString::remove_space() noexcept
	{
		assert(this->tokens.size() == this->name.size());
		this->name.erase(std::remove(this->name.begin(), this->name.end(), ' '), this->name.end());
		this->tokens.erase(std::remove(this->tokens.begin(), this->tokens.end(), token::space), this->tokens.end());
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
				if (in_interval(current, 'a', 'z') || in_interval(current, 'A', 'Z') || current == '_' || current == '\'') {
					tokenized[i] = token::character;
					continue;
				}
				if (in_interval(current, '0', '9') || current == '.') {
					tokenized[i] = token::number;
					continue;
				}

				switch (current) {
				case ',': tokenized[i] = token::comma;  continue;
				case '^': tokenized[i] = token::hat;    continue;
				case '=': tokenized[i] = token::equals; continue;
				case '|': tokenized[i] = token::bar;    continue;
				case ':': tokenized[i] = token::colon;  continue;
				case ' ': tokenized[i] = token::space;  continue;

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
					throw_if<ParseFailure>(--nr_paren < 0, i, "poor grouping, expected matching open parenthesis");
					throw_if<ParseFailure>(name[find_open_par(i, TokenView(tokenized))] != '(', i, "poor grouping, expected matching open parenthesis");
					tokenized[i] = token::clse_grouping;
					continue;
				case ']':
					throw_if<ParseFailure>(--nr_brack < 0, i, "poor grouping, expected matching open bracket");
					throw_if<ParseFailure>(name[find_open_par(i, TokenView(tokenized))] != '[', i, "poor grouping, expected matching open bracket");
					tokenized[i] = token::clse_grouping;
					continue;
				case '}':
					throw_if<ParseFailure>(--nr_brace < 0, i, "poor grouping, expected matching open brace");
					throw_if<ParseFailure>(name[find_open_par(i, TokenView(tokenized))] != '{', i, "poor grouping, expected matching open brace");
					tokenized[i] = token::clse_grouping;
					continue;
				}
				throw ParseFailure{ i, "unexpected character" };	//programm only reaches here if current is not valid.
			}
			throw_if<ParseFailure>(nr_paren != 0, name.length() - 1, "poor grouping, not all parenteses where closed");
			throw_if<ParseFailure>(nr_brack != 0, name.length() - 1, "poor grouping, not all brackets where closed");
			throw_if<ParseFailure>(nr_brace != 0, name.length() - 1, "poor grouping, not all braces where closed");
		}

		if (name.front() == '-') {
			tokenized.front() = token::unary_minus;
		} 
		//change tokens to better matches, decided by also looking at the previous token
		//caution: only test up to second last element, as all matches so far implemented dont exist at the end anyways
		for (std::size_t prev_idx = 0u; prev_idx + 2u < name.length(); prev_idx++) {
			const std::size_t curr_idx = prev_idx + 1u;
			const char prev = tokenized[prev_idx];
			const char curr = tokenized[curr_idx];
			throw_if<ParseFailure>((prev == token::sum || prev == token::product) && (curr == token::sum || curr == token::product),
				curr_idx, "illegal operator sequence");

			//change token representing digits occuring in names to token::character
			if (prev == token::character && curr == token::number) { 
				tokenized[curr_idx] = token::character;
			}
			//change token representing any of "e+-" occuring in numbers to token::number
			else if (prev == token::number && (name[curr_idx] == 'e' || name[curr_idx] == 'E') &&
				(tokenized[curr_idx + 1] == token::sum || tokenized[curr_idx + 1] == token::number)) 
			{
				tokenized[curr_idx] = token::number;
			}
			else if (prev == token::number && curr == token::sum && (name[prev_idx] == 'e' || name[prev_idx] == 'E')) {
				tokenized[curr_idx] = token::number;
			}
			//change unary minus to token::unary_minus
			else if (prev == token::open_grouping && name[curr_idx] == '-') {
				tokenized[curr_idx] = token::unary_minus;
			}
			//change token::unary_minus to part of number, if it is (as unary_minus is not needed to seperate summands anyways)
			else if (prev == token::unary_minus && curr == token::number) {
				tokenized[prev_idx] = token::number;
			}
		}

		return tokenized;
	} //tokenize

	std::size_t find_open_par(std::size_t clsd_par, const TokenView name)
	{
		std::size_t open_par = name.find_last_of(token::open_grouping, clsd_par - 1u);
		clsd_par = name.find_last_of(token::clse_grouping, clsd_par - 1u);
		while (clsd_par > open_par && clsd_par != TokenView::npos) {
			open_par = name.find_last_of(token::open_grouping, open_par - 1u);
			clsd_par = name.find_last_of(token::clse_grouping, clsd_par - 1u);
		}
		return open_par;
	} //find_open_par

	std::size_t find_closed_par(std::size_t open_par, const TokenView name)
	{
		std::size_t clsd_par = name.find_first_of(token::clse_grouping, open_par + 1u);
		open_par = name.find_first_of(token::open_grouping, open_par + 1u);
		while (open_par < clsd_par) {
			clsd_par = name.find_first_of(token::clse_grouping, clsd_par + 1u);
			open_par = name.find_first_of(token::open_grouping, open_par + 1u);
		}
		return clsd_par;
	} //find_closed_par

	std::size_t find_first_of_skip_pars(const TokenView name, const Token token)
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

	std::size_t count_skip_pars(const TokenView name, const Token token)
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

} //namespace bmath::intern