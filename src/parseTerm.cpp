
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

	ParseString::ParseString(std::string new_name)
	{
		//all groups of whitespaces are shortened /changed to only a single ' '
		//caution: runs in O(n^2), but input size_ is assumed to be small enough.
		const auto standardize_whitespace = [](std::string& str) {
			for (std::size_t prev_idx = 0; prev_idx + 1 < str.length(); prev_idx++) {
				const std::size_t curr_idx = prev_idx + 1;
				const char prev = str[prev_idx];
				const char curr = str[curr_idx];
				if (std::isspace(curr)) {
					str[curr_idx] = ' ';
					if (prev == ' ') {
						str.erase(prev_idx--, 1);//erase this char -> set prev_idx one back, as string got shorter
					}
				}
			}
		}; //standardize_whitespace

		standardize_whitespace(new_name);
		this->name = std::move(new_name);
		this->tokens = tokenize(name);
	}

	void ParseString::allow_implicit_product() noexcept
	{
		assert(this->tokens.length() == this->name.length());
		for (std::size_t prev_idx = 0; prev_idx + 2 < this->tokens.length(); prev_idx++) {
			const std::size_t curr_idx = prev_idx + 1;
			const Token prev = this->tokens[prev_idx];
			const Token curr = this->tokens[prev_idx + 1];
			const Token next = this->tokens[prev_idx + 2];
			if (is_literal(prev) && curr == ' ' && is_literal(next)) {
				this->tokens[curr_idx] = token::product;
				this->name[curr_idx] = '*';
			}
		}
	} //allow_implicit_product

	void ParseString::remove_space() noexcept
	{
		for (std::size_t i = 0; i < this->size(); i++) {
			if (this->tokens[i] == ' ') {
				assert(this->name[i] == ' ' && "name and tokens represent different data");
				this->name.erase(i, 1);
				this->tokens.erase(i, 1);
				i--;
			}
		}
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
				if (in_interval(current, 'a', 'z') || in_interval(current, 'A', 'Z') || current == '_') {
					tokenized[i] = token::character;
					continue;
				}
				if (in_interval(current, '0', '9') || current == '.') {
					tokenized[i] = token::number;
					continue;
				}

				switch (current) {
				case '^': [[fallthrough]];
				case ',': [[fallthrough]];
				case '!': [[fallthrough]];
				case '?': [[fallthrough]];
				case '=': [[fallthrough]];
				case '|': [[fallthrough]];
				case '&': [[fallthrough]];
				case '<': [[fallthrough]];
				case '>': [[fallthrough]];
				case ' ':
					tokenized[i] = current;
					continue;

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
					throw_if<ParseFailure>(--nr_paren < 0, i, ParseFailure::What::poor_grouping);
					throw_if<ParseFailure>(name[find_open_par(i, TokenView(tokenized))] != '(', i, ParseFailure::What::poor_grouping);
					tokenized[i] = token::clse_grouping;
					continue;
				case ']':
					throw_if<ParseFailure>(--nr_brack < 0, i, ParseFailure::What::poor_grouping);
					throw_if<ParseFailure>(name[find_open_par(i, TokenView(tokenized))] != '[', i, ParseFailure::What::poor_grouping);
					tokenized[i] = token::clse_grouping;
					continue;
				case '}':
					throw_if<ParseFailure>(--nr_brace < 0, i, ParseFailure::What::poor_grouping);
					throw_if<ParseFailure>(name[find_open_par(i, TokenView(tokenized))] != '{', i, ParseFailure::What::poor_grouping);
					tokenized[i] = token::clse_grouping;
					continue;
				}
				throw ParseFailure{ i, ParseFailure::What::illegal_char };	//programm only reaches here if current is not valid.
			}
			throw_if<ParseFailure>(nr_paren != 0, name.length(), ParseFailure::What::poor_grouping);
			throw_if<ParseFailure>(nr_brack != 0, name.length(), ParseFailure::What::poor_grouping);
			throw_if<ParseFailure>(nr_brace != 0, name.length(), ParseFailure::What::poor_grouping);
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
				curr_idx, ParseFailure::What::illegal_ops);

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
			//change token::unary_minus to part of number, if it is
			else if (prev == token::unary_minus && curr == token::number) {
				tokenized[prev_idx] = token::number;
			}
			//change comparison character pairs to their representing tokens
			else if (prev == '<' && curr == '=') {
				tokenized[prev_idx] = token::smaller_equal;
				tokenized[curr_idx] = token::smaller_equal;
			}
			else if (prev == '>' && curr == '=') {
				tokenized[prev_idx] = token::larger_equal;
				tokenized[curr_idx] = token::larger_equal;
			}
			else if (prev == '=' && curr == '=') {
				tokenized[prev_idx] = token::equal;
				tokenized[curr_idx] = token::equal;
			}
		}

		return tokenized;
	} //tokenize

	std::size_t find_open_par(std::size_t clsd_par, const TokenView name)
	{
		std::size_t open_par = name.find_last_of('(', clsd_par - 1u);
		clsd_par = name.find_last_of(')', clsd_par - 1u);
		while (clsd_par > open_par && clsd_par != TokenView::npos) {
			open_par = name.find_last_of('(', open_par - 1u);
			clsd_par = name.find_last_of(')', clsd_par - 1u);
		}
		return open_par;
	} //find_open_par

	std::size_t find_closed_par(std::size_t open_par, const TokenView name)
	{
		std::size_t clsd_par = name.find_first_of(')', open_par + 1u);
		open_par = name.find_first_of('(', open_par + 1u);
		while (open_par < clsd_par) {
			clsd_par = name.find_first_of(')', clsd_par + 1u);
			open_par = name.find_first_of('(', open_par + 1u);
		}
		return clsd_par;
	} //find_closed_par

	std::size_t find_first_of_skip_pars(const TokenView name, const Token token)
	{
		std::size_t open_par = name.find_first_of('(');
		std::size_t after_clsd_par = 0u;
		while (open_par != TokenView::npos) {
			const TokenView search_view(name.substr(after_clsd_par, open_par - after_clsd_par)); //only search "...)here(..."			
			if (const std::size_t found = search_view.find_first_of(token); found != TokenView::npos) {
				return found + after_clsd_par;
			}
			after_clsd_par = find_closed_par(open_par, name) + 1u;
			open_par = name.find_first_of('(', after_clsd_par);
		}
		return name.find_first_of(token, after_clsd_par);
	} //find_first_of_skip_pars

	std::size_t count_skip_pars(const TokenView name, const Token token)
	{
		std::size_t count = 0u;
		std::size_t open_par = name.find_first_of('(');
		std::size_t after_clsd_par = 0u;
		while (open_par != TokenView::npos) {
			const Token* begin = name.data() + after_clsd_par;
			const Token* end = name.data() + open_par; //only search "...)here(..."
			count += std::count(begin, end, token);

			after_clsd_par = find_closed_par(open_par, name) + 1u;
			open_par = name.find_first_of('(', after_clsd_par);
		}
		const Token* begin = name.data() + after_clsd_par;
		const Token* end = name.data() + name.length();
		count += std::count(begin, end, token);
		return count;
	} //count_skip_pars

} //namespace bmath::intern