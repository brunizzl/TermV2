
#include <cassert>
#include <algorithm>

#include "parseTerm.hpp"
#include "termUtility.hpp"

namespace bmath::intern {

	void remove_whitspace(std::string& str)
	{
		for (std::size_t i = 0; i < str.length(); i++) {
			if (std::isspace(str[i])) {
				str.erase(i--, 1);	//erase this char -> set i one back, as string got shorter
			}
		}
	} //remove_whitspace

	TokenString tokenize(const std::string_view name)
	{
		const auto in_interval = [](const char test, const char lower, const char upper) {
			return test >= lower && test <= upper;
		};

		TokenString tokenized(name.length(), '\0');
		{
			int nr_paren = 0;	//counts number of '(' minus number of ')', may never be negative 
			int nr_brack = 0;	//counts number of '[' minus number of ']', may never be negative
			int nr_brace = 0;	//counts number of '{' minus number of '}', may never be negative
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
				case '>':
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
		for (std::size_t last_idx = 0u; last_idx + 2u < name.length(); last_idx++) {
			const std::size_t curr_idx = last_idx + 1u;
			const char last = tokenized[last_idx];
			const char curr = tokenized[curr_idx];
			throw_if<ParseFailure>((last == token::sum || last == token::product) && (curr == token::sum || curr == token::product),
				curr_idx, ParseFailure::What::illegal_ops);

			//change token representing digits occuring in names to token::character
			if (last == token::character && curr == token::number) { 
				tokenized[curr_idx] = token::character;
			}
			//change token representing any of "e+-" occuring in numbers to token::number
			else if (last == token::number && (name[curr_idx] == 'e' || name[curr_idx] == 'E') &&
				(tokenized[curr_idx + 1] == token::sum || tokenized[curr_idx + 1] == token::number)) 
			{
				tokenized[curr_idx] = token::number;
			}
			else if (last == token::number && curr == token::sum && (name[last_idx] == 'e' || name[last_idx] == 'E')) {
				tokenized[curr_idx] = token::number;
			}
			//change unary minus to token::unary_minus
			else if (last == token::open_grouping && name[curr_idx] == '-') {
				tokenized[curr_idx] = token::unary_minus;
			}
			//change token::unary_minus to part of number, if it is
			else if (last == token::unary_minus && curr == token::number) {
				tokenized[last_idx] = token::number;
			}
			//change comparison character pairs to their representing tokens
			else if (last == '<' && curr == '=') {
				tokenized[last_idx] = token::smaller_equal;
				tokenized[curr_idx] = token::smaller_equal;
			}
			else if (last == '>' && curr == '=') {
				tokenized[last_idx] = token::larger_equal;
				tokenized[curr_idx] = token::larger_equal;
			}
			else if (last == '=' && curr == '=') {
				tokenized[last_idx] = token::equal;
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