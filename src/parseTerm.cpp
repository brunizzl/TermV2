
#include <cassert>
#include <charconv>

#include "parseTerm.hpp"
#include "termUtility.hpp"

namespace bmath::intern {

	void remove_whitspace(std::string& str)
	{
		for (std::size_t i = 0; i < str.length(); i++) {	//deleting whitespace and counting parentheses
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
			for (std::size_t i = 0; i < name.length(); i++) {
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
		for (std::size_t last_idx = 0; last_idx + 1 < name.length(); last_idx++) {
			const std::size_t curr_idx = last_idx + 1;
			const char last = tokenized[last_idx];
			const char curr = tokenized[curr_idx];
			throw_if<ParseFailure>((last == token::sum || last == token::product) && (curr == token::sum || curr == token::product),
				curr_idx, ParseFailure::What::illegal_ops);

			//change token representing digits occuring in names to token::character
			if (last == token::character && curr == token::number) { 
				tokenized[curr_idx] = token::character;
			}
			//change token representing any of "e+-" occuring in numbers to token::number
			else if (last == token::number && (name[curr_idx] == 'e' || name[curr_idx] == 'E')) {
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
		std::size_t open_par = name.find_last_of('(', clsd_par - 1);
		clsd_par = name.find_last_of(')', clsd_par - 1);
		while (clsd_par > open_par && clsd_par != TokenView::npos) {
			open_par = name.find_last_of('(', open_par - 1);
			clsd_par = name.find_last_of(')', clsd_par - 1);
		}
		return open_par;
	} //find_open_par

	std::size_t find_closed_par(std::size_t open_par, const TokenView name)
	{
		std::size_t clsd_par = name.find_first_of(')', open_par + 1);
		open_par = name.find_first_of('(', open_par + 1);
		while (open_par < clsd_par) {
			clsd_par = name.find_first_of(')', clsd_par + 1);
			open_par = name.find_first_of('(', open_par + 1);
		}
		return clsd_par;
	} //find_closed_par

	std::size_t find_last_of_skip_pars(const TokenView name, const char character)
	{
		std::size_t open_par = name.length() - 1;
		std::size_t clsd_par = name.find_last_of(')');
		while (clsd_par != TokenView::npos) {
			const TokenView search_view(name.data() + clsd_par, open_par - clsd_par);
			const std::size_t found = search_view.find_last_of(character);
			if (found != TokenView::npos) {
				return found + clsd_par;	//search_view starts with offset of clsd_par (two lines above). this offset has to be added to get distance from begin of name
			}
			else {
				open_par = find_open_par(clsd_par, name);
				clsd_par = name.find_last_of(')', open_par);	//one could start the search with an offset of one, only to have an underflow of open_par == 0
			}
		}
		return name.find_last_of(character, open_par);
	} //find_last_of_skip_pars (single char)

	std::size_t find_first_of_skip_pars(const TokenView name, const char character)
	{
		std::size_t open_par = name.find_first_of('(');
		std::size_t clsd_par = -1;
		while (open_par != TokenView::npos) {
			const TokenView search_view(name.substr(clsd_par + 1, open_par - clsd_par - 1)); //only "...)here(..."
			const std::size_t found = search_view.find_first_of(character);
			if (found != TokenView::npos) {
				return found + clsd_par;
			}
			else {
				clsd_par = find_closed_par(open_par, name);
				open_par = name.find_first_of('(', clsd_par);
			}
		}
		return name.find_first_of(character, clsd_par == TokenView::npos ? 0 : clsd_par);
	} //find_first_of_skip_pars (single char)



	namespace arithmetic {

		bool is_arithmetic(const TokenView view)
		{
			using namespace token;
			const char allowed_tokens[] = 
			{ character, number, open_grouping, clse_grouping, unary_minus, sum, product, ',', '^', '\0' };
			return view.find_first_not_of(allowed_tokens) == TokenView::npos;
		}

		void allow_implicit_product(TokenString& tokens, std::string& name)
		{
			assert(tokens.length() == name.length());
			auto tokens_iter = tokens.begin();
			auto name_iter = name.begin();
			for (; std::next(tokens_iter) != tokens.end(); tokens_iter++, name_iter++) {
				if (*tokens_iter == token::number && *std::next(tokens_iter) == token::character) {
					tokens_iter = tokens.insert(std::next(tokens_iter), token::product);
					name_iter = name.insert(std::next(name_iter), '*');
				}
			}
		}

		Head find_head_type(const TokenView token_view, std::size_t offset)
		{
			std::size_t op;
			if ((op = find_first_of_skip_pars(token_view, token::sum)) != TokenView::npos) {
				return Head{ op, Head::Type::sum };
			}
			if (token_view.front() == token::unary_minus) {
				return Head{ 0, Head::Type::negate };
			}
			if ((op = find_first_of_skip_pars(token_view, token::product)) != TokenView::npos) {
				return Head{ op, Head::Type::product };
			}
			if ((op = find_first_of_skip_pars(token_view, '^')) != TokenView::npos) {
				return Head{ op, Head::Type::power };
			}
			if (token_view.find_first_not_of(token::number) == TokenView::npos) {
				return Head{ 0, Head::Type::value };
			}
			if ((op = token_view.find_first_not_of(token::character)) == TokenView::npos) {
				return Head{ 0, Head::Type::variable };
			}
			throw_if<ParseFailure>(token_view[op] != '(', op + offset, ParseFailure::What::illegal_char);
			throw_if<ParseFailure>(!token_view.ends_with(')'), token_view.length() + offset, ParseFailure::What::poor_grouping);
			if (op == 0) {
				return Head{ 0, Head::Type::group };
			}
			else {
				return Head{ op, Head::Type::function };
			}
		}

		TypedIdx build_number(ArithmeticStore& store, double re, double im)
		{
			return TypedIdx(store.emplace_new(Complex{ std::complex<double>(re, im) }), Type::complex);
		}

		TypedIdx build(ArithmeticStore& store, ParseView view, std::size_t offset)
		{
			throw_if<ParseFailure>(view.size() == 0, offset, ParseFailure::What::illegal_ops);
			Head head = find_head_type(view.tokens, offset);
			while (head.type == Head::Type::group) {
				view.remove_prefix(1);
				view.remove_suffix(1);
				head = find_head_type(view.tokens, offset);
			}
			switch (head.type) {
			case Head::Type::sum: {
				std::size_t sum_idx;
				{	//first summand is not guaranteed to start with '+' or '-'
					const auto summand_view = view.substr(0, head.where);
					view.remove_prefix(head.where);  //leave next operator as first symbol
					const TypedIdx summand = build(store, summand_view, offset);
					offset += head.where;
					sum_idx = store.emplace_new(Sum(summand));
				}
				while (view.size()) {
					const char current_sign = view.chars[0];
					view.remove_prefix(1);  //remove sign of current summand
					const std::size_t next_op_idx = find_first_of_skip_pars(view.tokens, token::sum);
					const auto summand_view = view.substr(0, next_op_idx);
					view.remove_prefix(next_op_idx == TokenView::npos ? view.size() : next_op_idx);
					const TypedIdx summand = build(store, summand_view, offset);
					offset += next_op_idx + 1;
					switch (current_sign) {
					case '+':
						insert_new<ToSum>(store, sum_idx, summand);
						break;
					case '-':
					{
						const TypedIdx minus_1 = build_number(store, -1.0);
						const TypedIdx product = TypedIdx(store.emplace_new(Product({ summand, minus_1 })), Type::product);
						insert_new<ToSum>(store, sum_idx, product);
					} break;
					default: assert(false);
					}
				}
				return TypedIdx(sum_idx, Type::sum);
			} break;
			case Head::Type::negate: {
				view.remove_prefix(1);  //remove minus sign
				const TypedIdx to_negate = build(store, view, offset + 1);
				const TypedIdx minus_1 = build_number(store, -1.0);
				return TypedIdx(store.emplace_new(Product({ to_negate, minus_1 })), Type::product);
			} break;
			case Head::Type::product: {
				std::size_t product_idx;
				{
					const auto factor_view = view.substr(0, head.where);
					view.remove_prefix(head.where);  //leave next operator as first symbol
					const TypedIdx factor = build(store, factor_view, offset);
					offset += head.where;
					product_idx = store.emplace_new(Product(factor));
				}
				while (view.size()) {
					const char current_operation = view.chars[0];
					view.remove_prefix(1);  //remove '*' or '/' of current factor
					const std::size_t next_op_idx = find_first_of_skip_pars(view.tokens, token::product);
					const auto factor_view = view.substr(0, next_op_idx);
					view.remove_prefix(next_op_idx == TokenView::npos ? view.size() : next_op_idx);
					const TypedIdx factor = build(store, factor_view, offset);
					offset += next_op_idx + 1;
					switch (current_operation) {
					case '*':
						insert_new<ToProduct>(store, product_idx, factor);
						break;
					case '/':
					{
						const TypedIdx minus_1 = build_number(store, -1.0);
						const TypedIdx to_minus_1 = TypedIdx(store.emplace_new(KnownFunction{FunctionType::pow, factor, minus_1, TypedIdx()}), Type::known_function);
						insert_new<ToProduct>(store, product_idx, to_minus_1);
					} break;
					default: assert(false);
					}
				}
				return TypedIdx(product_idx, Type::product);
			} break;
			case Head::Type::power: {
				const auto base_view = view.substr(0, head.where);
				view.remove_prefix(head.where + 1);
				const TypedIdx base = build(store, base_view, offset);
				const TypedIdx expo = build(store, view, offset + head.where + 1);
				return TypedIdx(store.emplace_new(KnownFunction{FunctionType::pow, base, expo, TypedIdx()}), Type::known_function);
			} break;
			case Head::Type::value: {
				double val;
				const auto [ptr, error] = std::from_chars(view.chars, view.chars + view.size(), val);
				throw_if<ParseFailure>(error == std::errc::invalid_argument, offset, ParseFailure::What::illformed_val);
				throw_if<ParseFailure>(ptr != view.chars + view.size(), std::size_t(offset + ptr - view.chars + 1), ParseFailure::What::illformed_val);
				return build_number(store, val);
			} break;
			case Head::Type::variable:
				if (view.chars[0] == 'i' && view.size() == 1) {
					return build_number(store, 0.0, 1.0);
				}
				else {
					return TypedIdx(insert_string(store, std::string_view(view.chars, view.size())), Type::variable);
				}
			}
		}

	} //namespace arithmetic

} //namespace bmath::intern