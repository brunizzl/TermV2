#include <iostream>
#include <charconv>
#include <cassert>

#include "termStore.hpp"
#include "termColony.hpp"
#include "arithmeticTerm.hpp"
#include "parseTerm.hpp"
#include "parseArithmetic.hpp"

using namespace bmath::intern::arithmetic;
using namespace bmath::intern;

int main()
{
	{
		std::string parentheses = "1([[.]])2(.)[(.){()(.)}]({}{}{()})3";
		TokenString tokens = tokenize(parentheses);
		std::size_t closed = parentheses.find_last_of(')');
		std::size_t open = find_open_par(closed, TokenView(tokens));
		std::cout << parentheses << '\n';
		std::cout << std::string(open, ' ') << "^open\n";
		std::cout << std::string(closed, ' ') << "^closed\n";
		std::cout << "counted " << count_skip_pars(TokenView(tokens), token::number) << " unenclosed instances of token::number\n\n";
	}
	{
		//std::string term_name = "-(b+c)*2i-5*(a+3e*2weinachtsmannVomNordpolUnterWasserWeilKlimawandel)";
		std::string term_name = "herbert(20, 2+2, anneliese(fred, marko * 4))";
		try {
			remove_whitspace(term_name);
			auto tokens = tokenize(term_name);
			allow_implicit_product(tokens, term_name);
			assert(find_first_not_arithmetic(TokenView(tokens)) == TokenView::npos);
			Store term(term_name.size());
			auto head = build(term, {tokens, term_name});

			std::string term_str;
			to_string(term, head, term_str);
			std::cout << "to_string: " << term_str << std::endl;
			std::cout << bmath::ArithmeticTerm{ head, term }.show_memory_layout() << '\n';
		}
		catch (ParseFailure failure) {
			switch (failure.what) {
			case ParseFailure::What::illegal_char:
				std::cout << "error while building: encountered illegal char:\n";
				break;
			case ParseFailure::What::poor_grouping:
				std::cout << "error while building: encountered poor grouping:\n";
				break;
			case ParseFailure::What::illegal_ops:
				std::cout << "error while building: encountered illegal operator placement:\n";
				break;
			case ParseFailure::What::illformed_val:
				std::cout << "error while building: encountered illformed value:\n";
				break;
			case ParseFailure::What::wrong_param_count:
				std::cout << "error while building: encountered wrong parameter count:\n";
				break;
			}
			std::cout << term_name << '\n';
			std::cout << std::string(failure.where, ' ') << "^\n\n";
		}

		double result;
		std::from_chars(term_name.data(), term_name.data() + term_name.size(), result);
		std::cout << result << '\n';
	}
	{
		TermStore<TypesUnion> store;
		std::string_view info = "ich bin bruno und ich bin der kameramann";
		std::uint32_t head = insert_string(store, info);
		insert_new<ToString>(store, head, '.');

		std::string output;
		read<ToConstString>(store, head, output);
		std::cout << output << '\n';
		for (auto& c : range<ToString>(store, head)) {
			if (c % 2 == 0) {
				c = '\0';
			}
		}
		sort<ToString>(store, head, [](auto& c1, auto& c2) { return c1 < c2; });

		output.clear();
		read<ToConstString>(store, head, output);
		std::cout << output << '\n';
	}
}