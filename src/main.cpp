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
using namespace bmath;

int main()
{
	{
		std::string parentheses = "1([[0]])2(0)[(0){()(0)}]({}{}{()})3";
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
		//std::string term_name = "loge(2)*herbert(20e-10, a 2, 6anneliese(fred, marko * 4))/5";
		std::string term_name = "sin(1) + 3 + sin(3) + fred + 1 + sin(7) - hans + jens + herbert + 7 + anneliese + fred + sin(3) + marco + 3 + bernd";
		try {
			bmath::ArithmeticTerm term(std::move(term_name));
			
			std::cout << "to_string: \n" << term.to_string() << "\n\n";
			std::cout << "speicher nach bau:\n" << term.show_memory_layout() << "\n\n";

			term.flatten_variadic();
			term.sort();
			//term.combine_values_unexact();

			std::cout << "to_string nach vereinfachen: \n" << term.to_string() << "\n\n";
			std::cout << "speicher nach vereinfachen:\n" << term.show_memory_layout() << "\n\n\n";
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
	}
}