#include <iostream>
#include <charconv>
#include <cassert>
#include <numeric>

#include "termStore.hpp"
#include "termColony.hpp"
#include "arithmeticTerm.hpp"
#include "parseTerm.hpp"
#include "ioArithmetic.hpp"

using namespace bmath::intern;
using namespace bmath;


enum class E1 { a, b, c, COUNT };
enum class E2 { d, e, f, COUNT };

using E = SumEnum<E1, E2>;

void f(E e)
{
	switch (e) {
	case E(E1::a): std::cout << "a\n"; break;
	case E(E1::b): std::cout << "b\n"; break;
	case E(E1::c): std::cout << "c\n"; break;
	case E(E2::d): std::cout << "d\n"; break;
	case E(E2::e): std::cout << "e\n"; break;
	case E(E2::f): std::cout << "f\n"; break;
	default: std::cout << "upsie!\n"; break;
	}
}

int main()
{
	{
		ShortVector<int, 10> test = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 };
		std::cout << "test: " << test << "\n\n";
		f(E1::a);
		f(E1::b);
		f(E1::c);
		f(E2::d);	
		f(E2::e);	
		f(E2::f);	
		std::cout << "E::COUNT = " << unsigned(E::COUNT) << "\n\n";
	}
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
		std::vector<std::string> term_names = {
			" -(b'+c)*2*i-5*(a+3 e 2 weinachtsmannVomNordpolUnterWasserWeilKlimawandel)",
			"1/5*herbert(20e-10, 3 a, 6 anneliese(fred, marko * 4))",
			"sin(1) + 3 + sin(3) + fred + 1 + sin(7) - hans + jens + herbert + 7 + anneliese + fred + sin(3) + marco + 3 + bernd",
			"a+ln(b^2)+ln(c)+2-b^2-c*a",
			"c*d+g*f+g",
			"12*herbert+herbert+4",
			"(3*x-2*y)/5",
			"(1*[2i^(-2)*3*(4*(a^5))])",
			"(10/5)^3",
			"auto^herbert*3+auto^(-32*a)-4",
			"3*(sin(a+b+c)^2+cos(a+b+c)^2+4)+i",
			"(3^(x^2))^(x)",
			"sin(-a*b)",
			"sin(x)*b+3*b",
			"a*d+a*b*c",
			"a/(a b)",
		};
		for (auto& term_name : term_names) {
			std::cout << "-------------------------------------------------------------------------------------\n";

			try {
				std::cout << "baue aus string: \"" << term_name << "\"\n\n";

				bmath::ArithmeticTerm term(term_name);

				std::cout << "nach bau: \n" << term.to_string() << "\n\n";
				//std::cout << "speicher nach bau:\n" << term.show_memory_layout() << "\n\n";

				term.combine_layers();
				term.sort();
				term.combine_values_unexact();

				std::cout << "nach vereinfachen in huebsch: \n" << term.to_pretty_string() << "\n\n";
				std::cout << "speicher nach vereinfachen:\n" << term.show_memory_layout() << "\n\n\n";
			}
			catch (ParseFailure failure) {
				std::cout << failure.what << '\n';
				std::cout << term_name << '\n';
				std::cout << std::string(failure.where, ' ') << "^\n\n";
			}
		}
	}
}
