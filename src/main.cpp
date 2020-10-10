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
enum class E2 { a, b, c, COUNT };
using E12 = SumEnum<E1, E2>;

void f1(E12 e)
{
	std::cout << unsigned(e) << "\t";
	if (e.is<E1>()) {
		std::cout << "E1\n";
	}
	else if (e.is<E2>()) {
		std::cout << "E2\n";
	}
	else {
		std::cout << "upsi!\n";
	}
}

void f2(E12 e)
{
	switch (e) {
	case E12(E1::a): std::cout << "E1::a\n"; break;
	case E12(E1::b): std::cout << "E1::b\n"; break;
	case E12(E1::c): std::cout << "E1::c\n"; break;
	case E12(E1::COUNT): std::cout << "E1::COUNT\n"; break;
	case E12(E2::a): std::cout << "E2::a\n"; break;
	case E12(E2::b): std::cout << "E2::b\n"; break;
	case E12(E2::c): std::cout << "E2::c\n"; break;
	case E12(E2::COUNT): std::cout << "E2::COUNT\n"; break;
	default: std::cout << "upsi!\n"; break;
	}
}

enum class E3 { a, b, c, COUNT };
using E123 = SumEnum<E12, E3>;

void f3(E123 e)
{
	std::cout << unsigned(e) << "\t";
	if (e.is<E1>()) {
		std::cout << "E1";
	}
	else if (e.is<E2>()) {
		std::cout << "E2";
	}
	else if (e.is<E3>()) {
		std::cout << "E3";
	}
	else {
		std::cout << "upsi!";
	}

	if (e.is<E12>()) {
		std::cout << "\t -> ";
		f2(e.operator E12());
	}
	else {
		std::cout << "\n";
	}
}


int main()
{
	{
		std::cout << "E::COUNT = " << unsigned(E12::COUNT) << "\n";
		f1(E1::a);
		f1(E1::b);
		f1(E1::c);
		f1(E1::COUNT);
		f1(E2::a);
		f1(E2::b);
		f1(E2::c);
		f1(E2::COUNT);
		std::cout << "\n";
		f2(E1::a);
		f2(E1::b);
		f2(E1::c);
		f2(E1::COUNT);
		f2(E2::a);
		f2(E2::b);
		f2(E2::c);
		f2(E2::COUNT);
		std::cout << "\n";
		f3(E1::a);
		f3(E1::b);
		f3(E1::c);
		f3(E1::COUNT);
		f3(E2::a);
		f3(E2::b);
		f3(E2::c);
		f3(E2::COUNT);
		f3(E3::a);
		f3(E3::b);
		f3(E3::c);
		f3(E3::COUNT);
		std::cout << "\n";
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
				term.combine_values_exact();

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
	{
		using namespace bmath::intern::pattern;
		std::string s = "a, b | a^2 + 2 a b + b^2 = (a + b)^2";
		//std::string s = "cos('pi') = -1";
		const PnTerm term(s);
		std::cout << "pattern: " << term.to_string() << "\n\n";
		std::cout << "lhs speicher:\n" << term.lhs_memory_layout() << "\n\n";
		std::cout << "rhs speicher:\n" << term.rhs_memory_layout() << "\n\n";
	}
}
