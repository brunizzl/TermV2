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

enum class A1 { a, b, c, COUNT };
enum class A2 { a, b, c, COUNT };
using A12 = SumEnum<A1, A2>;

void f1(A12 e)
{
	bool hit = false;
	if (e.is<A1>()) { std::cout << "A1 \t"; hit = true; }
	if (e.is<A2>()) { std::cout << "A2 \t"; hit = true; }
	std::cout << (hit ? "\n" : "upsi!\n");
}

enum class A3 { a, b, c, COUNT };
using A123 = SumEnum<A12, A3>;

void f2(A123 e)
{
	bool hit = false;
	if                (e.is<A3>()) { std::cout << "A3 \t";        hit = true; }
	if (e.is<A12>() && e.is<A1>()) { std::cout << "A12 && A1 \t"; hit = true; }
	if (e.is<A12>() && e.is<A2>()) { std::cout << "A12 && A2 \t"; hit = true; }
	std::cout << (hit ? "\n" : "upsi!\n");
}

enum class B1 { a, b, c, COUNT };
enum class B2 { a, b, c, COUNT };
using BB1 = SumEnum<B1>;
using B12B1 = SumEnum<BB1, B2, B1>;

void g1(B12B1 e)
{
	bool hit = false;
	if (e.is<B1>())  { std::cout << "B1 \t";  hit = true; }
	if (e.is<B2>())  { std::cout << "B2 \t";  hit = true; }
	if (e.is<BB1>()) { std::cout << "BB1 \t"; hit = true; }
	std::cout << (hit ? "\n" : "upsi!\n");
}

void cmp(double a, double b)
{
	std::cout << (a == b) << "  " << a - b << "\n";
}

int main()
{
	{
		cmp(std::pow(7.0, 14.0), 678223072849.0);
		cmp(std::pow(875.0, 3.0), 669921875.0);
		cmp(std::pow(754.0, 4.0), 323210442256.0);
		cmp(std::pow(9.0, 9.0), 387420489.0);
		f1(A1::a);
		f1(A1::b);
		f1(A1::c);
		f1(A2::a);
		f1(A2::b);
		f1(A2::c);
		std::cout << "\n";
		f2(A1::a);
		f2(A1::b);
		f2(A1::c);
		f2(A2::a);
		f2(A2::b);
		f2(A2::c);
		f2(A3::a);
		f2(A3::b);
		f2(A3::c);
		std::cout << "\n";
		g1(B1::a);
		g1(B1::b);
		g1(B1::c);
		g1(B2::a);
		g1(B2::b);
		g1(B2::c);
		g1(BB1(B1::a));
		g1(BB1(B1::b));
		g1(BB1(B1::c));
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
			"1+3*4-6",
			"a * (7 ^ 14 + 9)",
			"2.2 + 4",
			"1e-2 + 5",
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
		std::cout << "-------------------------------------------------------------------------------------\n";
		using namespace bmath::intern::pattern;
		std::string s = "a , b | a^2 + 2 a b + b^2 = (a + b)^2";
		//std::string s = "as :sum,    | -as  =     sum{ -a  | a <- as }";
		//std::string s = "as :product | 1/as = product{ 1/a | a <- as }";
		//std::string s = "cos('pi') = -1";
		const PnTerm pattern(s);
		std::cout << "pattern: " << pattern.to_string() << "\n\n";
		std::cout << "lhs speicher:\n" << pattern.lhs_memory_layout() << "\n\n";
		std::cout << "rhs speicher:\n" << pattern.rhs_memory_layout() << "\n\n";
	}
}
