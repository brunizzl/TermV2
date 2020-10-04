#include <iostream>
#include <charconv>
#include <cassert>

#include "termStore.hpp"
#include "termColony.hpp"
#include "arithmeticTerm.hpp"
#include "parseTerm.hpp"
#include "ioArithmetic.hpp"

using namespace bmath::intern::arithmetic;
using namespace bmath::intern;
using namespace bmath;

enum class PrimaryColor { red, green, blue, COUNT };

struct Color
{
	enum class Name { yellow = static_cast<unsigned>(PrimaryColor::COUNT) + 1u, magenta, cyan } val;
	constexpr Color(PrimaryColor name) :val(static_cast<Name>(name)) {}
	constexpr Color(Name name) : val(name) {}
	constexpr operator Name() const { return this->val; } //implizite convertierung nach Name
	constexpr bool operator==(const Color&) const = default; 
	constexpr bool operator==(PrimaryColor c) const { return static_cast<Name>(c) == this->val; }
};

static_assert(Color(PrimaryColor::red) == Color(PrimaryColor::red));
static_assert(Color(PrimaryColor::red) != Color(PrimaryColor::blue));
static_assert(Color(PrimaryColor::red) == PrimaryColor::red);
static_assert(Color(PrimaryColor::red) != PrimaryColor::blue);

void function1(PrimaryColor c) {
	switch (c) {
	case PrimaryColor::red:	   //nur diese drei sind erlaubt
	case PrimaryColor::green:  //nur diese drei sind erlaubt
	case PrimaryColor::blue:   //nur diese drei sind erlaubt
	//case 3u: <-(error: enum class muss explizit convertiert werden)
	//case Color::Name::cyan: <-(error enum class muss explizit convertiert werden)
		break;
	}
}

void function2(Color c) {
	switch (c) {
	case Color::Name::yellow:		  //alle 6 (wenn auch mit hässlicher syntax) sind erlaubt
	case Color::Name::magenta:		  //alle 6 (wenn auch mit hässlicher syntax) sind erlaubt
	case Color::Name::cyan:			  //alle 6 (wenn auch mit hässlicher syntax) sind erlaubt
	case Color(PrimaryColor::red):	  //alle 6 (wenn auch mit hässlicher syntax) sind erlaubt
	case Color(PrimaryColor::green):  //alle 6 (wenn auch mit hässlicher syntax) sind erlaubt
	case Color(PrimaryColor::blue):	  //alle 6 (wenn auch mit hässlicher syntax) sind erlaubt
	//case 3u: <-(error: enum class muss explizit convertiert werden)
		break;
	}
}

int main_() {
	function1(PrimaryColor::red);
	//function1(Color::Name::cyan); <-(error: kann nicht von Klasse Color zu enum PrimaryColor konvertieren)
	//function1(3u); <-(error: kann nicht implizit von unsigned nach PrimaryColor convertieren)
	function2(PrimaryColor::red);
	function2(Color::Name::cyan);
	//function2(3u); <-(error: kann nicht implizit von unsigned nach Color convertieren)
	return 0;
}

int main()
{
	{
		ShortVector<int, 10> test = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 };
		std::cout << "test: " << test << "\n\n";
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
			"-(b+c)*2i-5*(a+3e*2weinachtsmannVomNordpolUnterWasserWeilKlimawandel)",
			"loge(2)*herbert(20e-10, a 2, 6anneliese(fred, marko * 4))/5",
			"1/5*herbert(20e-10, a 2, 6anneliese(fred, marko * 4))",
			"sin(1) + 3 + sin(3) + fred + 1 + sin(7) - hans + jens + herbert + 7 + anneliese + fred + sin(3) + marco + 3 + bernd",
			"1/5*herbert(3)",
			"a+ln(b^2)+ln(c)+2-b^2-c*a",
			"c*d+g*f+g",
			"12*herbert+herbert+4",
			"(3*x-2*y)/5",
			"(1*[2i^(-2)*3*(4*(a^5))])",
			"(10/5)^3",
			"5+pi+7/(5-a+ln[2])^3",
			"auto^herbert*3+auto^(-32*a)-4",
			"3*(sin(a+b+c)^2+cos(a+b+c)^2+4)+i",
			"(3^(x^2))^(x)",
			"sin(-a*b)",
			"sin(x)*b+3*b",
			"a*d+a*b*c",
			"a/(a*b)",
			"4*a+9*(a^2)",
		};
		for (auto& term_name : term_names) {
			std::cout << "-------------------------------------------------------------------------------------\n";

			try {
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
}
