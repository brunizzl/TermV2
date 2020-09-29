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
		std::string term_name = "loge(2)*herbert(20e-10, a 2, 6anneliese(fred, marko * 4))/5";
		//std::string term_name = "log2(8)";
		try {
			auto str = ParseString(std::move(term_name));
			allow_implicit_product(str);
			remove_space(str);
			assert(find_first_not_arithmetic(TokenView(str.tokens)) == TokenView::npos);
			bmath::ArithmeticTerm term;
			term.head = build(term.values, str);

			std::string term_str;
			to_string(term.values, term.head, term_str);
			std::cout << "to_string: \n" << term_str << std::endl;
			std::cout << "speicher nach bau:\n" << term.show_memory_layout() << '\n';

			//flatten_variadic(term.values, term.head);
			//if (auto val = combine_values_unexact(term.values, term.head)) {
			//	term.head = TypedIdx(term.values.insert(*val), Type::complex);
			//}	
			free_tree(term.values, term.head);
			term.head = TypedIdx(term.values.insert(Complex(1337.0, 0.0)), Type::complex);

			term_str.clear();
			to_string(term.values, term.head, term_str);
			std::cout << "to_string nach vereinfachen: \n" << term_str << std::endl;
			std::cout << "speicher nach vereinfachen:\n" << term.show_memory_layout() << '\n';
			std::cout << "\n\n";
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
	{
		bmath::ArithmeticTerm term;
		auto head_idx = insert_string(term.values, "ich bin bruno und ich bin der kameramann.");
		term.head = TypedIdx(head_idx, Type::variable);
		std::cout << "speicher nach bau:\n" << term.show_memory_layout() << '\n';
	}
}