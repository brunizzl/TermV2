#include <iostream>
#include <charconv>
#include <cassert>

#include "termStore.hpp"
#include "termColony.hpp"
#include "arithmeticTerm.hpp"
#include "parseTerm.hpp"
#include "parseArithmetic.hpp"

using namespace bmath::in::arm;
using namespace bmath::in;
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
		std::string term_name = "-(b+c)*2i-5*(a+3e*2weinachtsmannVomNordpolUnterWasserWeilKlimawandel)";
		//std::string term_name = "loge(2)*herbert(20e-10, a 2, 6anneliese(fred, marko * 4))/5";
		//std::string term_name = "log2(8)";
		try {
			bmath::ArithmeticTerm term(std::move(term_name));

			std::string term_str;
			append_to_string(term.store, term.head, term_str);
			std::cout << "to_string: \n" << term_str << std::endl;
			std::cout << "speicher nach bau:\n" << term.show_memory_layout() << '\n';

			flatten_variadic(term.store, term.head);
			if (auto val = combine_values_unexact(term.store, term.head)) {
				term.head = TypedIdx(term.store.insert(*val), Type::complex);
			}	
			//free_tree(term.store, term.head);
			//term.head = TypedIdx(term.store.insert(Complex(1337.0, 0.0)), Type::complex);

			term_str.clear();
			append_to_string(term.store, term.head, term_str);
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
		auto head_idx = insert_string(term.store, "ich bin bruno und ich bin der kameramann.");
		term.head = TypedIdx(head_idx, Type::variable);
		auto idx2 = insert_string(term.store, " hi :) ");
		auto idx3 = insert_string(term.store, " naaa ;) ");
		auto idx4 = insert_string(term.store, " heute schon was vor? :o ");
		auto idx5 = insert_string(term.store, " jetzt schon ;)");
		append<ToString>(term.store, head_idx, idx2);
		append<ToString>(term.store, head_idx, idx3);
		append<ToString>(term.store, head_idx, idx4);
		append<ToString>(term.store, head_idx, idx5);
		std::cout << term.show_memory_layout() << '\n';
		compact<ToString>(term.store, head_idx);
		std::cout << term.show_memory_layout() << '\n';
	}
}