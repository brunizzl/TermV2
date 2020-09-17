#include <iostream>

#include "termStore.hpp"
#include "termColony.hpp"
#include "arithmeticTerm.hpp"

using namespace bmath::intern::arithmetic;
using namespace bmath::intern;

int main()
{
	{
		bmath::ArithmeticTerm term;
		auto val1_idx = term.values.emplace_new(Complex{ {3.0, 0.0} });
		auto val2_idx = term.values.emplace_new(Complex{ {12.0, 0.0} });
		auto val1_ref = TypedIdx(val1_idx, Type::complex);
		auto val2_ref = TypedIdx(val2_idx, Type::complex);
		auto sum_idx = term.values.emplace_new(Sum{ {val1_ref, val2_ref} });
		auto sum_ref = TypedIdx(sum_idx, Type::sum);
		auto var_idx = insert_string(term.values, "DerWeinachtsmannVomNordpol");
		auto var_ref = TypedIdx(var_idx, Type::variable);
		auto product_idx = term.values.emplace_new(Product{ {sum_ref, var_ref} });
		auto product_ref = TypedIdx(product_idx, Type::product);
		for (int i = 0; i < 10; i++) {
			std::string term_str;
			to_string(term.values, product_ref, term_str);
			std::cout << term_str << std::endl;
			insert_new<ToProduct, Product>(term.values, product_idx, val1_ref);
		}

		std::string term_str;
		to_string(term.values, product_ref, term_str);
		std::cout << term_str << std::endl;
		std::cout << eval(term.values, sum_ref) << std::endl;
	}
	{
		TermStore<TypesUnion> store;
		std::string_view info = "ich bin bruno und ich bin der kameramann";
		auto head = insert_string(store, info);
		insert_new<ToString, TermString128>(store, head, '.');

		std::string output;
		read<ToConstString>(store, head, output);
		std::cout << output << '\n';
		for (auto& c : range<ToString>(store, head)) {
			if (c % 2 == 0) {
				c = '\0';
			}
		}
		sort<ToString>(store, ToString::apply(store.at(head)), [](auto& c1, auto& c2) { return c1 < c2; });

		output.clear();
		read<ToConstString>(store, head, output);
		std::cout << output << '\n';
	}
}