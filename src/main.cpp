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
		auto val1_ref = TypedRef(val1_idx, Type::complex);
		auto val2_ref = TypedRef(val2_idx, Type::complex);
		auto sum_idx = term.values.emplace_new(Sum{ {val1_ref, val2_ref} });
		auto sum_ref = TypedRef(sum_idx, Type::sum);
		auto var_idx = insert_string(term.values, "DerWeinachtsmannVomNordpol");
		auto var_ref = TypedRef(var_idx, Type::variable);
		auto product_idx = term.values.emplace_new(Product{ {sum_ref, var_ref} });
		auto product_ref = TypedRef(product_idx, Type::product);

		std::cout << eval(term.values, sum_ref) << std::endl;
		std::string term_str;
		to_string(term.values, product_ref, term_str);
		std::cout << term_str << std::endl;
	}
	{
		TermStore<TypesUnion> store;
		std::string_view info = "ich bin bruno und ich bin der kameramann";
		auto head = insert_string(store, info);
		ToString::apply(store.at(head)).insert_new<ToString>(store, '.');

		std::string output;
		read<ToConstString>(store, head, output);
		std::cout << output << '\n';
		for (auto& c : range<ToString>(store, ToString::apply(store.at(head)))) {
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