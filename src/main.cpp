#include <iostream>

#include "termStore.hpp"
#include "termColony.hpp"
#include "arithmeticTerm.hpp"

using namespace bmath::intern::arithmetic;
using namespace bmath::intern;

struct Chars
{
	char chars1[2];
	char chars2[2];
};

int main()
{
	{
		Chars chars;
		for (int i = 0; i < 4; i++) {
			chars.chars1[i] = 'c';
		}
		for (int i = 0; i < 4; i++) {
			std::cout << chars.chars1[i];
		}
		std::cout << '\n';
	}
	{
		bmath::ArithmeticTerm term;
		auto val1_idx = term.values.emplace_new(Complex{ {3.0, 0.0} });
		auto val2_idx = term.values.emplace_new(Complex{ {12.0, 0.0} });
		auto val1_ref = TypedRef(val1_idx, Type::complex);
		auto val2_ref = TypedRef(val2_idx, Type::complex);
		auto sum_idx = term.values.emplace_new(Sum());
		term.values.at(sum_idx).sum.values[0] = val1_ref;
		term.values.at(sum_idx).sum.values[0] = val2_ref;
		auto sum_ref = TypedRef(sum_idx, Type::sum);
		std::cout << eval(term.values, sum_ref) << std::endl;
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
			if (c == 'a' || c == 'o' || c == 'i' || c == 'e' || c == 'u') {
				c = 'o';
			}
		}

		output.clear();
		read<ToConstString>(store, head, output);
		std::cout << output << '\n';
	}
}