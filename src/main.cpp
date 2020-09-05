#include <iostream>

#include "termStore.hpp"
#include "termColony.hpp"
#include "arithmeticTerm.hpp"

using namespace bmath::intern::arithmetic;
using namespace bmath::intern;

int main()
{
	bmath::ArithmeticTerm term;
	auto val1_idx = term.values.insert_new(Complex{ {3.0, 0.0} });
	auto val2_idx = term.values.insert_new(Integer{ {12, 0, 0} });
	auto val1_ref = TypedRef(val1_idx, Type::complex);
	auto val2_ref = TypedRef(val2_idx, Type::integer);
	auto sum_idx = term.values.insert_new(Sum{ {val1_ref, val2_ref} });
	auto sum_ref = TypedRef(sum_idx, Type::sum);
	std::cout << eval(term.values, sum_ref) << std::endl;

	TermStore<TermString128> store;
	std::string_view info = "hallo ich bin bruno und ich bin der kameramann.";
	auto head = store.insert_new(TermString128(store, info));

	const auto identity = [](auto& x) -> auto& { return x; };

	std::string output;
	store.at(head).read(store, identity, output);
	std::cout << output << '\n';
	
	for (auto& c : make_iterable(store, store.at(head), identity)) {
		if (c >= 'a' && c <= 'z') {
			c += ('A' - 'a');
		}
	}

	output.clear();
	store.at(head).read(store, identity, output);
	std::cout << output << '\n';
}