#include <iostream>

#include "termStore.hpp"
#include "termColony.hpp"
#include "arithmeticTerm.hpp"

using namespace bmath::intern;

int main()
{
	TermStore<TermString128> store;
	std::string_view info = "hallo ich bin bruno und ich bin der kameramann.";
	auto head = store.insert_new(TermString128(store, info));

	const auto identity = [](auto& x) -> auto& { return x; };

	std::string output;
	store.at(head).read(store, identity, output);
	std::cout << output << '\n';

	store.at(head).for_each(store, identity, [](char& c) {
		if (c >= 'a' && c <= 'z') {
			c += ('A' - 'a');
		}
	});
	output.clear();
	store.at(head).read(store, identity, output);
	std::cout << output << '\n';
}