#include <iostream>

#include "termStore.hpp"
#include "termColony.hpp"

using namespace bmath::intern;

enum class Herbert
{
	herbert,
	ist,
	ein,
	cooler,
	typ,
	COUNT
};

int main()
{
	TermStore<TermString128> store;
	std::string_view info = "hallo ich bin bruno und ich bin der kameramann.";
	auto head = store.insert_new(TermString128(store, info));
	std::string output;

	const auto identity = [](auto& x) {return x; };
	store.at(head).read(store, output, identity);
	std::cout << output << '\n';

	std::cin.get();
}