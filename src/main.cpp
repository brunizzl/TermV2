#include <iostream>

#include "termStore.hpp"

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
	IndexTypePair<Herbert, Herbert::COUNT> herbert1;
	std::cout << "herberts groesse: " << sizeof(IndexTypePair<Herbert, Herbert::COUNT>) << '\n';
	IndexTypePair<Herbert, Herbert::COUNT> herbert2 = { 0, Herbert::herbert };
	std::cout << "herbert" << ((herbert1 > herbert2) ? "1" : "2") << " hat in der automatischen ordnung hoeheren index.\n\n";

	TermStore<double> test;
	for (double d = 0; d < 128; d++) {
		std::cout << test.emplace_new(d) << '\n';
	}
	test.free(42);
	std::cout << test.emplace_new(-3.14159265359) << '\n';
	std::cout << '\n';
	for (int i = 0; i < 131; i++) {
		if (i % 64 != 0) {
			std::cout << i << '\t' << test.at(i) << '\n';
		}
	}
	std::cin.get();
}