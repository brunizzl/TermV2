#include <iostream>

#include "termStore.hpp"

using namespace bmath::intern;

int main()
{
	TermStore<IndexTypePair<int, 10>, double> test;
	for (double d = 0; d < 128; d++) {
		std::cout << test.emplace_back(d) << '\n';
	}
	test.free(42);
	std::cout << test.emplace_back(-3.14159265359) << '\n';
	std::cout << '\n';
	for (int i = 0; i < 131; i++) {
		if (i % 64 != 0) {
			std::cout << i << '\t' << test.at(i) << '\n';
		}
	}
	std::cin.get();
}