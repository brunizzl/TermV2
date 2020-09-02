#include <iostream>

#include "visit.hpp"
#include "internalFunctions.hpp"
#include "termStore.hpp"

using namespace bmath::intern;


int main()
{
    Regular_Power* term = new Regular_Power(new Regular_F1(F1_Type::sin, new Regular_Value({ 3.1415/4, 0 })), new Regular_Value({ 2, 0 }));
	std::cout <<
		regular_visit(term,
			[](Regular_F1*) { return "par"; },
			[](Regular_Log*) { return "log"; },
			[](Regular_Power*) { return "pow"; },
			[](Regular_Product*) { return "pro"; },
			[](Regular_Sum*) { return "sum"; },
			[](Regular_Value*) { return "val"; },
			[](Regular_Variable*) { return "var"; }) << std::endl;
	std::cout << evaluate(term) << std::endl;
    delete term;

	std::cout << "max index: " << std::hex << TermIndexTypePair::max_index << '\n';
}