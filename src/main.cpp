
#include "stupidTests.hpp"
using namespace bmath::intern;

/*
TODO:
 - match::permutation_equals: abort earlier (use fact that both terms are sorted, not only pattern)
 - match::match_and_replace: fist copy in some monotonic buffer store or something
 - ctor of PnTerm: regroup sums / products if nessecairy to allow value match variables to better catch values
 - ctor of PnTerm: throw if two value match variables are held directly in same sum / product
 - TermSLC: only allow null_value at end -> Iterator becomes be simpler (meaning some functions in current form may produce invalid results)
 - write nonrecursive copy with queue for better locality in store (challenge: children are allocated after parents, need to keep (sub) index where child reference is to be held)
 - restructure everything to use modules
 - write data structure on abstraction layer above PnTerm to group multiple pattens
 - (perhaps?) let each PnTerm have a name in form of a constant c-string
 - enable rematch or write factorisation per hand (if from hand, perhaps introduce some way to call such routines as pattern?)
*/



int main()
{
	//test::combine_exact();
	//test::arithmetic_term();
	//test::pattern_term();
	//test::copy();
	//test::bit_set();
	//test::match();
	//debug::enumerate_pn_type();
	debug::test_rechner();
}











