
#include "stupidTests.hpp"
using namespace bmath::intern;

/*
TODO:

important:
 - restructure everything to use modules
 - TermSLC: only allow null_value at end -> Iterator becomes  simpler (meaning some functions in current form may produce invalid results)
 - write data structure one abstraction layer above PnTerm to group multiple pattens
 - enable rematch or write factorisation per hand (if from hand, perhaps introduce some way to call such routines as pattern?)
 - group all variations of pattern variables in one SumEnum
 - add pattern variable catching functions with other pattern variables beeing parameters of these functions (perhaps two types, one variadic?)
 - bug: tree::combine_values (both exact and inexact) can produce sum / product with single element (e.g. "a+0" or "a*1"), 
      but tree::establish_basic_order combines layers only once and bevore values (also this (to be added) layer combination could allow another value combination perhaps? (maybe not))
	  idea 1: make all combining functions return a bool to know if any combination actually occured and if so, rerun them all. (potentially very slow)
	  idea 2: bundle all combining actions into a single function (potentially very ugly) (combine_inexact requires a second version though...)

nice to have:
 - write bit_vector class
 - const term_slc iterator dereferences to Ref, not typed_idx
 - match::permutation_equals: abort earlier (use fact that both terms are (assumed to be) sorted, not only pattern)
 - match::match_and_replace: fist copy in some monotonic buffer store or something
 - ctor of PnTerm: regroup sums / products if nessecairy to allow value match variables to better catch values
 - add (perhaps untyped?, perhaps only TypedIdx_T needed) Ref for sub-index references in store (needed below)
 - avoid copying pattern::MultiVar entries (needs to know original storage location of each entry, to steal it from there)
 - write nonrecursive copy with queue for better locality in store (challenge: children are allocated after parents, need to keep (sub) index where child reference is to be held)

idea status:
 - let each PnTerm have a name in form of a constant c-string (not what is constructed from, but name of rule
 - prettify pattern::Multivar sytax to require (and allow) "..." after variable name 
 - always keep -1 at index 1 in store and never allocate new -1 in build_negated and build_inverted (problem: identify bevore copy)
*/



int main()
{
	//test::combine_exact();
	//test::pattern_term();
	//test::arithmetic_term();
	//test::copy();
	//test::bit_set();
	//test::match();
	//debug::enumerate_pn_type();
	debug::test_rechner();
}











