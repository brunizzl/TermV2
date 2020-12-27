
#include "stupidTests.hpp"
using namespace bmath::intern;

/*
TODO:

important:
 - seperate match and replace into multiple functions
 - add concept of "easy pattern" with every pattern variable occuring only once, match these in guaranteed linear time
 - write memory leak detection function
 - throw if MatchData::max_variadic_count is succeeded in lhs of pattern (but ignore sums / products beneath value match variable)
 - check tree::copy and match::copy weather they work with two identical stores
 - use templates to allow multiple stores
 - write data structure one abstraction layer above PnTerm to group multiple pattens
 - restructure everything to use modules (basically needed to constexprfy all the things)
 - allow to restrict a variables possibility space e.g. natural, integer, real, complex (split Leaf::variable in own enum?)
 - reintroduce named_fn / variadic_fn, but make them better compatible with Fn (and sum / product)
     idea: also store Fn in StoredVector and append name of named_fn after parameters

nice to have:
 - introduce "intrinsic" type and use index in intrinsic typed_idx to specify witch (have all stored at central function pointer array)
 - add a pattern variable type catching functions with other pattern variables beeing parameters of these functions (perhaps two types, one parameters?)
 - const term_slc iterator dereferences to Ref, not typed_idx
 - match::permutation_equals: abort earlier (use fact that both terms are (assumed to be) sorted, not only pattern)
 - avoid copying pattern::MultiPn entries (needs to know original storage location of each entry, to steal it from there)x where child reference is to be held)
 - seperate utility in different files, create utility folder
 - noexceptify everything

idea status:
 - ctor of PnTerm: regroup sums / products if nessecairy to allow value match variables to better catch values
 - TermSLC: only allow null_value at end -> SLC_Iterator becomes  simpler (meaning some functions in current form may produce invalid results)
 - let each PnTerm have a name in form of a constant c-string (not what pattern is constructed from, but name of rule, e.g. "differentiation: product rule" or something)
 - prettify pattern::Multivar sytax to require (and allow) "..." after variable name 
 - always keep -1 at some known index in store and never allocate new -1 in build_negated and build_inverted (problem: identify bevore copy)
*/

#include <iomanip>
#include <bitset>

int main()
{
	debug::enumerate_type();
	debug::test_rechner();
	//test::combine_exact();
	//test::pattern_term();
	//test::arithmetic_term();
	//test::copy();
	//test::bit_set();
	//test::match();
	//test::combine();
	//test::alloc_n();
	//test::term_array();
}








