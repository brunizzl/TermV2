
#include "stupidTests.hpp"
using namespace bmath::intern;

/*
TODO:

important:
 - seperate match and replace into multiple functions
 - add concept of "easy pattern" with every pattern variable occuring only once, match these in guaranteed linear time
 - throw if MatchData::max_variadic_count is succeeded in lhs of pattern (but ignore sums / products beneath value match variable)
 - check tree::copy and match::copy weather they work with two identical stores
 - use templates to allow multiple stores
 - write data structure one abstraction layer above RewriteRule to group multiple pattens
 - restructure everything to use modules (basically needed to constexprfy all the things)
 - allow to restrict a variables possibility space e.g. natural, integer, real, complex (split Literal::variable in own enum?)

nice to have:
 - introduce "intrinsic" type and use index in intrinsic typed_idx to specify witch (have all stored at central function pointer array)
 - add a pattern variable type catching functions with other pattern variables beeing parameters of these functions (perhaps two types, one parameters?)
 - match::permutation_equals: abort earlier (use fact that both terms are (assumed to be) sorted, not only pattern)
 - avoid copying pattern::MultiPn entries (needs to know original storage location of each entry, to steal it from there)
 - seperate utility in different files, create utility folder
 - noexceptify everything

idea status:
 - const term_slc iterator dereferences to Ref, not typed_idx
 - ctor of RewriteRule: regroup sums / products if nessecairy to allow value match variables to better catch values
 - TermSLC: only allow null_value at end -> SLC_Iterator becomes  simpler (meaning some functions in current form may produce invalid results)
 - let each RewriteRule have a name in form of a constant c-string (not what pattern is constructed from, but name of rule, e.g. "differentiation: product rule" or something)
 - prettify pattern::Multivar sytax to require (and allow) "..." after variable name 
 - always keep -1 at some known index in store and never allocate new -1 in build_negated and build_inverted (problem: identify bevore copy)
*/

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








