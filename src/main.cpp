
#include <array>


#include "stupidTests.hpp"
#include "utility/typeDebug.hpp"


/*
TODO:

important:
 - add RuleRef type grouping two pattern refs
 - make match and replace two distinct functions in RuleSet, allow caller to rematch
 - write version of tree::combine only checking changed tree parts
 - decide if garbage collection should be used (and if so implement it + change functions currently deallocating)
 - implement meta_pn::match function for variadic patterns
 - improve multi-match capabilities of NonComm variadic patterns (allow multiple multis in one NonComm instance)
 - achieve feature parity between compile time pattern and run time pattern (add value match to ct and conditions to rt

nice to have:
 - enable StupidBufferVector to handle non-trivial destructible types -> change name to BufferVector
 - pattern::match::copy should copy in same store (again...)
 - noexceptify everything
 - restructure everything to use modules (basically needed to constexprfy all the things)
 - allow to restrict a variables domain (split Literal::symbol in own enum?)

idea status:
 - add type system: check only when pattern /usual term instanciates, not needed when copied
 - introduce "intrinsic" type and use index in intrinsic typed_idx to specify witch (have all stored at central function pointer array)
 - add a pattern variable type catching functions with other pattern variables beeing parameters of these functions (perhaps two types, one parameters?)
 - add concept of "easy pattern" with every pattern variable occuring only once, match these in guaranteed linear time
 - const term_slc iterator dereferences to Ref, not typed_idx
 - let each RewriteRule have a name in form of a constant c-string (not what pattern is constructed from, but name of rule, e.g. "differentiation: product rule" or something)
 - prettify pattern::Multivar sytax to require "..." after variable name 
 - always keep -1 at some known index in store and never allocate new -1 in build_negated and build_inverted (problem: identify bevore copy, solution: you know the index)
 - change permutation_equals (currently returning bool) to permutation_compare returning one of {match, smaller, larger, unordered} -> binary search for math types?
	   also requires find_matching_permutation to return same values.
 - sort pattern with care for match variables
 - change macros in meta_pn to create function objects, not functions 
 - build meta_pn::match not from pattern encoded as type, but pattern encoded as value by taking lamdas returning pattern part as template parameter
*/


using namespace bmath::intern;
using namespace bmath::intern::pattern;

void test_f(PnType t) {
	using Options = EnumSwitch<PnType, 
		meta::List<Variadic, NamedFn, Fn, MatchType, LambdaParam>,
		meta::Seq<Literal::complex, Literal::symbol>
	>;

	std::cout << (unsigned)t << "\t=   ";
	
	switch (Options::decide(t)) {
	case Options::is_type<Variadic>:
		std::cout << "type Variadic\n";
		break;
	case Options::is_type<NamedFn>:
		std::cout << "type NamedFn\n";
		break;
	case Options::is_type<Fn>:
		std::cout << "type Fn\n";
		break;
	case Options::is_value<Literal::symbol>:
		std::cout << "value symbol\n";
		break;
	case Options::is_value<Literal::complex>:
		std::cout << "value complex\n";
		break;
	case Options::is_type<LambdaParam>:
		std::cout << "type MatchType\n";
		break;
	case Options::is_type<MatchType>:
		std::cout << "type MatchType\n";
		break;
	}
}

int main()
{
	//for (int i = 0; i < (unsigned)PnType::COUNT; i++) {
	//	test_f((PnType)i);
	//}
	//std::cout << "\n";

	debug::enumerate_type();
	debug::test_rechner();
	//test::stable_sort();
	//test::meta_pattern();
	//test::meta_pattern_2();
	//test::pattern_term();
	//test::arithmetic_term();
	//test::copy();
	//test::bit_set();
	//test::match();
	//test::alloc_n();
	//test::term_array();
	//test::combine();
}








