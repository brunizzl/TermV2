
#include <array>


#include "stupidTests.hpp"
#include "utility/typeDebug.hpp"

#include "types.hpp"
#include "io.hpp"
#include "rewrite.hpp"

/*
TODO:

important:
 - construct pattern: single match, mutli match / variadic, value match
 - improve multi-match capabilities of NonComm variadic patterns (allow multiple multis in one NonComm instance)
 - check for invalid tokens when parsing pattern and literal
 - split FixedArity in Complex to complex, ... to boolean, etc.

nice to have:
 - implement meta_pn::match function for variadic patterns
 - achieve feature parity between compile time pattern and run time pattern (add value match to ct and conditions to rt)
 - enable StupidBufferVector to handle non-trivial destructible types -> change name to BufferVector
 - automate error checking in stupidTests.hpp -> change name to tests.hpp
 - pattern::match::copy should copy in same store (again...)
 - noexceptify everything
 - restructure everything to use modules (basically needed to constexprfy all the things)
 - allow to restrict a variables domain (split Literal::symbol in own enum?)

idea status:
 - add type system: check only when pattern /usual term instanciates, not needed when copied
 - introduce "intrinsic" rule type and use index in intrinsic typed_idx to specify witch (have all stored at central function pointer array)
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

int main()
{
	{
		const auto names = std::to_array<std::string>({
			{ "list(3 != 4, 3 < 4, 3 <= 4, 3 >= 4, 3 > 4, 3 == 4)" },
			{ "list(4 != 4, 4 < 4, 4 <= 4, 4 >= 4, 4 > 4, 4 == 4)" },
			{ "set(1, -4, 5, a, 12, 13+3i, 13+4i, 13-1i, 13, 13+1i, b, -10)" },
			{ "\\f.(\\x.f(x(x)))(\\x.f(x(x)))" },
			{ "\\x.\\y.\\z. x + y + z" },
			{ "true(3, 4)" },
			{ "false(3, 4^2)" },
			{ "a + b + 3 + c + 1 + 6 + a" },
			{ "(\\x y. x y)(a, 4)" },
			{ "(\\x y z. list(x, y, z))(a)(4)(sin(x))" },
			{ "(\\x y z. list(x, y, z))(a)(4, sin(x))" },
			{ "4^(0.5)" },
			{ "2^(0.5)" },
			{ "fmap(\\x. -x, sum(a, b, sin(x), 3, 5))" },
			{ "berb && frobbl && true && (false || !false || schmenck) && true && !alf" },
			{ "10/5" },
			{ "set(1, 100, a, b, 50 + 2 * 25, a, (\\x.2 x)(50))" },
			{ "min(1, 100, 40, 50 + 2 * 25, -20, 101, (\\x.2 x)(50))" },
			{ "max(1, 100, 40, 50 + 2 * 25, -20, 101, (\\x.2 x)(50))" },
		});
		for (const auto& name : names) {
			std::cout << name << "\n";
			auto term = simp::Literal(name);
			std::cout << "  ->  " << term.to_string() << "\n";
			term.establish_order();
			std::cout << "  ->  " << term.to_string() << "\n\n";
		}
		std::cout << "\n";
	}
	{
		const auto names = std::to_array<std::string>({
			{ "a^2 + 2*a*b + b^2 = (a + b)^2" },
			{ "$a^2 + 2*$a*b + b^2 = ($a + b)^2" },
			{ "list(x, xs...) = list(xs..., x)" },
			{ "x*as... + x*bs... = x*(as... + bs...)" },
			{ "x*as... + x*bs... = x*(as... + bs...)" },
			{ "'Y' = \\f.(\\x.f(x(x)))(\\x.f(x(x)))" },
			{ "'true_' = \\x y . x" }
		});
		for (const auto& name : names) {
			auto rule = simp::RewriteRule(name);
			std::cout << name << "\n  ->  " << rule.to_string() << "\n\n";
		}
		std::cout << "\n";
	}
	std::cin.get();
	//debug::enumerate_type();
	//debug::test_rechner();
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








