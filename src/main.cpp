
#include <array>


#include "utility/typeDebug.hpp"
#include "stupidTests.hpp"

#include "types.hpp"
#include "io.hpp"
#include "rewrite.hpp"

/*
TODO:

important:
 - type checking
 - add option to normalize::Options to only evaluate no-lazy lambdas (perhaps rebrand current .eval_lambda option?)
 - fix pattern construction of value match in simp (value match is used like a proxy, but in reality owns a store node!)
 - construct pattern: single match, mutli match / variadic, value match
 - improve multi-match capabilities of NonComm variadic patterns (allow multiple multis in one NonComm instance)
 - check for invalid tokens when parsing pattern and literal
 - add eval_buildin for min/max to remove values guaranteed to not be minimum / maximum without requiring full evaluation

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

int main()
{
	{
		const auto names = std::to_array<std::string>({
			{ "(\\f n. f(f, n))(\\f n.(n <= 1)(1, n * f(f, -1 + n)), 5)" },
			{ "(\\f n. f(f, n))(\\f n.(n == 1)(1, n * f(f, -1 + n)), 5)" },
			{ "list(3 == 4, 3 != 4, 3 < 4, 3 <= 4, 3 >= 4, 3 > 4)" },
			{ "list(4 == 4, 4 != 4, 4 < 4, 4 <= 4, 4 >= 4, 4 > 4)" },
			{ "list(x == 4, x != 4, x < 4, x <= 4, x >= 4, x > 4)" },
			{ "set(1, -4, 5, a, 12, 13+3i, 13+4i, 13-1i, 13, 13+1i, b, -10)" },
			{ "\\x.\\y.\\z. x + y + z" },
			{ "true(3, 4)" },
			{ "false(3, 4^2)" },
			{ "a + b + 3 + c + 1 + 6 + a" },
			{ "(\\x y. x y)(a, 4)" },
			{ "(\\x y z. list(x, y, z))(a, 4)(sin(x))" },
			{ "(\\x y z. list(x, y, z))(a)(4, sin(x))" },
			{ "4^(0.5)" },
			{ "2^(0.5)" },
			{ "fmap(\\x. -x, sum(a, b, sin(x), 3, 5))" },
			{ "berb && frobbl && true && (false || !false || schmenck) && true && !alf" },
			{ "10/5" },
			{ "set(1, 100, a, b, 50 + 2 * 25, a, (\\x.2 x)(50))" },
			{ "list(floor(4.2), floor(3.9), floor(2-i), ceil(3.9), ceil(4), ceil(4+i))" },
		});
		for (const auto& name : names) {
			std::cout << name << "\n";
			auto term = simp::LiteralTerm(name);
			std::cout << "  ->  " << term.to_string() << "\n";
			std::cout << term.to_memory_layout() << "\n";
			term.normalize();
			std::cout << "  ->  " << term.to_string() << "\n";
			std::cout << term.to_memory_layout() << "\n";
			std::cout << "\n";
		}
		std::cout << "\n";
	}
	{
		const auto names = std::to_array<std::string>({
			{ "a^2 + 2 a b + b^2 = (a + b)^2" },
			{ "$a^2 + 2 $a b + b^2 = ($a + b)^2" },
			{ "list(x, xs...) = list(xs..., x)" },
			{ "x as... + x bs... = x (as... + bs...)" },
			{ "x as... + x bs... = x (as... + bs...)" },
			{ "'Y' = \\f n. f(f, n)" },
			{ "a + _VM(idx, dom, match) | type(a, complex) = _VM(idx, dom, match - a)" },
		});
		for (const auto& name : names) {
			auto rule = simp::RewriteRule(name);
			std::cout << name << "\n  ->  " << rule.to_string() << "\n\n";
		}
		std::cout << "\n";
	}
	//debug::enumerate_type();
	bmath::intern::debug::test_rechner();
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








