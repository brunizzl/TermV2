
#include <array>


#include "utility/typeDebug.hpp"
#include "stupidTests.hpp"

#include "types.hpp"
#include "io.hpp"
#include "rewrite.hpp"

/*
TODO:

important:
 - start building RuleSet
 - store hints for faster matching in PatternCallData
 - type checking (extended: keep track of what restrictions apply to match variable in lhs, use in rhs)
 - add eval_buildin for min/max to remove values guaranteed to not be minimum / maximum without requiring full evaluation
 - finnish building / verifying pattern:
      - verify only one occurence of each multi match
      - verify only one occurence of multi match in one Comm, any number of occurences in one NonComm, no occurences elsewhere
      - adjust muti match indices to equal corresponding call + add management field to call
      - check PatternCall to each not exceed maximal length and to not have more parameters than is allowed 
	  - check no two multi match variables in direct succession
	  - check no call in pattern only containing multi match
      - bubble value match variables up as high as possible, make first occurence owning

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
 - let each RewriteRule have a name in form of a constant c-string (not what pattern is constructed from, but name of rule, e.g. "differentiation: product rule" or something)
 - always keep -1 at some known index in store and never allocate new -1 in build_negated and build_inverted (problem: identify bevore copy, solution: you know the index)
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
		const simp::RuleSet rules = {
			{ "a^2 + 2 a b + b^2 + cs... = (a + b)^2 + cs..." },
			{ "$a^2 + 2 $a b + b^2 + cs... = ($a + b)^2 + cs..." },
			{ "a_sqr + two_a b + b^2 + cs... | (sqrt(a_sqr) == 0.5 two_a) = (0.5 two_a + b)^2 + cs..." },
			{ "list(x, xs...) = list(xs..., x)" },
			{ "product(x, as...) + product(x, bs...) = x (product(as...) + product(bs...))" },
			{ "'Y' = \\f n. f(f, n)" },
			{ "a + _VM(idx, dom, match) | type(a, complex) = _VM(idx, dom, match - a)" },
			{ "list(x, y, z, zs...) | x == y + z, x != y, type(y, complex) = 'huebsch'" },
			{ "fmap(f, g(xs..., x))= 'reverse_cons'(fmap(f, g(xs...)), f(x))" }
		};
		for (const simp::RuleRef rule : rules) {
			std::cout << rule.to_string() << "\n\n";
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








