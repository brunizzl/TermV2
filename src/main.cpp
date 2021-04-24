
#include <array>


#include "utility/typeDebug.hpp"
#include "stupidTests.hpp"

#include "types.hpp"
#include "io.hpp"
#include "rewrite.hpp"

/*
TODO:

important:
 - finish match:
	   - test_condition
	   - find_shift
	   - adjust rematch to work with implementation of find_shift
	   - (eval_value_match)
 - implement fst, snd, ffilter, fsplit, ...
 - add eval_buildin for min/max to remove values guaranteed to not be minimum / maximum without requiring full evaluation
 - store hints for faster /nonrepetitive matching in PatternCallData
 - only test subset of rulerange
 - type checking (extended: keep track of what restrictions apply to match variable in lhs, use in rhs)
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
 - achieve feature parity between compile time pattern and run time pattern (add value match to ct)
 - enable StupidBufferVector to handle non-trivial destructible types -> change name to BufferVector
 - automate error checking in stupidTests.hpp -> change name to tests.hpp
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
		for (const auto& props : simp::nv::common_table) {
			std::cout
				<< props.name
				<< std::string(25 - props.name.length(), '.')
				<< (unsigned)props.type
				<< " \t-> "
				<< simp::nv::name_of(props.result_space)
				<< "\n";
		}
	}
	if (false) {
		const auto names = std::to_array<std::string>({
			{ "list(conj(3-i), conj(4+3i), conj(8), conj(-i))" },
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
			{ "a_sqr + two_a b + b^2 + cs... | (sqrt(a_sqr) == 0.5 two_a) = (0.5 two_a + b)^2 + cs..." },
			{ "'Y' = \\f n. f(f, n)" },
			{ "a + _VM(idx, dom, match) | a :complex = _VM(idx, dom, match - a)" },

			{ "0 xs... = 0" },
			{ "0^x     = 0" },
			{ "x^0     = 1" },
			{ "x^1     = x" },

			{ "(x^a)^b = x^(a b)" },
			{ "x x     = x^2" },
			{ "x x^a   = x^(a + 1)" },
			{ "x^a x^b = x^(a + b)" },
			{ "exp(product(ln(y), xs...)) = y^(product(xs...))" },

			{ " a^2 +  2 a b + b^2 =  (a + b)^2" },
			{ " a^2 -  2 a b + b^2 =  (a - b)^2" },
			{ "$a^2 + 2 $a b + b^2 = ($a + b)^2" },

			{ "a bs... + a cs... | !(a :complex) = a (product(bs...) + product(cs...))" },
			{ "a bs... + a       | !(a :complex) = a (product(bs...) + 1)" },
			{ "a       + a       | !(a :complex) = 2 a" },
			{ "a (b + cs...)     |   a :complex  = a b + a product(cs...)" },

			{ "-a     | a :sum     = fmap(\\x. -x    , a)" },
			{ "a^(-1) | a :product = fmap(\\x. x^(-1), a)" },

			{ "sin(x)^2 + cos(x)^2 = 1" },

			//roots and extreme points of sin and cos:
			{ "cos(             'pi')           = -1" },
			{ "cos(($k + 0.5)   'pi') | $k :int =  0" },
			{ "cos((2 $k)       'pi') | $k :int =  1" },
			{ "cos((2 $k + 1)   'pi') | $k :int = -1" },
			{ "sin(             'pi')           =  0" },
			{ "sin($k           'pi') | $k :int =  0" },
			{ "sin((2 $k + 0.5) 'pi') | $k :int =  1" },
			{ "sin((2 $k + 1.5) 'pi') | $k :int = -1" },

			//differentiation rules:
			{ "diff(x, x)                                 = 1" },
			{ "diff(a, x)       | a :symbol || a :complex = 0" },
			{ "diff(f^a, x)     | a:complex               = diff(f, x) a f^(a-1)" },
			{ "diff(a^f, x)     | a:complex               = diff(f, x) ln(a) a^f" },
			{ "diff(g^h, x)                               = (diff(h, x) ln(g) + h diff(g, x)/g) g^h" },
			{ "diff(a, x)       | a :sum                  = fmap(\\f .diff(f, x), a)" },
			{ "diff(u vs..., x)                           = diff(u, x) vs... + u diff(product(vs...), x)" },
			{ "diff(f(y), x)                              = diff(y, x) fdiff(f)(y)" },

			{ "fdiff(\\x .y) = \\x .diff(y, x)" },
			{ "fdiff(sin)    = cos" },
			{ "fdiff(cos)    = \\x .-sin(x)" },
			{ "fdiff(exp)    = exp" },
			{ "fdiff(ln)     = \\x .x^(-1)" },
			{ "fdiff(tan)    = \\x .cos(x)^(-2)" },
			
			//exponential runtime fibonacci implementation:
			{ "'fib'(0) = 0" },
			{ "'fib'(1) = 1" },
			{ "'fib'(n) = 'fib'(n - 1) + 'fib'(n - 2)" },
			
			////reversing a list:
			//{ "xs :list...                 | reverse(list{xs}) = reverse'(list{}, list{xs})" },
			//{ "xs :list..., y, ys :list... | reverse'(list{xs}, list{y, ys}) = reverse'(list{y, xs}, list{ys})" },
			//{ "xs :list...,                | reverse'(list{xs}, list{})      = list{xs}" },
			//
			////listing first n fibonacci numbers:
			//{ "n :nat0                     | fib_n(n + 2)                   = reverse(list_fibs(n, list{1, 0}))" },
			//{ "n :nat, a, b, tail :list... | list_fibs(n, list{a, b, tail}) = list_fibs(n - 1, list{force(a + b), a, b, tail})" },
			//{ "              tail :list... | list_fibs(0, list{tail})       = list{tail}" },

			{ "ffilter(p, f(xs...)) = 'take_true'(f(), fmap(\\x .pair(p(x), x), f(xs...)))" },
			{ "'take_true'(f(xs...), f(pair(true, x), ys...)) = 'take_true'(f(xs..., x), f(ys...))" },
			{ "'take_true'(f(xs...), f(pair(_   , x), ys...)) = 'take_true'(f(xs...), f(ys...))" },
			{ "'take_true'(f(xs...), f())                     = f(xs...)" },

			{ "'sort'(list())         = list()" },
			{ "'sort'(list(x))        = list(x)" },
			{ "'sort'(list(x, xs...)) = 'concatcos'(ffilter(\\y .y < x, list(xs...)), x, ffilter(\\y .y >= x, list(xs...)))" },
			{ "'concatcons'(list(xs...), x, list(ys...)) = list(xs..., x, ys...)" },
			
			{ "union(set(xs...), set(ys...)) = set(xs..., ys...)" },
			{ "union()                       = set()" },
			
			{ "intersection(set(x, xs...), set(x, ys...)) = union(set(x), intersection(set(xs...), set(ys...)))" },
			{ "intersection(x, xs...)                     = set()" },
			
			{ "min{x, y} | x :real, y :real, x > y = y" },
			{ "max{x, y} | x :real, y :real, x > y = x" },
			
			{ "foldr(f, acc, list())         = acc" },
			{ "foldr(f, acc, list(x, xs...)) = f(x, foldr(f, acc, list(xs...)))" },
		};
		for (const simp::RuleRef rule : rules) {
			std::cout << rule.to_string() << "\n\n";
		}
		std::cout << "\n";

		while (true) {
			std::string name;
			std::cout << "simp> ";
			std::getline(std::cin, name);
			try {
				auto term = simp::LiteralTerm(name);
				term.normalize();
				term.head = simp::greedy_apply_ruleset(rules, term.mut_ref(), 0);
				std::cout << " = " << term.to_string() << "\n\n";
			}
			catch (bmath::ParseFailure failure) {
				std::cout << "parse failure: " << failure.what << '\n';
				std::cout << name << '\n';
				std::cout << std::string(failure.where, ' ') << "^\n\n";
			}
		}
	}
	{
		const simp::RuleSet rules = {
			{ " a^2 +  2 a b + b^2 =  (a + b)^2" },
			{ "a bs... + a cs...   = a (product(bs...) + product(cs...))" },
			{ "sin(x)^2 + cos(x)^2 = 1" },
		};
		const auto names = std::to_array<std::string>({
			{ "sin(x)^2 + 2 sin(x) herbert + herbert^2" },
			{ "sin(a + b)^2 + cos(a + b)^2" },
			{ "a c d f + b c d g h" },
		});
		for (const auto& name : names) {
			std::cout << name << "\n";
			auto term = simp::LiteralTerm(name);
			term.normalize();
			for (const auto rule : rules) {
				simp::match::MatchData match_data = term.store.data();
				std::cout << "  " << (simp::match::match_(rule.lhs, term.ref(), match_data) ? "MAATCH " : "nope   ") << simp::print::to_string(rule.lhs) << "\n";
			}
		}
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








