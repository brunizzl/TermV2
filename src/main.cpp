
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
	{
		const auto names = std::to_array<std::string>({
			{ "(\\x .\\x. x)(1)(2)" },
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
			{ "'Y' = \\f n. f(f, n)" },
			{ "a + _VM(idx, dom, match) | a :complex = _VM(idx, dom, match - a)" },
			{ "list(x, y, z, zs...) | x == y + z, x != y, y :complex = 'huebsch'" },
			{ "fmap(f, g(xs..., x))= 'reverse_cons'(fmap(f, g(xs...)), f(x))" },

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
			{ "fdiff(tan)    = \\x .cos(x)^(-2)" }
			//
			////exponential runtime fibonacci implementation:
			//{ "         fib(0) = 0" },
			//{ "         fib(1) = 1" },
			//{ "n :nat | fib(n) = fib(n - 1) + fib(n - 2)" },
			//
			////reversing a list:
			//{ "xs :list...                 | reverse(list{xs}) = reverse'(list{}, list{xs})" },
			//{ "xs :list..., y, ys :list... | reverse'(list{xs}, list{y, ys}) = reverse'(list{y, xs}, list{ys})" },
			//{ "xs :list...,                | reverse'(list{xs}, list{})      = list{xs}" },
			//
			////listing first n fibonacci numbers:
			//{ "n :nat0                     | fib_n(n + 2)                   = reverse(list_fibs(n, list{1, 0}))" },
			//{ "n :nat, a, b, tail :list... | list_fibs(n, list{a, b, tail}) = list_fibs(n - 1, list{force(a + b), a, b, tail})" },
			//{ "              tail :list... | list_fibs(0, list{tail})       = list{tail}" },
			//
			////sorting numbers:
			//{ "                     | sort(list{})      = list{}" },
			//{ "x                    | sort(list{x})     = list{x}" },
			//{ "p :real, xs :list... | sort(list{p, xs}) = concat3(sort(filter_s(p, list{}, list{xs})), list{p}, sort(filter_le(p, list{}, list{xs})))" },
			//
			//{ "xs :list..., ys :list..., zs :list... | concat3(list{xs}, list{ys}, list{zs}) = list{xs, ys, zs}" },
			//
			//{ "cond :not_positive, true_res, false_res | if_positive(cond, true_res, false_res) = false_res" },
			//{ "cond :positive,     true_res, false_res | if_positive(cond, true_res, false_res) = true_res" },
			//
			//{ "p :real, xs :list..., y :real, ys :list... | filter_le(p, list{xs}, list{y, ys}) = filter_le(p, if_positive[force(p - y), list{xs}, list{xs, y}], list{ys})" },
			//{ "p :real, xs :list...,                      | filter_le(p, list{xs}, list{})      = list{xs}" },
			//
			//{ "p :real, xs :list..., y :real, ys :list... | filter_s(p, list{xs}, list{y, ys}) = filter_s(p, if_positive[force(p - y), list{xs, y}, list{xs}], list{ys})" },
			//{ "p :real, xs :list...,                      | filter_s(p, list{xs}, list{})      = list{xs}" },
			//
			//
			//{ "x, xs :set... | set(x, x, xs) = set(x, xs)" },
			//
			//{ "xs :set..., ys :set... | union(set(xs), set(ys)) = union(set(xs, ys))" },
			//{ "                       | union()                 = set()" },
			//
			//{ "x, xs :set..., ys :set... | intersection(set(x, xs), set(x, ys)) = union(set(x), intersection(set(xs), set(ys)))" },
			//{ "xs, ys                    | intersection(xs, ys)                 = set()" },
			//{ "                          | intersection()                       = set()" },
			//
			//{ "x :real, y :real | min{x, y} = if_positive(force(x-y), y, x)" },
			//{ "x :real, y :real | max{x, y} = if_positive(force(x-y), x, y)" },
			//
			////A / B is more commonly written A \ B
			//{ "x, xs :set..., y, ys :set... | set{x, xs} / set{x, ys} = set{xs} / set{x, ys}" },
			//{ "   xs :set...,    ys :set... | set{xs}    / set{ys}    = set{xs}" },
			//
			//{ "'true'  = lambda($0)" },
			//{ "'false' = lambda($1)" },
			//{ "'not'   = lambda(call($0, lambda($1), lambda($0)))" },
			//{ "'and'   = lambda(call($0, $1, $0))" },
			//{ "'or'    = lambda(call($0, $0, $1))" },
			//
			//{ "'zero'  = lambda($1)" },
			//{ "'one'   = lambda(call($0, $1))" },
			//{ "'two'   = lambda(call($0, call($0, $1)))" },
			//{ "'three' = lambda(call($0, call($0, call($0, $1))))" },
			//{ "'succ'  = lambda(call($1, call($0, $1, $2)))" },
			//
			//{ "   x, xs :list... | cons(x, list{xs})   = list{x, xs}" },
			//{ "f, x, xs :list... | map(f, list{x, xs}) = cons(call(f, x), map(f, list{xs}))" },
			//{ "f,                | map(f, list{})      = list{}" },
			//
			//{ "f, acc, x, xs :list... | foldl(f, acc, list{x, xs}) = call(f, x, foldl(f, acc, list{xs}))" },
			//{ "f, acc,                | foldl(f, acc, list{})      = acc" },
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








