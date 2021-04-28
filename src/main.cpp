
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
       - fix bug: match will not always succeed, if Literal::call in pattern holds a rematchable subterm 
	        (idea: combine set_rematchable and prime_call, problem is, that prime_call has to use preorder, where set_rematchable (currently) uses postorder)
	   - test_condition
	   - find_dilation
	   - adjust rematch to work with implementation of find_dilation
	   - eval_value_match
 - implement fst, snd, ffilter, fsplit, ...
 - add eval_buildin for min/max to remove values guaranteed to not be minimum / maximum without requiring full evaluation
 - only test subset of rulerange (requires rule iterator to become random access)
 - remove "0" from sum, "1" from product
 - type checking (extended: keep track of what restrictions apply to match variable in lhs, use in rhs)
 - finnish building / verifying pattern:
      - (depends on type checking) verify whole patterns
      - check PatternCall to each not exceed maximal length and check numer of PatternCall in pattern not more than allowed 
	  - enable implicit outer multi

nice to have:
 - use ref-counting in store instead of always copy
 - store symbols in program wide map, only use hashes (aka. index in map) in every term
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
		for (unsigned i = 0; i < (unsigned)simp::NodeType::COUNT; i++) {
			const auto as_native = simp::nv::Native(simp::NodeType(i));
			const auto name = simp::nv::name_of(as_native);
			std::cout
				<< name
				<< std::string(std::max(20 - (int)name.size(), 0), '.')
				<< i
				<< "\n";
		}
		std::cout << "\n";
		for (const auto& props : simp::nv::common_table) {
			std::cout
				<< props.name
				<< std::string(std::max(20 - (int)props.name.size(), 0), '.')
				<< (unsigned)props.type
				<< " \t-> "
				<< simp::nv::name_of(props.result_space)
				<< "\n";
		}
		std::cout << "\n\n";
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
		std::cout << "\n\n\n";
	}
	{
		const simp::RuleSet rules = {
			{ "'Y' = \\f n. f(f, n)" },
			
			{ "0 xs... = 0" },
			{ "0 + xs... = sum(xs...)" },
			{ "1 * xs... = product(xs...)" },
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
			
			{ "a bs... + a cs... | !(a :complex)      = a (product(bs...) + product(cs...))" },
			{ "a bs... + a       | !(a :complex)      = a (product(bs...) + 1)" },
			{ "a       + a       | !(a :complex)      = 2 a" },
			{ "a b               | a :complex, b :sum = fmap(\\x .a x, b)" },
			
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
			{ "diff(x, x)                    = 1" },
			{ "diff(a, x)       | a :complex = 0" },
			{ "diff(a, x)       | a :symbol  = 0" },
			{ "diff(f^a, x)     | a :complex = diff(f, x) a f^(a-1)" },
			{ "diff(a^f, x)     | a :complex = diff(f, x) ln(a) a^f" },
			{ "diff(g^h, x)                  = (diff(h, x) ln(g) + h diff(g, x)/g) g^h" },
			{ "diff(a, x)       | a :sum     = fmap(\\f .diff(f, x), a)" },
			{ "diff(u vs..., x)              = diff(u, x) vs... + u diff(product(vs...), x)" },
			{ "diff(f(y), x)                 = diff(y, x) fdiff(f)(y)" },
			
			{ "fdiff(\\x .y) = \\x .diff(y, x)" },
			{ "fdiff(sin)    = cos" },
			{ "fdiff(cos)    = \\x .-sin(x)" },
			{ "fdiff(exp)    = exp" },
			{ "fdiff(ln)     = \\x .x^(-1)" },
			{ "fdiff(tan)    = \\x .cos(x)^(-2)" },
			
				//exponential runtime fibonacci implementation:
			{ "'fib'(n) | n >= 0 = (n < 2)(n, 'fib'(n - 1) + 'fib'(n - 2))" },
			
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
			{ "'take_true'(fxs, f())                          = fxs" },
			
			{ "fsplit(p, f(xs...)) = 'split_hlp'(f(), f(), fmap(\\x .pair(p(x), x), f(xs...)))" },
			{ "'split_hlp'(f(xs...), f(ys...), f(pair(true, z), zs...)) = 'split_hlp'(f(xs..., z), f(ys...), f(zs...))" },
			{ "'split_hlp'(f(xs...), f(ys...), f(pair(_   , z), zs...)) = 'split_hlp'(f(xs...), f(ys..., z), f(zs...))" },
			{ "'split_hlp'(fxs, fys, f())                               = pair(fxs, fys)" },
			
			{ "'sort'(list())                               = list()" },
			{ "'sort'(list(x))                              = list(x)" },
			{ "'sort'(list(x, xs...))                       = 'sort_h1'(fsplit(\\y .y < x, list(xs...)), x)" },
			{ "'sort_h1'(pair(list(xs...), list(ys...)), x) = 'sort_h2'('sort'(xs...), x, 'sort'(ys...))" },
			{ "'sort_h2'(list(xs...), x, list(ys...))       = list(xs..., x, ys...)" },
			
			{ "union(set(xs...), set(ys...)) = set(xs..., ys...)" },
			{ "union()                       = set()" },
			
			{ "intersection(set(x, xs...), set(x, ys...)) = union(set(x), intersection(set(xs...), set(ys...)))" },
			{ "intersection(x, xs...)                     = set()" },
			
			{ "min{x, y} | x :real, y :real, x > y = y" },
			{ "max{x, y} | x :real, y :real, x > y = x" },
			
			{ "ffoldr(f, acc, list())         = acc" },
			{ "ffoldr(f, acc, list(x, xs...)) = f(x, ffoldr(f, acc, list(xs...)))" },

			{ "pair('test_'(ws..., a, b, c, xs...), 'test'(ys..., a, b, c, zs...)) = pair('found'(ws..., a, b, c, xs...), 'found'(ys..., a, b, c, zs...))" },
			{ "pair(a + b, list(b, a)) = 'success'('a_is', a, 'and_b_is', b)" },
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
				//std::cout << " = " << term.to_string() << "\n\n";
				term.head = simp::greedy_apply_ruleset(rules, term.mut_ref());
				std::cout << " = " << term.to_string() << "\n\n";
			}
			catch (bmath::ParseFailure failure) {
				std::cout << "parse failure: " << failure.what << '\n';
				std::cout << name << '\n';
				std::cout << std::string(failure.where, ' ') << "^\n\n";
			}
			catch (simp::TypeError error) {
				std::cout << "type error: " << error.what << "\n";
				std::cout << simp::print::to_string(error.occurence) << "\n";
			}
		}
	}
	//debug::enumerate_type();
	//bmath::intern::debug::test_rechner();
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








