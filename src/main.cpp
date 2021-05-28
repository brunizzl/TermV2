
#include <array>


#include "utility/typeDebug.hpp"
#include "stupidTests.hpp"

#include "types.hpp"
#include "io.hpp"
#include "rewrite.hpp"

#include "utility/queue.hpp"

/*
TODO:

important:
 - move callstack to heap in recursive functions iterating over terms
 - finish match:
	  - eval_value_match
 - only test subset of rulerange (requires rule iterator to become random access)
	  - do one binary search to find begin of maybe matching range, continue trying to match until match returns "less"
 - type checking (extended: keep track of what restrictions apply to match variable in lhs, use in rhs)
 - finnish building / verifying pattern:
      - (depends on type checking) verify whole patterns
      - check PatternFApp to each not exceed maximal length and check numer of PatternFApp in pattern not more than allowed 
	  - enable implicit outer multi
	  - allow only function calls returning bool in test_condition 
 - add Literal::options to allow nondeterministic match 
 - tests

nice to have:
 - implement fst, snd, filter, split, ...
 - add eval_native for min/max to remove values guaranteed to not be minimum / maximum without requiring full evaluation
 - add neutral element to associative functions
 - add "dont care" pattern
 - implement meta_pn::match function for variadic patterns
 - achieve feature parity between compile time pattern and run time pattern (add value match to ct)
 - enable StupidBufferVector to handle non-trivial destructible types -> change name to BufferVector
 - automate error checking in stupidTests.hpp -> change name to tests.hpp
 - noexceptify everything
 - restructure everything to use modules (basically needed to constexprfy all the things)
 - allow to restrict a variables domain (add to Names vector?)

idea status:
 - introduce special condition "submatch" , where "submatch(x, ???, pat)" tries to match pat in parts of x, where the parts can be chosen using ??? in some way
 - allow PatternFApp in rhs to indicate, that a call of lhs can be "stolen" e.g. the parameter count in rhs is guaranteed to be lower than the one in lhs -> the old allocation can be reused
 - let each RewriteRule have a name in form of a constant c-string (not what pattern is constructed from, but name of rule, e.g. "differentiation: product rule" or something)
 - always keep -1 at some known index in store and never allocate new -1 in build_negated and build_inverted (problem: identify bevore copy, solution: you know the index)
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
	{
		const auto names = std::to_array<std::string>({
			{ "tup(conj(3-1i), conj(4+3i), conj(8), conj(-1i))" },
			{ "tup(3 == 4, 3 != 4, 3 < 4, 3 <= 4, 3 >= 4, 3 > 4)" },
			{ "tup(4 == 4, 4 != 4, 4 < 4, 4 <= 4, 4 >= 4, 4 > 4)" },
			{ "tup(x == 4, x != 4, x < 4, x <= 4, x >= 4, x > 4)" },
			{ "tup(floor(4.2), floor(3.9), floor(2-1i), ceil(3.9), ceil(4), ceil(4+1i))" },
			{ "set(1, -4, 5, a, 12, 13+3i, 13+4i, 13-1i, 13, 13+1i, b, -10)" },
			{ "true(3, 4)" },
			{ "false(3, 4^2)" },
			{ "\\x.\\y.\\z. x + y + z" },
			{ "a + b + 3 + c + 1 + 6 + a" },
			{ "(\\x y. x y)(a, 4)" },
			{ "(\\x y z. tup(x, y, z))(a, 4, sin(x))" },
			{ "4^(0.5)" },
			{ "2^(0.5)" },
			{ "map(sum, \\x. -x, sum(a, b, sin(x), 3, 5))" },
			{ "berb && frobbl && true && (false || !false || schmenck) && true && !alf" },
			{ "10/5" },
			{ "set(1, 100, a, b, 50 + 2 * 25, a, (\\x.2 x)(50))" },
			{ "(\\f n. f(f, n))(\\f n.(n <= 1)(1, n * f(f, -1 + n)), 5)" },
			{ "(\\f n. f(f, n))(\\f n.(n == 1)(1, n * f(f, -1 + n)), 5)" },
			{ "(\\x .\\y .x(y))(\\x .\\y .x(y))(\\x .x)" },
		});
		constexpr bool show = true;
		for (const auto& name : names) {
			if (show) std::cout << name << "\n";
			auto term = simp::LiteralTerm(name);
			if (show) std::cout << "  ->  " << term.to_string() << "\n";
			if (show) std::cout << term.to_memory_layout() << "\n";
			term.normalize({});
			if (show) std::cout << "  ->  " << term.to_string() << "\n";
			if (show) std::cout << term.to_memory_layout() << "\n";
			if (show) std::cout << "\n";

			assert((simp::free_tree(term.mut_ref()), term.store.nr_used_slots() == 0u));
		}
		std::cout << "\n\n\n";
	}
	if (true) {
		const simp::RuleSet rules = {
			{ "0 xs... = 0" },
			{ "0^x     = 0" },
			{ "x^0     = 1" },
			{ "x^1     = x" },
			
			{ "(x^a)^b = x^(a b)" },
			{ "x x     = x^2" },
			{ "x x^a   = x^(a + 1)" },
			{ "x^a x^b = x^(a + b)" },
			{ "'exp'(ln(y) xs...) = y^('prod'(xs...))" },
			
			{ " a^2   +  2 a b + b^2 =  (a + b)^2" },
			{ " a^2   -  2 a b + b^2 =  (a - b)^2" },
			{ " aPow2 +  _2a b + b^2 | 2 'sqrt'(aPow2) == _2a =  (1/2 _2a + b)^2" },
			{ "$a^2 + 2 $a b + b^2 = ($a + b)^2" },
			
			{ "a bs... + a cs... | !(a :'complex')          = a ('prod'(bs...) + 'prod'(cs...))" },
			{ "a bs... + a       | !(a :'complex')          = a ('prod'(bs...) + 1)" },
			{ "a       + a       | !(a :'complex')          = 2 a" },
			{ "a b               |   a :'complex', b :'sum' = 'map'('sum', \\x .a x, b)" },
			
			{ "-a     | a :'sum'     = 'map'('sum' , \\x. -x    , a)" },
			{ "a^(-1) | a :'prod'    = 'map'('prod', \\x. x^(-1), a)" },
			
			{ "'sin'(x)^2 + 'cos'(x)^2 = 1" },
			
			//roots and extreme points of sin and cos:
			//{ "'cos'(             'pi')             = -1" },
			//{ "'cos'(($k + 0.5)   'pi') | $k :'int' =  0" },
			//{ "'cos'((2 $k)       'pi') | $k :'int' =  1" },
			//{ "'cos'((2 $k + 1)   'pi') | $k :'int' = -1" },
			//{ "'sin'(             'pi')             =  0" },
			//{ "'sin'($k           'pi') | $k :'int' =  0" },
			//{ "'sin'((2 $k + 0.5) 'pi') | $k :'int' =  1" },
			//{ "'sin'((2 $k + 1.5) 'pi') | $k :'int' = -1" },

			{ "'cos'(  'pi')                      = -1" },
			{ "'cos'(a 'pi') | a + 1/2     :'int' =  0" },
			{ "'cos'(a 'pi') | a / 2       :'int' =  1" },
			{ "'cos'(a 'pi') | (a - 1) / 2 :'int' = -1" },
			{ "'sin'(             'pi')             =  0" },
			{ "'sin'($k           'pi') | $k :'int' =  0" },
			{ "'sin'((2 $k + 0.5) 'pi') | $k :'int' =  1" },
			{ "'sin'((2 $k + 1.5) 'pi') | $k :'int' = -1" },
			
			//differentiation rules:
			{ "'diff'(x, x)                           = 1" },
			{ "'diff'(a, x)       | !'contains'(a, x) = 0" },
			{ "'diff'(f^a, x)     | !'contains'(a, x) = 'diff'(f, x) a f^(a-1)" },
			{ "'diff'(a^f, x)     | !'contains'(a, x) = 'diff'(f, x) 'ln'(a) a^f" },
			{ "'diff'(g^h, x)                         = ('diff'(h, x) 'ln'(g) + h 'diff'(g, x)/g) g^h" },
			{ "'diff'(a, x)       | a :'sum'          = 'map'('sum', \\f .'diff'(f, x), a)" },
			{ "'diff'(u vs..., x)                     = 'diff'(u, x) vs... + u 'diff'('prod'(vs...), x)" },
			{ "'diff'(f(y), x)                        = 'diff'(y, x) 'fdiff'(f)(y)" },
			
			{ "'fdiff'(\\x .y) = \\x .'diff'(y, x)" },
			{ "'fdiff'('sin')    = 'cos'" },
			{ "'fdiff'('cos')    = \\x .-'sin'(x)" },
			{ "'fdiff'('exp')    = 'exp'" },
			{ "'fdiff'('ln')     = \\x .x^(-1)" },
			{ "'fdiff'('tan')    = \\x .'cos'(x)^(-2)" },
			
			//exponential runtime fibonacci implementation:
			{ "'fib'(n) | n >= 0 = (n < 2)(n, 'fib'(n - 1) + 'fib'(n - 2))" },
			
			//reversing parameters in application of f:
			{ "'reverse'(f, f(xs...)) = 'reverse_h'(f(), f(xs...))" },
			{ "'reverse_h'(f(xs...), f(y, ys...)) = 'reverse_h'(f(y, xs...), f(ys...))" },
			{ "'reverse_h'(fxs, f())              = fxs" },
			
			//listing first n fibonacci numbers:
			{ "'fib_n'(n)  |  n >= 2                       = 'reverse'('tup', 'list_fibs'(n - 2, 'tup'(1, 0)))" },
			{ "'list_fibs'(n, 'tup'(a, b, bs...)) | n > 0 = 'list_fibs'(n - 1, 'tup'(a + b, a, b, bs...))" },
			{ "'list_fibs'(n, res)                         = res" },
			
			{ "'filter'(f, p, f(xs...)) = 'take_true'(f(), 'map'(f, \\x .'pair'(p(x), x), f(xs...)))" },
			{ "'take_true'(f(xs...), f('pair'('true', x), ys...)) = 'take_true'(f(xs..., x), f(ys...))" },
			{ "'take_true'(f(xs...), f('pair'(_     , x), ys...)) = 'take_true'(f(xs...), f(ys...))" },
			{ "'take_true'(fxs, f())                              = fxs" },
			
			{ "'split'(f, p, f(xs...)) = 'split_hlp'(f(), f(), 'map'(f, \\x .'pair'(p(x), x), f(xs...)))" },
			{ "'split_hlp'(f(xs...), f(ys...), f('pair'('true', z), zs...)) = 'split_hlp'(f(xs..., z), f(ys...), f(zs...))" },
			{ "'split_hlp'(f(xs...), f(ys...), f('pair'(_   , z), zs...))   = 'split_hlp'(f(xs...), f(ys..., z), f(zs...))" },
			{ "'split_hlp'(fxs, fys, f())                                   = 'pair'(fxs, fys)" },
			
			{ "'sort'('tup'())                                   = 'tup'()" },
			{ "'sort'('tup'(x))                                  = 'tup'(x)" },
			{ "'sort'('tup'(x, xs...))                           = 'sort_h1'('split'('tup', \\y .y < x, 'tup'(xs...)), x)" },
			{ "'sort_h1'('pair'('tup'(xs...), 'tup'(ys...)), x) = 'sort_h2'('sort'('tup'(xs...)), x, 'sort'('tup'(ys...)))" },
			{ "'sort_h2'('tup'(xs...), x, 'tup'(ys...))         = 'tup'(xs..., x, ys...)" },
			
			{ "'union'('set'(xs...), 'set'(ys...)) = 'set'(xs..., ys...)" },
			{ "'union'()                           = 'set'()" },
			
			{ "'intersection'('set'(x, xs...), 'set'(x, ys...)) = 'union'('set'(x), 'intersection'('set'(xs...), 'set'(ys...)))" },
			{ "'intersection'(x, xs...)                         = 'set'()" },
			
			{ "'min'(x, y) | x > y = y" },
			{ "'max'(x, y) | x > y = x" },
			
			{ "'foldr'(f, g, acc, f())         = acc" },
			{ "'foldr'(f, g, acc, f(x, xs...)) = g(x, 'foldr'(f, g, acc, f(xs...)))" },
			
			{ "'pair'('test_'(ws..., a, b, c, xs...), 'test_'(ys..., a, b, c, zs...)) = 'tup'(ws..., 'found'(a, b, c), xs..., '_space_', ys..., 'found'(a, b, c), zs...)" },
			{ "'pair'(a + b, 'tup'(b, a)) = 'success'('a_is', a, 'and_b_is', b)" },
			
			{ "'make_ints'('tup', a, b)              | a <= b = 'make_ints_tup_h'(b, 'tup'(a))" },
			{ "'make_ints_tup_h'(b, 'tup'(xs..., x)) | x < b  = 'make_ints_tup_h'(b, 'tup'(xs..., x, x + 1))" },
			{ "'make_ints_tup_h'(b, res)                      = res" },

			{ "'make_ints'('cons', a, b)      | a <= b = 'make_ints_cons_h'(b - a, b :: 'null')" },
			{ "'make_ints_cons_h'(n, y :: ys) | n > 0  = 'make_ints_cons_h'(n - 1, (y - 1) :: y :: ys)" },
			{ "'make_ints_cons_h'(n, res)              = res" },
			
			{ "'change'(f, g, f(xs...)) = g(xs...)" },
		};
		for (const simp::RuleRef rule : rules) {
			std::cout << rule.to_string() << "\n\n";
		}
		std::cout << "\n";

		bool exact = true;
		bool show_memory = false;
		while (true) {
			std::string name;
			std::cout << "simp> ";
			std::getline(std::cin, name);
			if (name == "--not exact") {
				exact = false;
				std::cout << "set exact to false\n\n";
				continue;
			}
			if (name == "--exact") {
				exact = true;
				std::cout << "set exact to true\n\n";
				continue;
			}
			if (name == "--not show memory") {
				show_memory = false;
				std::cout << "set show_memory to false\n\n";
				continue;
			}
			if (name == "--show memory") {
				show_memory = true;
				std::cout << "set show_memory to true\n\n";
				continue;
			}
			try {
				auto term = simp::LiteralTerm(name);
				//std::cout << " = " << term.to_string() << "\n\n";
				std::cout << term.to_memory_layout() << "\n\n\n";
				term.normalize({ .exact = exact });
				term.head = simp::greedy_apply_ruleset(rules, term.mut_ref(), { .exact = exact });
				std::cout << " = " << term.to_string() << "\n\n";
				if (show_memory) std::cout << term.to_memory_layout() << "\n\n\n";

				assert((simp::free_tree(term.mut_ref()), term.store.nr_used_slots() == 0u));
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








