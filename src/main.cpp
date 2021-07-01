
#include <array>


#include "utility/typeDebug.hpp"
#include "stupidTests.hpp"

#include "types.hpp"
#include "io.hpp"
#include "rewrite.hpp"
#include "control.hpp"

#include "utility/queue.hpp"

/*
TODO:

important:
 - is find_backtrack sufficient if associativity is respected by find_dilation?
 - test if combining match_ and rematch brings performance improvements
 - move callstack to heap in recursive functions iterating over terms
      - normalize::recursive (alterantively add depth limit for lambda normalisation)
	  - greedy_apply_ruleset
	  - print::append_to_string
	  - (perhaps) print::to_memory_layout

 - finish match:
	  - eval_value_match
	  - somehow enable "a^2 + 2 a b + b^2" to match "(x y)^2 + 2 x y z + z^2", for an arbitrary ammount of factors like "x" and "y"
 - fix when match returns std::strong_ordering::unordered, by utilizing FAppInfo::always_preceeding_next as 
		bool for MatchStrategy::backtracking and MatchStrategy::linear (perhaps rename accordingly?)
 - type checking (extended: keep track of what restrictions apply to match variable in lhs, use in rhs)
 - finnish building / verifying pattern:
      - (depends on type checking) verify whole patterns
      - check PatternFApp to each not exceed maximal length and check numer of PatternFApp in pattern not more than allowed 
	  - enable implicit outer multi
	  - allow only function calls returning bool in test_condition 
 - add Literal::options to allow nondeterministic match 
 - tests

nice to have:
 - to_tree
 - implement fst, snd, filter, split, ...
 - add eval_native for min/max to remove values guaranteed to not be minimum / maximum without requiring full evaluation
 - add neutral element to associative functions
 - add "dont care" pattern
 - enable StupidBufferVector to handle non-trivial destructible types -> change name to BufferVector
 - automate error checking in stupidTests.hpp -> change name to tests.hpp
 - noexceptify everything
 - restructure everything to use modules (basically needed to constexprfy all the things)
 - allow to restrict a variables domain (add to Names vector?)

idea status:
 - achieve feature parity between compile time pattern and run time pattern (add value match to ct)
 - introduce special condition "submatch" , where "submatch(x, ???, pat)" tries to match pat in parts of x, where the parts can be chosen using ??? in some way
 - allow PatternFApp in rhs to indicate, that a call of lhs can be "stolen" e.g. the parameter count in rhs is guaranteed to be lower than the one in lhs -> the old allocation can be reused
 - let each RewriteRule have a name in form of a constant c-string (not what pattern is constructed from, but name of rule, e.g. "differentiation: product rule" or something)
 - always keep -1 at some known index in store and never allocate new -1 in build_negated and build_inverted (problem: identify bevore copy, solution: you know the index)
 - change macros in meta_pn to create function objects, not functions 
 - build meta_pn::match not from pattern encoded as type, but pattern encoded as value by taking lamdas returning pattern part as template parameter
*/


int main()
{
	//bmath::intern::debug::enumerate_type();
	//bmath::intern::debug::test_rechner();
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
			{ "(\\x .\\y .x(y))(\\x .\\y .x(y))(\\x .x)(jens)" },
		});
		constexpr bool show = false;
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
			{ "0^_x    = 0" },
			{ "_x^0    = 1" },
			{ "_x^1    = _x" },
			
			{ "(_x^_a)^_b        = _x^(_a _b)" },
			{ "_x _x             = _x^2" },
			{ "_x _x^_a          = _x^(_a + 1)" },
			{ "_x^_a _x^_b       = _x^(_a + _b)" },
			{ "exp(ln(_y) xs...) = _y^(prod(xs...))" },
			
			{ "_a^2 + 2 _a _b + _b^2 = (_a + _b)^2" },
			{ "_a^2 - 2 _a _b + _b^2 = (_a - _b)^2" },
			{ "$a^2 + 2 $a _b + _b^2 = ($a + _b)^2" },
			{ "$a^2 - 2 $a _b + _b^2 = ($a - _b)^2" },
			{ "_aPow2 + _2a _b + _b^2 | 2 sqrt(_aPow2) == _2a = (1/2 _2a + _b)^2" },
			
			{ "_a bs... + _a cs... | !(_a :complex)         = _a (prod(bs...) + prod(cs...))" },
			{ "_a bs... + _a       | !(_a :complex)         = _a (prod(bs...) + 1)" },
			{ "_a       + _a       | !(_a :complex)         = 2 _a" },
			{ "_a _b               |   _a :complex, _b :sum = map(sum, \\x ._a x, _b)" },
			
			{ "-_a     | _a :sum  = map(sum , \\x. -x    , _a)" },
			{ "_a^(-1) | _a :prod = map(prod, \\x. x^(-1), _a)" },
			
			{ "sin(_x)^2 + cos(_x)^2 = 1" },
			
			//roots and extreme points of sin and cos:
			//{ "cos(             pi)           = -1" },
			//{ "cos(($k + 0.5)   pi) | $k :int =  0" },
			//{ "cos((2 $k)       pi) | $k :int =  1" },
			//{ "cos((2 $k + 1)   pi) | $k :int = -1" },

			{ "cos(   pi)                     = -1" },
			{ "cos(_a pi) | _a + 1/2     :int =  0" },
			{ "cos(_a pi) | _a / 2       :int =  1" },
			{ "cos(_a pi) | (_a - 1) / 2 :int = -1" },
			{ "sin(             pi)           =  0" },
			{ "sin($k           pi) | $k :int =  0" },
			{ "sin((2 $k + 0.5) pi) | $k :int =  1" },
			{ "sin((2 $k + 1.5) pi) | $k :int = -1" },
			
			//differentiation rules:
			{ "diff(_x, _x)                          = 1" },
			{ "diff(_a, _x)      | !contains(_a, _x) = 0" },
			{ "diff(_f^_a, _x)   | !contains(_a, _x) = diff(_f, _x) _a _f^(_a - 1)" },
			{ "diff(_a^_f, _x)   | !contains(_a, _x) = diff(_f, _x) ln(_a) _a^_f" },
			{ "diff(_g^_h, _x)                       = (diff(_h, _x) ln(_g) + _h diff(_g, _x) / _g) _g^_h" },
			{ "diff(_a, _x)      | _a :sum           = map(sum, \\f .diff(f, _x), _a)" },
			{ "diff(_u vs..., _x)                    = diff(_u, _x) vs... + _u diff(prod(vs...), _x)" },
			{ "diff(_f(_y), _x)                      = diff(_y, _x) fdiff(_f)(_y)" },
			
			{ "fdiff(\\x ._y) = \\x .diff(_y, x)" },
			{ "fdiff(sin)     = cos" },
			{ "fdiff(cos)     = \\x .-sin(x)" },
			{ "fdiff(exp)     = exp" },
			{ "fdiff(ln)      = \\x .x^(-1)" },
			{ "fdiff(tan)     = \\x .cos(x)^(-2)" },
			
			//exponential runtime fibonacci implementation:
			{ "fib(_n) | _n >= 0 = (_n < 2)(_n, fib(_n - 1) + fib(_n - 2))" },
			
			//reversing parameters in application of f:
			{ "reverse(_f, _f(xs...))              = reverse_h(_f(), _f(xs...))" },
			{ "reverse_h(_f(xs...), _f(_y, ys...)) = reverse_h(_f(_y, xs...), _f(ys...))" },
			{ "reverse_h(_fxs, _f())               = _fxs" },
			
			//listing first n fibonacci numbers:
			{ "fib_n(_n)  |  _n >= 2                      = reverse(tup, list_fibs(_n - 2, tup(1, 0)))" },
			{ "list_fibs(_n, tup(_a, _b, bs...)) | _n > 0 = list_fibs(_n - 1, tup(_a + _b, _a, _b, bs...))" },
			{ "list_fibs(_n, _res)                        = _res" },

			{ "filter(_f, _p, _f(xs...)) = take_true(_f(), map(_f, \\x .pair(_p(x), x), _f(xs...)))" },
			{ "take_true(_f(xs...), _f(pair(true, _x), ys...))   = take_true(_f(xs..., _x), _f(ys...))" },
			{ "take_true(_f(xs...), _f(pair(_     , _x), ys...)) = take_true(_f(xs...), _f(ys...))" },
			{ "take_true(_fxs, _f())                             = _fxs" },

			{ "split(_f, _p, _f(xs...)) = split_hlp(_f(), _f(), map(_f, \\x .pair(_p(x), x), _f(xs...)))" },
			{ "split_hlp(_f(xs...), _f(ys...), _f(pair(true, _z), zs...)) = split_hlp(_f(xs..., _z), _f(ys...), _f(zs...))" },
			{ "split_hlp(_f(xs...), _f(ys...), _f(pair(_   , _z), zs...)) = split_hlp(_f(xs...), _f(ys..., z), _f(zs...))" },
			{ "split_hlp(_fxs, _fys, _f())                                = pair(_fxs, _fys)" },

			{ "sort(tup())                               = tup()" },
			{ "sort(tup(_x))                             = tup(_x)" },
			{ "sort(tup(_x, xs...))                      = sort_h1(split(tup, \\y .y < _x, tup(xs...)), _x)" },
			{ "sort_h1(pair(tup(xs...), tup(ys...)), _x) = sort_h2(sort(tup(xs...)), _x, sort(tup(ys...)))" },
			{ "sort_h2(tup(xs...), _x, tup(ys...))       = tup(xs..., _x, ys...)" },

			{ "union(set(xs...), set(ys...)) = set(xs..., ys...)" },
			{ "union()                       = set()" },

			{ "intersection(set(_x, xs...), set(_x, ys...)) = union(set(_x), intersection(set(xs...), set(ys...)))" },
			{ "intersection(_x, xs...)                      = set()" },

			{ "min(_x, _y) | _x > _y = _y" },
			{ "max(_x, _y) | _x > _y = _x" },

			{ "foldr(_f, _g, _acc, _f())          = _acc" },
			{ "foldr(_f, _g, _acc, _f(_x, xs...)) = _g(_x, foldr(_f, _g, _acc, _f(xs...)))" },

			{ "make_ints(tup, _a, _b)              | _a <= _b = make_ints_tup_h(_b, tup(_a))" },
			{ "make_ints_tup_h(_b, tup(xs..., _x)) | _x < _b  = make_ints_tup_h(_b, tup(xs..., _x, _x + 1))" },
			{ "make_ints_tup_h(_b, _res)                      = _res" },

			{ "make_ints(cons, _a, _b)      | _a <= _b  = make_ints_cons_h(_b - _a, _b :: null)" },
			{ "make_ints_cons_h(_n, _y :: _ys) | _n > 0 = make_ints_cons_h(_n - 1, (_y - 1) :: _y :: _ys)" },
			{ "make_ints_cons_h(_n, _res)               = _res" },

			{ "change(_f, _g, _f(xs...)) = _g(xs...)" },

			//experiments
			{ "teeest(tup(_a + _b, cs...), tup(_b, _a)) = teEsT(_a, _b, cs...)" },
			{ "tup(xs..., 1, _x) = _x" },
		};
		for (const simp::RuleRef rule : rules) {
			std::cout << rule.to_string() << "\n\n";
		}
		std::cout << "\n";

		bool exact = true;
		bool show_memory = false;
		bool show_tree = false;
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
			if (name == "--not show tree") {
				show_memory = false;
				std::cout << "set show_tree to false\n\n";
				continue;
			}
			if (name == "--show tree") {
				show_memory = true;
				std::cout << "set show_tree to true\n\n";
				continue;
			}
			try {
				auto term = simp::LiteralTerm(name);
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
	if (false) {
		const simp::RuleSet rules = {
			{ "_a^2   +  2 _a _b + _b^2 =  (_a + _b)^2" },
			{ "_a^2   -  2 _a _b + _b^2 =  (_a - _b)^2" },
			{ "_a bs... + _a cs...      = _a (prod(bs...) + prod(cs...))" },
			{ "_a bs... + _a            = _a (prod(bs...) + 1)" },
			{ "_a       + _a            = 2 _a" },
		};

		const auto weight_fun = [](const simp::UnsaveRef r) {
			using namespace simp;
			int count = 0;
			(void)ctrl::search(r.store_data(), r.typed_idx(), [&](auto) { count++; return false; });
			return count;
		};

		auto term = simp::LiteralTerm("a b c d + a b c d e + b c d e + c d e");
		term.normalize({});
		std::vector<simp::NodeIndex> normals = simp::nondeterministic_shallow_apply_ruleset(rules, term.mut_ref(), {});
		for (const auto n : normals) {
			std::cout << simp::print::to_string(term.ref().at(n)) << "\n";
		}
	}
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








