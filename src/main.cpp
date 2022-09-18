
#include "test.hpp"

/*
TODO:

important:
 - current strategy of ordering ruleset with stable sort, then binary searching is not sufficient: 
		p1 = "pow[l](_X0, _X1)" can remain in order behind p2 = "pow[l](_X0, 1)", 
		  despite cases, where literal is larger than p1, but would match p2.
 - is find_backtrack sufficient if associativity is respected by find_dilation?
 - test if combining match_ and rematch brings performance improvements (allows inlining of the different strategies)
 - move callstack to heap in recursive functions iterating over terms
      - normalize::recursive (alterantively add depth limit for lambda normalisation)
	  - free_tree
	  - (perhaps) print::to_memory_layout

 - fix when match returns std::strong_ordering::unordered, by utilizing FAppInfo::always_preceeding_next as 
		bool for MatchStrategy::identic and MatchStrategy::linear (perhaps rename accordingly?)
 - type checking (extended: keep track of what restrictions apply to match variable in lhs, use in rhs)
 - finnish building / verifying pattern:
      - (depends on type checking) verify whole patterns
      - check PatternFApp to each not exceed maximal length and check numer of PatternFApp in pattern not more than allowed 
	  - allow only function calls returning bool in test_condition 
	  - don't allow the same name (minus the oligatory "_") as both match variable and constant in a pattern
 - add Literal::options to allow nondeterministic match 

nice to have:
 - order the most general rule in a subset to the end of that subset
 - to_tree
 - implement fst, snd, filter, split, ...
 - add neutral element to associative functions
 - add "dont care" pattern
 - enable StupidBufferVector to handle non-trivial destructible types -> change name to BufferVector
 - noexceptify everything
 - restructure everything to use modules (basically needed to constexprfy all the things)
 - allow to restrict a variables domain (add to Names vector?)

idea status:
 - introduce special condition "submatch" , where "submatch(x, ???, pat)" tries to match pat in parts of x, where the parts can be chosen using ??? in some way
 - allow PatternFApp in rhs to indicate, that a call of lhs can be "stolen" e.g. the parameter count in rhs is guaranteed to be lower than the one in lhs -> the old allocation can be reused
 - let each RewriteRule have a name in form of a constant c-string (not what pattern is constructed from, but name of rule, e.g. "differentiation: product rule" or something)
 - always keep -1 (and maybe others) at some known index in store and never allocate new -1 in build_negated and build_inverted (problem: identify bevore copy, solution: you know the index)
*/


int main()
{
	simp::test::print_node_types();
	simp::test::print_native_symbols();
	simp::test::run_tests();
		
	simp::test::read_eval_print_loop({
		{ "0 xs... = 0" },
		{ "0^_x    = 0" },
		{ "_x^0    = 1" },
		{ "_x^1    = _x" },

		{ "(_x^_a)^_b        = _x^(_a _b)" },
		{ "_x _x ys...       = _x^2 ys..." },
		{ "_x _x^_a ys...    = _x^(_a + 1) ys..." },
		{ "_x^_a _x^_b ys... = _x^(_a + _b) ys..." },
		{ "exp(ln(_y) xs...) = _y^(prod(xs...))" },

		{ "_a^2 + 2 _a _b + _b^2 + cs... = (_a + _b)^2 + cs..." },
		{ "_a^2 - 2 _a _b + _b^2 + cs... = (_a - _b)^2 + cs..." },
		{ "$a^2 + 2 $a _b + _b^2 + cs... = ($a + _b)^2 + cs..." },
		{ "$a^2 - 2 $a _b + _b^2 + cs... = ($a - _b)^2 + cs..." },
		//{ "_aPow2 + _2a _b + _b^2 | 4 _aPow2 == _2a^2 = (1/2 _2a + _b)^2" },

		{ "_a bs... + _a cs... + ds... | !(_a :complex)         = _a (prod(bs...) + prod(cs...)) + ds..." },
		{ "_a bs... + _a       + ds... | !(_a :complex)         = _a (prod(bs...) + 1) + ds..." },
		{ "_a       + _a       + ds... | !(_a :complex)         = 2 _a + ds..." },
		{ "_a _b               + ds... |   _a :complex, _b :sum = map(sum, \\x ._a x, _b) + ds..." },

		{ "-_a     | _a :sum  = map(sum , \\x. -x    , _a)" },
		{ "_a^(-1) | _a :prod = map(prod, \\x. x^(-1), _a)" },

		{ "sin(_x)^2 + cos(_x)^2 + ys... = 1 + ys..." },

		//roots and extreme points of sin and cos:
		{ "cos(             pi)           = -1" },
		{ "cos(($k + 0.5)   pi) | $k :int =  0" },
		{ "cos((2 $k)       pi) | $k :int =  1" },
		{ "cos((2 $k + 1)   pi) | $k :int = -1" },
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

		//operations on sets:
		{ "union(set(xs...), set(ys...), sets...) = union(set(xs..., ys...), sets...)" },
		{ "union()                       = set()" },
		{ "intersection(set(_x, xs...), set(_x, ys...), sets...) = intersection(union(set(_x), intersection(set(xs...), set(ys...))), sets...)" },
		{ "intersection(_x, xs...)                               = set()" },

		{ "min(_x, _y, zs...) | _x >= _y = min(_y, zs...)" },
		{ "max(_x, _y, zs...) | _x >= _y = max(_x, zs...)" },

		//test stuff
		{ "make_int_tup(_a, _b)  | _a <= _b = gen(_a, \\x .x + 1, _b - _a)" },
		{ "make_int_list(_a, _b) | _a <= _b  = make_int_list_helper(_b - _a, _b :: null)" },
		{ "make_int_list_helper(_n, _y :: _ys) | _n > 0 = make_int_list_helper(_n - 1, (_y - 1) :: _y :: _ys)" },
		{ "make_int_list_helper(_n, _res)               = _res" },

		{ "change(_f, _g, _f(xs...)) = _g(xs...)" },

		//solving an equation for _x (no multiple occurences of _x are considered)
		{ "solve(_a, _b, _x) = solve_helper(_a - _b, 0, _x)" },
		{ "solve_helper(_x, _y, _x) = _y" },
		{ "solve_helper(_a + bs..., _y, _x) | !contains(_a, _x) = solve_helper(sum(bs...) , _y - _a    , _x)" },
		{ "solve_helper(_a bs...  , _y, _x) | !contains(_a, _x) = solve_helper(prod(bs...), _y / _a    , _x)" },
		{ "solve_helper(_a^_b     , _y, _x) | !contains(_a, _x) = solve_helper(_b         , log(_a, _y), _x)" },
		{ "solve_helper(_b^_a     , _y, _x) | !contains(_a, _x) = solve_helper(_b         , _y^(1 / _a), _x)" },
	});
}








