#pragma once

#include <iostream>
#include <charconv>
#include <cassert>
#include <numeric>
#include <array>

#include "utility/bit.hpp"
#include "utility/typeDebug.hpp"
#include "utility/algorithm.hpp"

#include "termStore.hpp"
#include "arithmeticTerm.hpp"
#include "parseTerm.hpp"
#include "ioArithmetic.hpp"
#include "termVector.hpp"
#include "pattern.hpp"
#include "transform.hpp"
#include "metaMatch.hpp"


namespace bmath::intern::debug {

	void enumerate_type()
	{
		using namespace pattern;
		std::cout
			<< "PnType(Fn::pow)                          = " << unsigned(PnType(Fn::pow))                          << "\n"
			<< "PnType(Fn::log)                          = " << unsigned(PnType(Fn::log))                          << "\n"
			<< "PnType(Fn::sqrt)                         = " << unsigned(PnType(Fn::sqrt))                         << "\n"
			<< "PnType(Fn::exp)                          = " << unsigned(PnType(Fn::exp))                          << "\n"
			<< "PnType(Fn::ln)                           = " << unsigned(PnType(Fn::ln))                           << "\n"
			<< "PnType(Fn::sin)                          = " << unsigned(PnType(Fn::sin))                          << "\n"
			<< "PnType(Fn::cos)                          = " << unsigned(PnType(Fn::cos))                          << "\n"
			<< "PnType(Fn::tan)                          = " << unsigned(PnType(Fn::tan))                          << "\n"
			<< "PnType(Fn::sinh)                         = " << unsigned(PnType(Fn::sinh))                         << "\n"
			<< "PnType(Fn::cosh)                         = " << unsigned(PnType(Fn::cosh))                         << "\n"
			<< "PnType(Fn::tanh)                         = " << unsigned(PnType(Fn::tanh))                         << "\n"
			<< "PnType(Fn::asin)                         = " << unsigned(PnType(Fn::asin))                         << "\n"
			<< "PnType(Fn::acos)                         = " << unsigned(PnType(Fn::acos))                         << "\n"
			<< "PnType(Fn::atan)                         = " << unsigned(PnType(Fn::atan))                         << "\n"
			<< "PnType(Fn::asinh)                        = " << unsigned(PnType(Fn::asinh))                        << "\n"
			<< "PnType(Fn::acosh)                        = " << unsigned(PnType(Fn::acosh))                        << "\n"
			<< "PnType(Fn::atanh)                        = " << unsigned(PnType(Fn::atanh))                        << "\n"
			<< "PnType(Fn::abs)                          = " << unsigned(PnType(Fn::abs))                          << "\n"
			<< "PnType(Fn::arg)                          = " << unsigned(PnType(Fn::arg))                          << "\n"
			<< "PnType(Fn::re)                           = " << unsigned(PnType(Fn::re))                           << "\n"
			<< "PnType(Fn::im)                           = " << unsigned(PnType(Fn::im))                           << "\n"
			<< "PnType(Fn::force)                        = " << unsigned(PnType(Fn::force))                        << "\n"
			<< "PnType(Fn::diff)                         = " << unsigned(PnType(Fn::diff))                         << "\n\n"
			<< "PnType(NamedFn{})                        = " << unsigned(PnType(NamedFn{}))                        << "\n\n"
			<< "PnType(NonComm::list)                    = " << unsigned(PnType(NonComm::list))                    << "\n"
			<< "PnType(NonComm::ordered_sum)             = " << unsigned(PnType(NonComm::ordered_sum))             << "\n"
			<< "PnType(NonComm::ordered_product)         = " << unsigned(PnType(NonComm::ordered_product))         << "\n\n"
			<< "PnType(Comm::sum)                        = " << unsigned(PnType(Comm::sum))                        << "\n"
			<< "PnType(Comm::product)                    = " << unsigned(PnType(Comm::product))                    << "\n"
			<< "PnType(Comm::multiset)                   = " << unsigned(PnType(Comm::multiset))                   << "\n"
			<< "PnType(Comm::set)                        = " << unsigned(PnType(Comm::set))                        << "\n"
			<< "PnType(Comm::union_)                     = " << unsigned(PnType(Comm::union_))                     << "\n"
			<< "PnType(Comm::intersection)               = " << unsigned(PnType(Comm::intersection))               << "\n"
			<< "PnType(Comm::min)                        = " << unsigned(PnType(Comm::min))                        << "\n"
			<< "PnType(Comm::max)                        = " << unsigned(PnType(Comm::max))                        << "\n\n"
			<< "PnType(Literal::variable)                = " << unsigned(PnType(Literal::variable))                << "\n"
			<< "PnType(Literal::complex)                 = " << unsigned(PnType(Literal::complex))                 << "\n\n\n\n"
			<< "PnType(TreeMatchNonOwning{})             = " << unsigned(PnType(TreeMatchNonOwning{}))             << "\n\n"
			<< "PnType(Restriction::any)                 = " << unsigned(PnType(Restriction::any))                 << "\n"
			<< "PnType(Restriction::nn1)                 = " << unsigned(PnType(Restriction::nn1))                 << "\n"
			<< "PnType(Restriction::no_val)              = " << unsigned(PnType(Restriction::no_val))              << "\n"
			<< "PnType(Restriction::variable)            = " << unsigned(PnType(Restriction::variable))            << "\n\n"
			<< "PnType(TreeDomain(Domain::natural))      = " << unsigned(PnType(TreeDomain(Domain::natural)))      << "\n"
			<< "PnType(TreeDomain(Domain::natural_0))    = " << unsigned(PnType(TreeDomain(Domain::natural_0)))    << "\n"
			<< "PnType(TreeDomain(Domain::integer))      = " << unsigned(PnType(TreeDomain(Domain::integer)))      << "\n"
			<< "PnType(TreeDomain(Domain::negative))     = " << unsigned(PnType(TreeDomain(Domain::negative)))     << "\n"
			<< "PnType(TreeDomain(Domain::positive))     = " << unsigned(PnType(TreeDomain(Domain::positive)))     << "\n"
			<< "PnType(TreeDomain(Domain::not_negative)) = " << unsigned(PnType(TreeDomain(Domain::not_negative))) << "\n"
			<< "PnType(TreeDomain(Domain::not_positive)) = " << unsigned(PnType(TreeDomain(Domain::not_positive))) << "\n"
			<< "PnType(TreeDomain(Domain::real))         = " << unsigned(PnType(TreeDomain(Domain::real)))         << "\n"
			<< "PnType(TreeDomain(Domain::complex))      = " << unsigned(PnType(TreeDomain(Domain::complex)))      << "\n\n"
			<< "PnType(MultiMatch::fst)                  = " << unsigned(PnType(MultiMatch::fst))                  << "\n"
			<< "PnType(MultiMatch::snd)                  = " << unsigned(PnType(MultiMatch::snd))                  << "\n\n"
			<< "PnType(ValueProxy{})                     = " << unsigned(PnType(ValueProxy{}))                     << "\n"
			<< "PnType(ValueMatch::owning)               = " << unsigned(PnType(ValueMatch::owning))               << "\n"
			<< "PnType(ValueMatch::non_owning)           = " << unsigned(PnType(ValueMatch::non_owning))           << "\n\n\n"
		;
	} //enumerate_type

	void test_rechner() 
	{

		static const RuleSet rules = std::to_array<pattern::RewriteRule>({
			{ "x :product... | 0 x = 0" },
			{ "x             | 0^x = 0" },
			{ "x             | x^0 = 1" },
			{ "x             | x^1 = x" },

			{ "x, a, b          | (x^a)^b = x^(a*b)" },
			{ "x                | x x     = x^2" },
			{ "x, a             | x x^a   = x^(a + 1)" },
			{ "x, a, b          | x^a x^b = x^(a + b)" },
			{ "x :product..., y | exp(x ln(y)) = y^x" },

			{ "a, b          | a^2 + 2 a b + b^2 = (a + b)^2" },
			{ "a, b          | a^2 - 2 a b + b^2 = (a - b)^2" },
			{ "a :complex, b | a^2 + 2 a b + b^2 = (a + b)^2" },

			{ "a :no_val, bs :product..., cs :product... | a bs + a cs = a (bs + cs)" },
			{ "a :no_val, bs :product...                 | a bs + a    = a (bs + 1)" },
			{ "a :no_val                                 | a    + a    = 2 a" },
			{ "a :value, b, cs :sum...                   | a (b + cs)  = a b + a cs" },

			{ "a, as :sum...     |  -(a + as) =  -a - as" },
			{ "a, as :product... | 1/(a as)   = 1/a 1/as" },
			//{ " as :sum        | -as       =     sum:{ -a | a <- as}" }, //not yet writable, will perhaps never happen :|
			//{ " as :product    | 1/as      = product:{1/a | a <- as}" }, //not yet writable, will perhaps never happen :|

			{ "x | sin(x)^2 + cos(x)^2 = 1" },

			//roots and extreme points of sin and cos:
			{ "         cos(            'pi') = -1" },
			{ "k :int | cos((k + 0.5)   'pi') =  0" },
			{ "k :int | cos((2 k)       'pi') =  1" },
			{ "k :int | cos((2 k + 1)   'pi') = -1" },
			{ "         sin(            'pi') =  0" },
			{ "k :int | sin(k           'pi') =  0" },
			{ "k :int | sin((2 k + 0.5) 'pi') =  1" },
			{ "k :int | sin((2 k + 1.5) 'pi') = -1" },

			//differentiation rules:
			{ "x :variable                        | diff(x, x)      = 1" },
			{ "x :variable, a :variable           | diff(a, x)      = 0" },
			{ "x :variable, a :value              | diff(a, x)      = 0" },
			{ "x :variable, a :value, f :any      | diff(f^a, x)    = diff(f, x) a f^(a-1)" },
			{ "x :variable, a :value, f :any      | diff(a^f, x)    = diff(f, x) ln(a) a^f" },
			{ "x :variable, g :any, h :any        | diff(g^h, x)    = (diff(h, x) ln(g) + h diff(g, x)/g) g^h" },
			{ "x :variable, u :any, v :sum...     | diff(u + v, x)  = diff(u, x) + diff(v, x)" },
			{ "x :variable, u :any, v :product... | diff(u v, x)    = diff(u, x) v + u diff(v, x)" },
			{ "x :variable, f :any                | diff(sin(f), x) = diff(f, x) cos(f)" },
			{ "x :variable, f :any                | diff(cos(f), x) = diff(f, x) (-sin(f))" },
			{ "x :variable, f :any                | diff(exp(f), x) = diff(f, x) exp(f)" },
			{ "x :variable, f :any                | diff(ln(f), x)  = diff(f, x) 1/f" },

			//exponential runtime fibonacci implementation:
			{ "         fib(0) = 0" },
			{ "         fib(1) = 1" },
			{ "n :nat | fib(n) = fib(n - 1) + fib(n - 2)" },

			//reversing a list:
			{ "xs :list...                 | reverse(list{xs}) = reverse'(list{}, list{xs})" },
			{ "xs :list..., y, ys :list... | reverse'(list{xs}, list{y, ys}) = reverse'(list{y, xs}, list{ys})" },
			{ "xs :list...,                | reverse'(list{xs}, list{})      = list{xs}" },

			//listing first n fibonacci numbers:
			{ "n :nat0                     | fib_n(n + 2)                   = reverse(list_fibs(n, list{1, 0}))" },
			{ "n :nat, a, b, tail :list... | list_fibs(n, list{a, b, tail}) = list_fibs(n - 1, list{force(a + b), a, b, tail})" },
			{ "              tail :list... | list_fibs(0, list{tail})       = list{tail}" },

			//sorting numbers:
			{ "                     | sort(list{})      = list{}" },
			{ "x                    | sort(list{x})     = list{x}" },
			{ "p :real, xs :list... | sort(list{p, xs}) = concat3(sort(filter_s(p, list{}, list{xs})), list{p}, sort(filter_le(p, list{}, list{xs})))" },

			{ "xs :list..., ys :list..., zs :list... | concat3(list{xs}, list{ys}, list{zs}) = list{xs, ys, zs}" },

			{ "cond :not_positive, true_res, false_res | if_positive(cond, true_res, false_res) = false_res" },
			{ "cond :positive,     true_res, false_res | if_positive(cond, true_res, false_res) = true_res" },

			{ "p :real, xs :list..., y :real, ys :list... | filter_le(p, list{xs}, list{y, ys}) = filter_le(p, if_positive[force(p - y), list{xs}, list{xs, y}], list{ys})" },
			{ "p :real, xs :list...,                      | filter_le(p, list{xs}, list{})      = list{xs}" },

			{ "p :real, xs :list..., y :real, ys :list... | filter_s(p, list{xs}, list{y, ys}) = filter_s(p, if_positive[force(p - y), list{xs, y}, list{xs}], list{ys})" },
			{ "p :real, xs :list...,                      | filter_s(p, list{xs}, list{})      = list{xs}" },


			{ "x, xs :set... | set(x, x, xs) = set(x, xs)" },

			{ "xs :set..., ys :set... | union(set(xs), set(ys)) = union(set(xs, ys))" },
			{ "                       | union()                 = set()" },

			{ "x, xs :set..., ys :set... | intersection(set(x, xs), set(x, ys)) = union(set(x), intersection(set(xs), set(ys)))" },
			{ "xs, ys                    | intersection(xs, ys)                 = set()" },
			{ "                          | intersection()                       = set()" },

			{ "x :real, y :real | min{x, y} = if_positive(force(x-y), y, x)" },
			{ "x :real, y :real | max{x, y} = if_positive(force(x-y), x, y)" },

			//A / B is more commonly written A \ B
			{ "x, xs :set..., y, ys :set... | set{x, xs} / set{x, ys} = set{xs} / set{x, ys}" },
			{ "   xs :set...,    ys :set... | set{xs}    / set{ys}    = set{xs}" },

			//lambda calculus (write a lambda "\x.e" as "L(x, e)" and function application "xy" as "A(x, y)")
			{ "x, e, y | A(L(x, e), y) = replace(x, y, e)" }, //beta conversion
			{ "x, y,           | replace(x, y, x)         = y" },
			{ "x, y,    e      | replace(x, y, L(x, e))   = L(x, e)" },
			{ "x, y, z, e      | replace(x, y, L(z, e))   = L(z, replace(x, y, e))" },
			{ "x, y,    e1, e2 | replace(x, y, A(e1, e2)) = A(replace(x, y, e1), replace(x, y, e2))" },
			{ "x, y, z         | replace(x, y, z)         = z" },

			{ "'true' = L('x', L('y', 'x'))" },
			{ "'false' = L('x', L('y', 'y'))" },
			{ "'not' = L('x', A(A('x', 'false'), 'true'))" },
			{ "'and' = L('x', L('y', A(A('x', 'y'), 'x')))" },
			//{ "" },
			//{ "" },
		});


		for (const auto& rule : rules) {
			std::cout << rule.to_string() << "\n\n";
		}

		while (true) {
			std::string name;
			std::cout << "test> ";
			std::getline(std::cin, name);
			try {
				bmath::Term test(name); 
				//std::cout << "input:  " << test.to_pretty_string() << "\n";
				rules.apply_to(test);

				std::cout << "    = " << test.to_pretty_string() << "\n";
				//std::cout << test.to_memory_layout() << "\n";
				//std::cout << test.to_tree() << "\n";
				std::cout << "\n";

				assert(tree::valid_storage(test.store, { test.head }));
			}
			catch (bmath::ParseFailure failure) {
				std::cout << "parse failure: " << failure.what << '\n';
				std::cout << name << '\n';
				std::cout << std::string(failure.where, ' ') << "^\n\n";
			}
		}
	} //test_rechner

} //namespace bmath::intern::debug

namespace bmath::intern::test {

	namespace detail_sum_enum {

		enum class A1 { a, b, c, COUNT };
		enum class A2 { a, b, c, COUNT };
		using A12 = SumEnum<A1, A2>;

		void f1(A12 e)
		{
			bool hit = false;
			if (e.is<A1>()) { std::cout << "A1 \t"; hit = true; }
			if (e.is<A2>()) { std::cout << "A2 \t"; hit = true; }
			std::cout << (hit ? "\n" : "upsi!\n");
		}

		enum class A3 { a, b, c, COUNT };
		using A123 = SumEnum<A12, A3>;

		void f2(A123 e)
		{
			bool hit = false;
			if                (e.is<A3>()) { std::cout << "A3 \t";        hit = true; }
			if (e.is<A12>() && e.is<A1>()) { std::cout << "A12 && A1 \t"; hit = true; }
			if (e.is<A12>() && e.is<A2>()) { std::cout << "A12 && A2 \t"; hit = true; }
			std::cout << (hit ? "\n" : "upsi!\n");
		}

	} //namespace detail_sum_enum

	void sum_enum()
	{
		using namespace detail_sum_enum;
		f1(A1::a);
		f1(A1::b);
		f1(A1::c);
		f1(A2::a);
		f1(A2::b);
		f1(A2::c);
		std::cout << "\n";
		f2(A1::a);
		f2(A1::b);
		f2(A1::c);
		f2(A2::a);
		f2(A2::b);
		f2(A2::c);
		f2(A3::a);
		f2(A3::b);
		f2(A3::c);
		std::cout << "\n";
	} //sum_enum

	void parentheses()
	{
		std::string parentheses = "1([[0]])2(0)[(0){()(0)}]({}{}{()})3";
		TokenString tokens = tokenize(parentheses);
		std::size_t closed = parentheses.find_last_of(')');
		std::size_t open = find_open_par(closed, TokenView(tokens));
		std::cout << parentheses << '\n';
		std::cout << std::string(open, ' ') << "^open\n";
		std::cout << std::string(closed, ' ') << "^closed\n";
		std::cout << "counted " << count_skip_pars(TokenView(tokens), token::number) << " unenclosed instances of token::number\n\n";
	} //parentheses

	void arithmetic_term()
	{
		std::vector<std::string> term_names = {
			"1*2*IchBinBrunoUndIchBinDerKameramann*3",
			" -(b'+c)*2*i-5*(a+3 e 2 weinachtsmannVomNordpolUnterWasserWeilKlimawandel)",
			"sin(1) + 3 + sin(3) + fred + 1 + sin(7) - hans + jens + herbert + 7 + anneliese + fred + sin(3) + marco + 3 + bernd",
			"2.2 + 4",
			"sqrt(100)",
			"2-a*b",
			"sin(-a*b)",
			"1/5*herbert(20e-10, 3 a, 6 anneliese(fred, marko * 4))",
			"a+ln(b^2)+ln(c)+2-b^2-c*a",
			"c*d+g*f+g",
			"c*d+g*f+f",
			"12*herbert+herbert+4",
			"(3*x-2*y)/5",
			"(1*[2i^(-2)*3*(4*(a^5))])",
			"(10/5)^3",
			"auto^herbert*3+auto^(-32*a)-4",
			"3*(sin(a+b+c)^2+cos(a+b+c)^2+4)+i",
			"(3^(x^2))^(x)",
			"sin(x)*b+3*b",
			"a*d+a*b*c",
			"a/(a b)",
			"1+3*4-6",
			"1e-5",
			"c+d+b+a+f+(a*c*b)+(a*d*b)+(a*f*b*(a+c+b)*(a+d+b))",
			"10/5",
			"a (7 ^ 14 + 9)",
			"i",
		};
		for (auto& term_name : term_names) {
			std::cout << "-------------------------------------------------------------------------------------\n";

			try {
				std::cout << "baue aus string: \"" << term_name << "\"\n\n";

				bmath::Term term(term_name);

				std::cout << "nach bau: \n" << term.to_string() << "\n\n";
				std::cout << "speicher nach bau:\n" << term.to_memory_layout() << "\n\n";
				//std::cout << "baum nach bau:\n" << term.to_tree() << "\n\n";

				term.establish_order();

				std::cout << "nach vereinfachen in huebsch: \n" << term.to_pretty_string() << "\n\n";
				std::cout << "speicher nach vereinfachen:\n" << term.to_memory_layout() << "\n\n\n";
				//std::cout << "baum nach vereinfachen:\n" << term.to_tree() << "\n\n";
			}
			catch (ParseFailure failure) {
				std::cout << failure.what << '\n';
				std::cout << term_name << '\n';
				std::cout << std::string(failure.where, ' ') << "^\n\n";
			}
		}
	} //arithmetic_term

	void pattern_term() {
		using namespace bmath::intern::pattern;

		std::vector<std::string> term_names = {
			"a :real, b | a^2 + (2 a) b + b^2 = (a + b)^2 ",
			"a, b | a^2 + 2 a b + b^2 = (a + b)^2",
			"a :any, k :nn1 | a^(2 k + 1) = a a^(2 k)",
			"a:real, b, c:complex | (a b)^c = a^c b^c",
			"cos('pi') = -1",
			"cos('pi') = -1",
			"a, b :factors | a b + a = a (b + 1)",
			"as :params | drop(0, list(as)) = list(as)",
			"n :nat, a, as :params | drop(n, list(a, as)) = drop(n - 1, list(as))",
			"as :params | sum(as) = 0+as",
			"as :params | product(as) = 1*as",
		};
		for (auto& s : term_names) {
			std::cout << "baue aus: \"" << s << "\"\n";
			const RewriteRule pattern(s);
			//std::cout << "pattern: " << pattern.to_string() << "\n\n";
			//std::cout << "lhs baum:\n" << pattern.lhs_tree() << "\n\n";
			//std::cout << "rhs baum:\n" << pattern.rhs_tree() << "\n\n";
			//std::cout << "lhs speicher:\n" << pattern.lhs_memory_layout() << "\n\n";
			//std::cout << "rhs speicher:\n" << pattern.rhs_memory_layout() << "\n\n";
			std::cout << "-------------------------------------------------------------------------------------\n";
		}
	} //pattern_term

	void bit_set()
	{
		const std::uint64_t init = ~0;
		BitSet<128> set = init;
		set.flip(33);
		set.set(111);
		set.reset(42);
		std::cout << "values in BitSet<...>:\n";
		for (std::size_t i = 0; i < set.size(); i++) {
			std::cout << (set.test(i) ? '|' : '.');
		}
		std::cout << "\n";
		std::cout << "first not set: " << set.find_first_false() << "\n";
		std::cout << "first set:     " << set.find_first_true() << "\n";
		std::cout << "count:         " << set.count() << "\n";
		std::cout << std::boolalpha;
		std::cout << "all:   " << set.all() << "\n";
		std::cout << "any:   " << set.any() << "\n";
		std::cout << "none:  " << set.none() << "\n";
		std::cout << std::noboolalpha;
	}


	void alloc_n()
	{
		BasicStore<long long> store;
		const auto print = [&store]() {
			for (int i = 0; i < store.size(); i++) {
				std::printf("%3d | ", i);
				if (store.valid_idx(i)) {
					std::cout << store.at(i) << "\n";
				}
				else {
					std::cout << "__free__\n";
				}
			}
			std::cout << "\n";
		};

		struct Input
		{
			std::vector<long long> elems;
			std::size_t start = -1;

			Input(std::initializer_list<long long> init) :elems(init) {}
		};

		std::vector<Input> inputs = {
			{1},
			{200, 201},
			{2},
			{210, 211},
			{3},
			{4},
			{5},
			{220, 221},
			{230, 231},
			{300, 301, 302},
			{6},
			{7},
			{600, 601, 602, 603, 604, 605},
			{310, 311, 312},
			{400, 401, 402, 403},

			{-1},
			{-2},
			{-300, -301, -302},
			{-200, -201},
			{-3},
			{-4},
			{-210, -211},
			{-5},
			{-6},
			{-310, -311, -312},
			{-7},
			{-220, -221},
			{-230, -231},
			{-400, -401, -402, -403},
			{-600, -601, -602, -603, -604, -605},
		};

		for (auto& input : inputs) {
			if (input.elems.front() < 0) {
				break;
			}
			input.start = store.allocate_n(input.elems.size());
			for (int i = 0; i < input.elems.size(); i++) {
				store.at(input.start + i) = input.elems[i];
			}
		}
		
		std::cout << "after inserts:\n";
		print();

		{
			int i = 0;
			for (auto& input : inputs) {
				if (i % 2 == 0) {
					if (input.start != -1) {
						store.free_n(input.start, input.elems.size());
						input.start = -1;
					}
				}
				i++;
			}
		}

		std::cout << "after frees:\n";
		print();

		for (auto& input : inputs) {
			if (input.elems.front() > 0) {
				continue;
			}
			input.start = store.allocate_n(input.elems.size());
			for (int i = 0; i < input.elems.size(); i++) {
				store.at(input.start + i) = input.elems[i];
			}
		}

		std::cout << "after new inputs:\n";
		print();

		for (auto& input : inputs) {
			if (input.start != -1) {
				store.free_n(input.start, input.elems.size());
				input.start = -1;
			}
		}

		std::cout << "final:\n";
		print();
	}

	void term_array()
	{
		using StringArray = StoredVector<char, 16>;
		using Store_T = BasicStore<StringArray>;
		using Ref_T = BasicNodeRef<StringArray, Store_T>;
	
		static_assert(StringArray::min_capacity == 12u);
		static_assert(StringArray::values_per_node == 16u);
	
		Store_T store;
		const std::vector<std::string> inputs = { "haaaaaaaaaaaaaaaaaaaaaalllllllllllooooo", "du", "nudel", ":)" };
		std::vector<std::size_t> positions;
		for (const auto& input : inputs) {
			positions.push_back(StringArray::build(store, input));
		}
	
		for (std::size_t position : positions) {
			for (char c : Ref_T(store, position)) {
				std::cout << c;
			}
			std::cout << " ";
		}
		std::cout << "\n";
	}

	void meta_pattern()
	{
		using namespace bmath::intern::meta_pn;

		{
			auto [x] = make_tree_matches<1>;
			auto rule_1 = rule(0_ * x = 0_);
			auto rule_2 = rule(0_ ^ x = 0_);
			auto rule_3 = rule(x ^ 0_ = 1_);
			auto rule_4 = rule(x ^ 1_ = x );

			std::cout << name(rule_1) << "\n";
			std::cout << name(rule_2) << "\n";
			std::cout << name(rule_3) << "\n";
			std::cout << name(rule_4) << "\n";
			std::cout << "\n";
		}
		{
			auto [x, a, b] = make_tree_matches<3>;
			auto [xs] = make_multi_matches<1>;
			auto rule_1 = rule((x^a)^b         = x^(a * b));
			auto rule_2 = rule(    x *     x   = x ^ 2_);
			auto rule_3 = rule(    x * (x^a)   = x^(a + 1_));
			auto rule_4 = rule((x^a) * (x^b)   = x^(a + b ));
			auto rule_5 = rule(exp(ln(x) * xs) = x^xs);

			std::cout << name(rule_1) << "\n";
			std::cout << name(rule_2) << "\n";
			std::cout << name(rule_3) << "\n";
			std::cout << name(rule_4) << "\n";
			std::cout << name(rule_5) << "\n";
			std::cout << "\n";
		}
		{
			auto [a, b] = make_tree_matches<2>;
			auto rule_1 = rule((a ^ 2_) + 2_ * a * b + (b ^ 2_) = (a + b) ^ 2_);
			auto rule_2 = rule((a ^ 2_) - 2_ * a * b + (b ^ 2_) = (a - b) ^ 2_);

			auto [a_square, two_a, b_] = make_tree_matches<3>;
			auto rule_3 = rule(a_square + two_a * b_ + (b_ ^ 2_) = (two_a / 2_ + b_) ^ 2_, sqrt(a_square) == two_a / 2_);

			std::cout << name(rule_1) << "\n";
			std::cout << name(rule_2) << "\n";
			std::cout << name(rule_3) << "\n";
			std::cout << "\n";
		}
		{
			auto [a, b, bs, cs] = make_match_variables<Matches::one, Matches::one, Matches::many, Matches::many>;
			auto rule_1 = rule(a * bs + a * cs = a * (bs + cs), !is_value(a));
			auto rule_2 = rule(a * bs + a      = a * (bs + 1_), !is_value(a));
			auto rule_3 = rule(a      + a      = 2_ * a       , !is_value(a));
			auto rule_4 = rule(a * (b + cs)    = a * b + a * cs);

			auto rule_5 = rule(-(a + bs)       = -a - bs);
			auto rule_6 = rule((a * bs) ^ (-1_) = (a ^ (-1_)) * (bs ^ (-1_)));

			std::cout << name(rule_1) << "\n";
			std::cout << name(rule_2) << "\n";
			std::cout << name(rule_3) << "\n";
			std::cout << name(rule_4) << "\n";
			std::cout << name(rule_5) << "\n";
			std::cout << name(rule_6) << "\n";
			std::cout << "\n";
		}
		{
			auto pi = make_variable_literal<"pi">;
			auto [x] = make_tree_matches<1>;
			auto rule_1 = rule((sin(x) ^ 2_) + (cos(x) ^ 2_) = 1_);

			auto rule_2 = rule(cos(    pi) = -1_);
			auto rule_3 = rule(cos(x * pi) =  0_, is_int(x - 0.5_));
			auto rule_4 = rule(cos(x * pi) =  1_, is_int(x / 2_));
			auto rule_5 = rule(cos(x * pi) = -1_, is_int((x - 1_) / 2_));

			auto rule_6 = rule(sin(    pi) =  0_);
			auto rule_7 = rule(sin(x * pi) =  0_, is_int(x));
			auto rule_8 = rule(sin(x * pi) =  1_, is_int((x - 0.5_) / 2_));
			auto rule_9 = rule(sin(x * pi) = -1_, is_int((x - 1.5_) / 2_));

			std::cout << name(rule_1) << "\n";
			std::cout << name(rule_2) << "\n";
			std::cout << name(rule_3) << "\n";
			std::cout << name(rule_4) << "\n";
			std::cout << name(rule_5) << "\n";
			std::cout << name(rule_6) << "\n";
			std::cout << name(rule_7) << "\n";
			std::cout << name(rule_8) << "\n";
			std::cout << name(rule_9) << "\n";
			std::cout << "\n";
		}
		{
			auto [x, a, f, g, h] = make_tree_matches<5>;
			auto [as] = make_multi_matches<1>;
			auto rule_1 = rule(diff(x, x)      = 1_                                               , is_variable(x));
			auto rule_2 = rule(diff(a, x)      = 0_                                               , is_variable(x), is_variable(a) || is_value(a));
			auto rule_3 = rule(diff(f^a, x)    = diff(f, x) * a * (f^(a - 1_))                    , is_variable(x), is_value(a));
			auto rule_4 = rule(diff(a^f, x)    = diff(f, x) * ln(a) * (a^f)                       , is_variable(x), is_value(a));
			auto rule_5 = rule(diff(g^h, x)    = (diff(h, x) * ln(g) + h * diff(g, x) / g) * (g^h), is_variable(x));
			auto rule_6 = rule(diff(a + as, x) = diff(a, x) + diff(as, x)                         , is_variable(x));
			auto rule_7 = rule(diff(a * as, x) = diff(a, x) * as + a * diff(as, x)                , is_variable(x));
			auto rule_8 = rule(diff(sin(f), x) = diff(f, x) * cos(f)                              , is_variable(x));
			auto rule_9 = rule(diff(cos(f), x) = diff(f, x) * -sin(f)                             , is_variable(x));
			auto rule10 = rule(diff(exp(f), x) = diff(f, x) * exp(f)                              , is_variable(x));
			auto rule11 = rule(diff(ln(f), x)  = diff(f, x) / f                                   , is_variable(x));

			std::cout << name(rule_1) << "\n";
			std::cout << name(rule_2) << "\n";
			std::cout << name(rule_3) << "\n";
			std::cout << name(rule_4) << "\n";
			std::cout << name(rule_5) << "\n";
			std::cout << name(rule_6) << "\n";
			std::cout << name(rule_7) << "\n";
			std::cout << name(rule_8) << "\n";
			std::cout << name(rule_9) << "\n";
			std::cout << name(rule10) << "\n";
			std::cout << name(rule11) << "\n";
			std::cout << "\n";
		}
		{
			auto [x] = make_tree_matches<1>;
			auto [xs, ys] = make_multi_matches<2>;
			auto rule_1 = rule(set(x, x, xs) = set(x, xs));

			auto rule_2 = rule(union_(set(xs), set(ys)) = set(xs, ys));
			auto rule_3 = rule(union_()                 = set());
			auto rule_4 = rule(intersection(set(x, xs), set(x, ys)) = union_(set(x), intersection(set(xs), set(ys))));
			auto rule_5 = rule(intersection(set(xs), set(ys))       = set());
			auto rule_6 = rule(intersection()                       = set());

			std::cout << name(rule_1) << "\n";
			std::cout << name(rule_2) << "\n";
			std::cout << name(rule_3) << "\n";
			std::cout << name(rule_4) << "\n";
			std::cout << name(rule_5) << "\n";
			std::cout << name(rule_6) << "\n";
			std::cout << "\n";
		}
	}

	template<InstanceOf<bmath::intern::meta_pn::RuleSet__> Rules, std::size_t N>
	void try_matching(const Rules& rules, const std::array<bmath::Term, N>& terms)
	{
		for (auto& term : terms) {
			const UnsaveRef head = term.ref();
			for (auto match_function : rules.match_functions) {
				pattern::match::MatchData match_data;
				std::cout << match_function(head, match_data) << " ";
			}
			std::cout << term.to_pretty_string() << "\n";
		}
		std::cout << "\n\n";
	}

	void meta_pattern_2() 
	{
		using namespace bmath::intern::meta_pn;
		{
			auto fib = make_function<"fib", 1>;
			auto [n] = make_tree_matches<1>;

			auto rule_1 = rule(fib(0_) = 0_);
			auto rule_2 = rule(fib(1_) = 1_);
			auto rule_3 = rule(fib(n) = fib(n - 1_) + fib(n - 2_), is_int(n), n > 1_);
			
			std::cout << name(rule_1) << "\n";
			std::cout << name(rule_2) << "\n";
			std::cout << name(rule_3) << "\n";
			std::cout << "\n";

			auto rules = RuleSet__(rule_1, rule_2, rule_3);
			auto terms = std::to_array<bmath::Term>({
				{ "fob(6)" },
				{ "fibb(6)" },
				{ "fib(-2)" },
				{ "fib(-1)" },
				{ "fib(0)" },
				{ "fib(1)" },
				{ "fib(2)" },
				{ "fib(3)" },
				{ "fib(4)" },
				{ "fib(5)" },
				{ "fib(6)" },
				});
			try_matching(rules, terms);
		}
		{
			//reversing a list:
			//{ "xs :list...                 | reverse(list{xs}) = reverse'(list{}, list{xs})" },
			//{ "xs :list..., y, ys :list... | reverse'(list{xs}, list{y, ys}) = reverse'(list{y, xs}, list{ys})" },
			//{ "xs :list...,                | reverse'(list{xs}, list{})      = list{xs}" },
			auto reverse = make_function<"reverse", 1>;
			auto reverse_ = make_function<"reverse'", 2>;
			auto [xs, y, ys] = make_match_variables<Matches::many, Matches::one, Matches::many>;
			auto rule_1 = rule(reverse(list(xs))               = reverse_(list(), list(xs)));
			auto rule_2 = rule(reverse_(list(xs), list(y, ys)) = reverse_(list(y, xs), list(ys)));
			auto rule_3 = rule(reverse_(list(xs), list())      = list(xs));

			//listing first n fibonacci numbers:
			//{ "n :nat0                     | fib_n(n + 2)                   = reverse(list_fibs(n, list{1, 0}))" },
			//{ "n :nat, a, b, tail :list... | list_fibs(n, list{a, b, tail}) = list_fibs(n - 1, list{force(a + b), a, b, tail})" },
			//{ "              tail :list... | list_fibs(0, list{tail})       = list{tail}" },
			auto fib_n = make_function<"fib_n", 1>;
			auto list_fibs = make_function<"list_fibs", 2>;
			auto [n, a, b] = make_tree_matches<3>;
			auto [tail] = make_multi_matches<1>;
			auto rule_4 = rule(fib_n(n)                       = reverse(list_fibs(n - 2_, list(1_, 0_))), is_int(n), n >= 2_);
			auto rule_5 = rule(list_fibs(n, list(a, b, tail)) = list_fibs(n - 1_, list(force(a + b), a, b, tail)), n > 0_);
			auto rule_6 = rule(list_fibs(0_, a)               = a);
		}
		{
			//sorting numbers:
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
			auto sort      = make_function<"sort", 1>;
			auto concat3   = make_function<"concat3", 3>;
			auto filter_le = make_function<"filter_le", 3>;
			auto filter_s  = make_function<"filter_s", 3>;
			auto [x, y, p] = make_tree_matches<3>;
			auto [xs, ys, zs] = make_multi_matches<3>;
			auto rule_1 = rule(sort(list())      = list());
			auto rule_2 = rule(sort(list(x))     = list(x));
				auto s_sorted  = sort( filter_s(x, list(), list(xs)));
				auto le_sorted = sort(filter_le(x, list(), list(xs)));
			auto rule_3 = rule(sort(list(x, xs)) = concat3(s_sorted, list(x), le_sorted), is_real(p));
			auto rule_4 = rule(concat3(list(xs), list(ys), list(zs)) = list(xs, ys, zs));
			auto rule_5 = rule(filter_le(p, list(xs), list(y, ys)) = filter_le(p, list(xs, y), list(ys)), y >= p);
			auto rule_6 = rule(filter_le(p, list(xs), list(y, ys)) = filter_le(p,    list(xs), list(ys)), y <  p);
			auto rule_7 = rule(filter_le(p, list(xs), list())      = list(xs));
			auto rule_8 = rule( filter_s(p, list(xs), list(y, ys)) =  filter_s(p, list(xs, y), list(ys)), y <  p);
			auto rule_9 = rule( filter_s(p, list(xs), list(y, ys)) =  filter_s(p,    list(xs), list(ys)), y >= p);
			auto rule10 = rule( filter_s(p, list(xs), list())      = list(xs));
		}
	}

	void stable_sort() 
	{
		struct Pair { int fst, snd; };
		auto pairs = std::to_array<Pair>({
			{3, 1},
			{2, 1},
			{4, 1},
			{1, 1},
			{2, 2},
			{3, 2},
			{3, 3},
			{3, 4},
			{3, 5},
			{5, 1},
			{3, 6},
			{4, 2},
			{2, 3},
			{4, 3},
			{1, 2},
			{4, 4},
			{5, 2},
			{1, 3},
			{1, 4},
			{4, 5},
			});
		bmath::intern::stable_sort(pairs, [](const Pair& a, const Pair& b) { return a.fst < b.fst; });
		for (const auto& pair : pairs) {
			std::cout << pair.fst << " " << pair.snd << "\n";
		}
	}


} //namespace bmath::intern::test
