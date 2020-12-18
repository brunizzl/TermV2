#pragma once

#include <iostream>
#include <charconv>
#include <cassert>
#include <numeric>

#include "termStore.hpp"
#include "termColony.hpp"
#include "arithmeticTerm.hpp"
#include "parseTerm.hpp"
#include "ioArithmetic.hpp"

namespace bmath::intern::debug {

	void enumerate_pn_type()
	{
		using namespace pattern;
		static_assert(unsigned(Type::COUNT) == unsigned(PnType(Type::COUNT)), "else at least a second version of this function is needed");
		std::cout
			<< "PnType(Op::sum)                 = " << unsigned(PnType(Op::sum))                 << "\n"
			<< "PnType(Op::product)             = " << unsigned(PnType(Op::product))             << "\n"
			<< "PnType(Op::named_fn)            = " << unsigned(PnType(Op::named_fn))            << "\n"
			                                                                                     << "\n"
			<< "PnType(Leaf::variable)          = " << unsigned(PnType(Leaf::variable))          << "\n"
			<< "PnType(Leaf::complex)           = " << unsigned(PnType(Leaf::complex))           << "\n"
			                                                                                     << "\n"
			<< "PnType(Fn::pow)                 = " << unsigned(PnType(Fn::pow))                 << "\n"
			<< "PnType(Fn::log)                 = " << unsigned(PnType(Fn::log))                 << "\n"
			<< "PnType(Fn::exp)                 = " << unsigned(PnType(Fn::exp))                 << "\n"
			<< "PnType(Fn::sqrt)                = " << unsigned(PnType(Fn::sqrt))                << "\n"
			<< "PnType(Fn::asinh)               = " << unsigned(PnType(Fn::asinh))               << "\n"
			<< "PnType(Fn::acosh)               = " << unsigned(PnType(Fn::acosh))               << "\n"
			<< "PnType(Fn::atanh)               = " << unsigned(PnType(Fn::atanh))               << "\n"
			<< "PnType(Fn::asin)                = " << unsigned(PnType(Fn::asin))                << "\n"
			<< "PnType(Fn::acos)                = " << unsigned(PnType(Fn::acos))                << "\n"
			<< "PnType(Fn::atan)                = " << unsigned(PnType(Fn::atan))                << "\n"
			<< "PnType(Fn::sinh)                = " << unsigned(PnType(Fn::sinh))                << "\n"
			<< "PnType(Fn::cosh)                = " << unsigned(PnType(Fn::cosh))                << "\n"
			<< "PnType(Fn::tanh)                = " << unsigned(PnType(Fn::tanh))                << "\n"
			<< "PnType(Fn::sin)                 = " << unsigned(PnType(Fn::sin))                 << "\n"
			<< "PnType(Fn::cos)                 = " << unsigned(PnType(Fn::cos))                 << "\n"
			<< "PnType(Fn::tan)                 = " << unsigned(PnType(Fn::tan))                 << "\n"
			<< "PnType(Fn::abs)                 = " << unsigned(PnType(Fn::abs))                 << "\n"
			<< "PnType(Fn::arg)                 = " << unsigned(PnType(Fn::arg))                 << "\n"
			<< "PnType(Fn::ln)                  = " << unsigned(PnType(Fn::ln))                  << "\n"
			<< "PnType(Fn::re)                  = " << unsigned(PnType(Fn::re))                  << "\n"
			<< "PnType(Fn::im)                  = " << unsigned(PnType(Fn::im))                  << "\n"
			                                                                                     << "\n"
			<< "PnType(PnVar::tree_match)       = " << unsigned(PnType(PnVar::tree_match))       << "\n"
			<< "PnType(PnVar::value_match)      = " << unsigned(PnType(PnVar::value_match))      << "\n"
			<< "PnType(PnVar::value_proxy)      = " << unsigned(PnType(PnVar::value_proxy))      << "\n"
			                                                                                     << "\n"
			<< "PnType(MultiVar::summands)      = " << unsigned(PnType(MultiVar::summands))      << "\n"
			<< "PnType(MultiVar::factors)       = " << unsigned(PnType(MultiVar::factors))       << "\n"
			<< "PnType(MultiVar::params)        = " << unsigned(PnType(MultiVar::params))        << "\n"
			                                                                                     << "\n"
			                                                                                     << "\n"
			;
	} //enumerate_pn_type

	//output as of 18.11.2020:
		//PnType(Op::sum)                 = 0
		//PnType(Op::product)             = 1
		//PnType(Op::named_fn)            = 2
		//
		//PnType(Leaf::variable)          = 3
		//PnType(Leaf::complex)           = 4
		//
		//PnType(Fn::pow)                 = 5
		//PnType(Fn::log)                 = 6
		//PnType(Fn::exp)                 = 7
		//PnType(Fn::sqrt)                = 8
		//PnType(Fn::asinh)               = 9
		//PnType(Fn::acosh)               = 10
		//PnType(Fn::atanh)               = 11
		//PnType(Fn::asin)                = 12
		//PnType(Fn::acos)                = 13
		//PnType(Fn::atan)                = 14
		//PnType(Fn::sinh)                = 15
		//PnType(Fn::cosh)                = 16
		//PnType(Fn::tanh)                = 17
		//PnType(Fn::sin)                 = 18
		//PnType(Fn::cos)                 = 19
		//PnType(Fn::tan)                 = 20
		//PnType(Fn::abs)                 = 21
		//PnType(Fn::arg)                 = 22
		//PnType(Fn::ln)                  = 23
		//PnType(Fn::re)                  = 24
		//PnType(Fn::im)                  = 25
		//
		//PnType(PnVar::tree_match)       = 26
		//PnType(PnVar::value_match)      = 27
		//PnType(PnVar::value_proxy)      = 28
		//
		//PnType(MultiVar::summands)      = 29
		//PnType(MultiVar::factors)       = 30
		//PnType(MultiVar::params)        = 31

	void test_rechner() 
	{
		const auto patterns = std::to_array<pattern::PnTerm>({ 
			{ "x :factors | x * 0 = 0" },
			{ "x          | x ^ 1 = x" },

			{ "x, a, b | (x^a)^b = x^(a*b)" },
			{ "x       | x x     = x^2" }, 
			{ "x, a    | x x^a   = x^(a + 1)" },
			{ "x, a, b | x^a x^b = x^(a + b)" },
			{ "x :factors, y | exp(x ln(y)) = y^x" },

			{ "a, b          | a^2 + 2 a b   + b^2 = (a + b)^2" }, 
			{ "a, b          | a^2 - 2 a b   + b^2 = (a - b)^2" }, 
			{ "a :complex, b | a^2 + (2 a) b + b^2 = (a + b)^2" }, 

			{ "a, bs :factors, cs :factors | a bs + a cs = a (bs + cs)" }, //will only work very few times for now (no rematch implemented yet)
			{ "a, bs :factors | a*bs + a = a (bs + 1)" }, 
			{ "a | a + a = 2 a" }, 
			
			{ "a, b | a a^b = 2 a" }, 
			{ "a | a + a = 2 a" }, 

			////exponential runtime fibonacci implementation:
			//{ "fib(0) = 0" },
			//{ "fib(1) = 1" },
			//{ "n :nat | fib(n) = fib(n - 1) + fib(n - 2)" },
			//
			////reversing a list:
			//{ "xs :params | reverse(list{xs}) = reverse'(list{}, list{xs})" },
			//{ "xs :params, y, ys :params | reverse'(list{xs}, list{y, ys}) = reverse'(list{y, xs}, list{ys})" },
			//{ "xs :params,               | reverse'(list{xs}, list{})      = list{xs}" },
			//
			////listing first n fibonacci numbers:
			//{ "n :nat0                    | fib_n(n + 2)                   = reverse(list_fibs(n, list{1, 0}))" },
			//{ "n :nat, a, b, tail :params | list_fibs(n, list{a, b, tail}) = list_fibs(n - 1, list{force(a + b), a, b, tail})" },
			//{ "              tail :params | list_fibs(0, list{tail})       = list{tail}" },
			//
			////sorting numbers:
			//{ "cond :not_positive, true_res, false_res | if_positive(cond, true_res, false_res) = false_res" },
			//{ "cond :positive,     true_res, false_res | if_positive(cond, true_res, false_res) = true_res" },
			//
			//{ "p :real, xs :params, y :real, ys :params | filter_le(p, list{xs}, list{y, ys}) = filter_le(p, if_positive[force(p - y), list{xs}, list{xs, y}], list{ys})" },
			//{ "p :real, xs :params,                     | filter_le(p, list{xs}, list{})      = list{xs}" },
			//
			//{ "p :real, xs :params, y :real, ys :params | filter_s(p, list{xs}, list{y, ys}) = filter_s(p, if_positive[force(p - y), list{xs, y}, list{xs}], list{ys})" },
			//{ "p :real, xs :params,                     | filter_s(p, list{xs}, list{})      = list{xs}" },
			//
			//{ "p :real, xs :params | sort(list{p, xs}) = weird_concat(sort(filter_s(p, list{}, list{xs})), p, sort(filter_le(p, list{}, list{xs})))" },
			//{ "                    | sort(list{})      = list{}" },
			//{ "xs :params, y, zs :params | weird_concat(list{xs}, y, list{zs}) = list{xs, y, zs}" }, 
			
			
			//differentiation rules:
			{ "x :variable                      | diff(x, x)      = 1" },
			{ "x :variable, a :variable         | diff(a, x)      = 0" },
			{ "x :variable, a :value            | diff(a, x)      = 0" },
			{ "x :variable, a :value, f :any    | diff(f^a, x)    = diff(f, x) a f^(a-1)" },
			{ "x :variable, a :value, f :any    | diff(a^f, x)    = diff(f, x) ln(a) a^f" },
			{ "x :variable, g :any, h :any      | diff(g^h, x)    = (diff(h, x) ln(g) + h diff(g, x)/g) g^h" },
			{ "x :variable, u :any, v :summands | diff(u + v, x)  = diff(u, x) + diff(v, x)" },
			{ "x :variable, u :any, v :factors  | diff(u v, x)    = diff(u, x) v + u diff(v, x)" },
			{ "x :variable, f :any              | diff(sin(f), x) = diff(f, x) cos(f)" },
			{ "x :variable, f :any              | diff(cos(f), x) = diff(f, x) (-sin(f))" },
			{ "x :variable, f :any              | diff(exp(f), x) = diff(f, x) exp(f)" },
			{ "x :variable, f :any              | diff(ln(f), x)  = diff(f, x) 1/f" },

			////fun with value match variables:
			//{ "k :int | 2 k + 1 = pair('how_odd', k + 1/(2^20))" }, 
			//{ "k :int | 2 k = pair('how_unodd', k + 1/(2^20))" },
		});

		for (const auto& p : patterns) {
			std::cout << p.to_string() << "\n";
			std::cout << "lhs:\n" << p.lhs_tree() << "\n";
			std::cout << "rhs:\n" << p.rhs_tree() << "\n\n\n";
		}

		while (true) {
			std::string name;
			std::cout << "> ";
			std::getline(std::cin, name);
			try {
				bmath::Term test(name); 
				//std::cout << "input:  " << test.to_string() << "\n";
				test.standardize();
				//std::cout << "sorted: " << test.to_string() << "\n";
				std::cout << test.to_tree() << "\n";
				bool changed;
				do {
					changed = false;
					for (const auto& p : patterns) {
						if (test.match_and_replace(p)) {
							changed = true;
							test.standardize();
							std::cout << "    = " << test.to_string() << "\n";
							//std::cout << test.to_memory_layout() << "\n";
							break;
						}
					}
				} while (changed);
				//std::cout << test.to_memory_layout() << "\n";
				std::cout << "result:   " << test.to_pretty_string() << "\n";
				//std::cout << "result:   " << test.to_string() << "\n";
				std::cout << "\n";
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

	namespace sum_enum_detail {

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

	} //namespace sum_enum_detail

	void sum_enum()
	{
		using namespace sum_enum_detail;
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
			"sqrt(100)",
			"2-a*b",
			"sin(-a*b)",
			" -(b'+c)*2*i-5*(a+3 e 2 weinachtsmannVomNordpolUnterWasserWeilKlimawandel)",
			"1/5*herbert(20e-10, 3 a, 6 anneliese(fred, marko * 4))",
			"sin(1) + 3 + sin(3) + fred + 1 + sin(7) - hans + jens + herbert + 7 + anneliese + fred + sin(3) + marco + 3 + bernd",
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
			"2.2 + 4",
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
				std::cout << "baum nach bau:\n" << term.to_tree() << "\n\n";

				term.standardize();

				std::cout << "nach vereinfachen in huebsch: \n" << term.to_pretty_string() << "\n\n";
				std::cout << "speicher nach vereinfachen:\n" << term.to_memory_layout() << "\n\n\n";
				std::cout << "baum nach vereinfachen:\n" << term.to_tree() << "\n\n";
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
			const PnTerm pattern(s);
			std::cout << "pattern: " << pattern.to_string() << "\n\n";
			std::cout << "lhs speicher:\n" << pattern.lhs_memory_layout() << "\n\n";
			std::cout << "rhs speicher:\n" << pattern.rhs_memory_layout() << "\n\n";
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

	void bit_vector()
	{
		BitVector vec;

		const auto print = [&]() {
			for (std::size_t i = 0; i < vec.size(); i++) {
				if (i % 64 == 0) {
					std::printf("\n%5d  ", (int)i); //pleas std::format, i need you :(
				}
				std::cout << (vec.test(i) ? '|' : '.');
			}
			std::cout << "\n\n";
		};

		for (int i = 0; i < 64; i++) {
			vec.push_true();
			vec.push_n_false(16u);
		}

		std::cout << "after push:\n";
		print();

		std::vector<std::size_t> sets;
		for (int i = 0; i < 10; i++) {
			sets.push_back(vec.set_first_n_alligned_false(16u));
			std::cout << i << " set at " << sets.back() << "\n";
		}

		std::cout << "after set:\n";
		print();

		std::cout << (vec.count() - 64) / 16.0 << " chunks set\n\n";

		for (auto set_pos : sets) {
			vec.reset_alligned_n(set_pos, 16u);
		}

		std::cout << "after resets:\n";
		print();

		vec.clear();
		vec.push_n_true(64u);
		vec.push_false();
		std::cout << "new content:\n";
		print();

		std::cout << "first false: " << vec.find_first_false() << "\n";
	}

	void combine_exact()
	{
		std::vector<std::string> names = { {"1/2"}, {"1/5"}, {"1 + 1e+200"} };
		for (auto& name : names) {
			auto term = Term(name);
			std::cout << "\"" << name << "\" -> " << term.to_string() << " -> ";
			term.combine_values_exact();
			std::cout << term.to_string() << " -> ";
			term.combine_values_inexact();
			std::cout << term.to_string() << "\n";

			//auto term = Term(name);
			//std::cout << "\"" << name << "\" -> \n\n" << term.to_memory_layout() << "\n\n -> \n";
			//term.combine_values_exact();
			//std::cout << term.to_memory_layout() << "\n\n -> \n";
			//term.combine_values_inexact();
			//std::cout << term.to_memory_layout() << "\n\n\n\n";
		}
	}

	void copy()
	{
		std::string term_name = "1+2-a(4+6)^2";
		Term term_1(term_name);
		std::cout << "term_1: " << term_1.to_string() << "\n";

		Ref ref = static_cast<const Term&>(term_1).ref();
		auto head_2 = tree::copy(ref, const_cast<Store&>(*ref.store));

		std::string term_2_str;
		print::append_to_string(ref, term_2_str);
		std::cout << "term_2: " << term_2_str << "\n";
		std::cout << print::to_memory_layout(*ref.store, { ref.typed_idx(), head_2 }) << "\n";
	}

	void match()
	{
		std::string p_name = "a:real, b | (a^2 + b)^a = 1";
		auto p = pattern::PnTerm(p_name);
		std::string t_name = "(100 + jochen)^10";
		auto t = Term(t_name);
		t.standardize();
		auto m = pattern::MatchData{};
		std::cout << "match lhs of \"" << p.to_string() << "\" with \"" << t.to_string() << ": " << match::equals(p.lhs_ref(), t.ref(), m) << "\n";
	}

} //namespace bmath::intern::test
