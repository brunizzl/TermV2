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
			<< "PnType(Op::COUNT)               = " << unsigned(PnType(Op::COUNT))               << "\n"
			                                                                                     << "\n"
			<< "PnType(Leaf::variable)          = " << unsigned(PnType(Leaf::variable))          << "\n"
			<< "PnType(Leaf::complex)           = " << unsigned(PnType(Leaf::complex))           << "\n"
			<< "PnType(Leaf::COUNT)             = " << unsigned(PnType(Leaf::COUNT))             << "\n"
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
			<< "PnType(Fn::COUNT)               = " << unsigned(PnType(Fn::COUNT))               << "\n"
			                                                                                     << "\n"
			<< "PnType(Type::COUNT)             = " << unsigned(PnType(Type::COUNT))             << "\n"
			                                                                                     << "\n"
			<< "PnType(PnVar::tree_match)       = " << unsigned(PnType(PnVar::tree_match))       << "\n"
			<< "PnType(PnVar::value_match)      = " << unsigned(PnType(PnVar::value_match))      << "\n"
			<< "PnType(PnVar::value_proxy)      = " << unsigned(PnType(PnVar::value_proxy))      << "\n"
			<< "PnType(PnVar::COUNT)            = " << unsigned(PnType(PnVar::COUNT))            << "\n"
			                                                                                     << "\n"
			<< "PnType(MultiVar::summands)      = " << unsigned(PnType(MultiVar::summands))      << "\n"
			<< "PnType(MultiVar::factors)       = " << unsigned(PnType(MultiVar::factors))       << "\n"
			<< "PnType(MultiVar::COUNT)         = " << unsigned(PnType(MultiVar::COUNT))         << "\n"
			                                                                                     << "\n"
			<< "PnType::COUNT                   = " << unsigned(PnType::COUNT)                   << "\n"
			                                                                                     << "\n"
			                                                                                     << "\n"
			;
	} //enumerate_pn_type

	//output as of 15.11.2020:
		//PnType(Op::sum)                 = 0
		//PnType(Op::product)             = 1
		//PnType(Op::named_fn)            = 2
		//PnType(Op::COUNT)               = 3
		//
		//PnType(Leaf::variable)          = 4
		//PnType(Leaf::complex)           = 5
		//PnType(Leaf::COUNT)             = 6
		//
		//PnType(Fn::pow)                 = 7
		//PnType(Fn::log)                 = 8
		//PnType(Fn::exp)                 = 9
		//PnType(Fn::sqrt)                = 10
		//PnType(Fn::asinh)               = 11
		//PnType(Fn::acosh)               = 12
		//PnType(Fn::atanh)               = 13
		//PnType(Fn::asin)                = 14
		//PnType(Fn::acos)                = 15
		//PnType(Fn::atan)                = 16
		//PnType(Fn::sinh)                = 17
		//PnType(Fn::cosh)                = 18
		//PnType(Fn::tanh)                = 19
		//PnType(Fn::sin)                 = 20
		//PnType(Fn::cos)                 = 21
		//PnType(Fn::tan)                 = 22
		//PnType(Fn::abs)                 = 23
		//PnType(Fn::arg)                 = 24
		//PnType(Fn::ln)                  = 25
		//PnType(Fn::re)                  = 26
		//PnType(Fn::im)                  = 27
		//PnType(Fn::COUNT)               = 28
		//
		//PnType(Type::COUNT)             = 29
		//
		//PnType(PnVar::tree_match)       = 30
		//PnType(PnVar::value_match)      = 31
		//PnType(PnVar::value_proxy)      = 32
		//PnType(PnVar::COUNT)            = 33
		//
		//PnType(MultiVar::summands)      = 34
		//PnType(MultiVar::factors)       = 35
		//PnType(MultiVar::COUNT)         = 36
		//
		//PnType::COUNT                   = 37

	void test_rechner() 
	{
		const auto patterns = std::to_array<pattern::PnTerm>({ 
			//{"x | sin(x)^2 + cos(x)^2 = 1"}, 
			//{"a, b | a^2 + 2 a b + b^2 = (a + b)^2"}, 
			//{"a, b | a^2 - 2 a b + b^2 = (a - b)^2"}, 
			//{"x | sin(x) / cos(x) = tan(x)"}, 
			//{"x | 0 x = 0"}, 
			{"a | a + a = 2 a"}, 
			{"a, bs :factors | a bs + a = a (bs + 1)"}, 
			{"a, bs :factors, cs :factors | a bs + a cs = a (bs + cs)"}, //will only work very few times for now (no rematch implemented yet)
			//{"b, a | a b + a = a (b + 1)"}, 
			//{"a :no_val, b, c | a b + a c = a (b + c)"}, 
			//{"b, a :no_val, c | a b + a c = a (b + c)"}, 
			//{"a, b, c | a b + a c = a (b + c)"}, 
			//{"b, a, c | a b + a c = a (b + c)"}, 
			//{ "fib(0) = 0" },
			//{ "fib(1) = 1" },
			//{ "n | fib(n) = fib(n - 1) + fib(n - 2)" },
			//{ "a :real, b | a^2 + (2 a) b + b^2 = (a + b)^2" }, 
			//{ "a :int | 2 a + 1 = 'how_odd'" }, 
		});

		for (const auto& p : patterns) {
			std::cout << p.to_string() << "\n";
			std::cout << "lhs:\n" << p.lhs_tree() << "\n";
			std::cout << "rhs:\n" << p.rhs_tree() << "\n\n\n";
		}

		while (true) {
			std::cout << "> ";
			std::string name;
			std::cin >> name;
			try {
				bmath::Term test(name); 
				std::cout << "input:  " << test.to_string() << "\n";
				test.standardize();
				std::cout << "sorted: " << test.to_string() << "\n";
				bool changed;
				do {
					changed = false;
					for (const auto& p : patterns) {
						if (test.match_and_replace(p)) {
							std::cout << test.to_memory_layout() << "\n";
							changed = true;
							std::cout << "replace -> " << test.to_string() << "\n";
							test.standardize();
							std::cout << "sort    -> " << test.to_string() << "\n";
							break;
						}
					}
				} while (changed);
				std::cout << "done:   " << test.to_string() << "\n";
				std::cout << "\n";
			}
			catch (bmath::ParseFailure failure) {
				std::cout << "parse failure: " << failure.what << '\n';
				std::cout << name << '\n';
				std::cout << std::string(failure.where, ' ') << "^\n\n";
			}
			std::cin.get();
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
		};
		for (auto& s : term_names) {
			std::cout << "-------------------------------------------------------------------------------------\n";
			std::cout << "baue aus: \"" << s << "\"\n";
			const PnTerm pattern(s);
			std::cout << "pattern: " << pattern.to_string() << "\n\n";
			std::cout << "lhs speicher:\n" << pattern.lhs_memory_layout() << "\n\n";
			std::cout << "rhs speicher:\n" << pattern.rhs_memory_layout() << "\n\n";
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
