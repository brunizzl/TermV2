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

		enum class B1 { a, b, c, COUNT };
		enum class B2 { a, b, c, COUNT };
		using BB1 = SumEnum<B1>;
		using B12B1 = SumEnum<BB1, B2, B1>;

		void g1(B12B1 e)
		{
			bool hit = false;
			if (e.is<B1>())  { std::cout << "B1 \t";  hit = true; }
			if (e.is<B2>())  { std::cout << "B2 \t";  hit = true; }
			if (e.is<BB1>()) { std::cout << "BB1 \t"; hit = true; }
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
		g1(B1::a);
		g1(B1::b);
		g1(B1::c);
		g1(B2::a);
		g1(B2::b);
		g1(B2::c);
		g1(BB1(B1::a));
		g1(BB1(B1::b));
		g1(BB1(B1::c));
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
			"2-a*b",
			"sin(-a*b)",
			" -(b'+c)*2*i-5*(a+3 e 2 weinachtsmannVomNordpolUnterWasserWeilKlimawandel)",
			"1/5*herbert(20e-10, 3 a, 6 anneliese(fred, marko * 4))",
			"sin(1) + 3 + sin(3) + fred + 1 + sin(7) - hans + jens + herbert + 7 + anneliese + fred + sin(3) + marco + 3 + bernd",
			"a+ln(b^2)+ln(c)+2-b^2-c*a",
			"c*d+g*f+g",
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
				//std::cout << "speicher nach bau:\n" << term.to_memory_layout() << "\n\n";

				term.combine_layers();
				term.combine_values_exact();
				term.sort();

				std::cout << "nach vereinfachen in huebsch: \n" << term.to_pretty_string() << "\n\n";
				//std::cout << "speicher nach vereinfachen:\n" << term.to_memory_layout() << "\n\n\n";
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
			"a :any, k :int | a^(2 k + 1) = a a^(2 k)",
			"a:real, b, c:complex | (a b)^c = a^c b^c",
			"a, b | a^2 + 2 a b + b^2 = (a + b)^2",
			"cos('pi') = -1",
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
		std::string term_name = "1+2-a*(4+6)^2";
		Term term_1(term_name);
		std::cout << "term_1: " << term_1.to_string() << "\n";

		auto [store, head_1] = term_1.data();
		auto head_2 = tree::copy(*store, *store, head_1);

		std::string term_2_str;
		print::append_to_string(*store, head_2, term_2_str);
		std::cout << "term_2: " << term_2_str << "\n";
		std::cout << print::to_memory_layout(*store, { head_1, head_2 }) << "\n";
	}

} //namespace bmath::intern::test
