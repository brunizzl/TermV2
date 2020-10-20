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
		std::cout << "-------------------------------------------------------------------------------------\n";
		using namespace bmath::intern::pattern;
		//std::string s = "a, b | a^2 + 2 a b + b^2 = (a + b)^2";
		//std::string s = "cos('pi') = -1";
		std::string s = "a:real, b, c:complex | (a b)^c = a^c b^c";
		const PnTerm pattern(s);
		std::cout << "pattern: " << pattern.to_string() << "\n\n";
		std::cout << "lhs speicher:\n" << pattern.lhs_memory_layout() << "\n\n";
		std::cout << "rhs speicher:\n" << pattern.rhs_memory_layout() << "\n\n";
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

	void stupid_solve_for()
	{
		std::vector<std::pair<std::string, std::string>> vec = { 
			{"1-a", "c"}, 
			{"1+a", "c"}, 
			{"exp(a)", "c"}, 
			{"a*4-2", "5"}, 
			{"a^4*5", "c"} };
		for (auto& [lhs_str, rhs_str] : vec) {
			ParseString lhs_parse = lhs_str;
			ParseString rhs_parse = rhs_str;
			Store store;
			tree::Equation<TypedIdx> eq = { build(store, lhs_parse), build(store, rhs_parse) };

			std::string equation_str;
			print::append_to_string(store, eq.lhs_head, equation_str);
			equation_str += " = ";
			print::append_to_string(store, eq.rhs_head, equation_str);
			std::cout << equation_str << "\n";

			const auto a_idx = tree::search_variable(store, eq.lhs_head, "a");
			tree::stupid_solve_for(store, eq, a_idx);
			tree::combine_layers(store, eq.rhs_head);
			{
				const OptComplex new_rhs = tree::combine_values_exact(store, eq.rhs_head);
				if (new_rhs) {
					tree::free(store, eq.rhs_head);
					eq.rhs_head = TypedIdx(store.insert(*new_rhs), Type::complex);
				}
			}

			equation_str.clear();
			print::append_to_string(store, eq.lhs_head, equation_str);
			equation_str += " = ";
			print::append_to_string(store, eq.rhs_head, equation_str);
			std::cout << equation_str << "\n\n";
		}
	}

	void copy()
	{
		std::string term_name = "1+2-a*(4+6)^2";
		Term term_1(term_name);
		std::cout << "term_1: " << term_1.to_string() << "\n";

		auto [store, head_1] = term_1.data();
		auto head_2 = tree::copy<TypedIdx>(*store, *store, head_1);

		std::string term_2_str;
		print::append_to_string(*store, head_2, term_2_str);
		std::cout << "term_2: " << term_2_str << "\n";
		std::cout << term_1.to_memory_layout() << "\n";
		std::cout << print::to_memory_layout(*store, head_2) << "\n";
	}

	void find_value_match_subtree()
	{
		using namespace pattern;
		std::string s = "a :any, k :int | a^(2 k+1) = a a^(2 k)";

		auto parse_string = ParseString(s);
		parse_string.allow_implicit_product();
		parse_string.remove_space();
		const auto parts = split(parse_string);
		NameLookupTable table = parse_declarations(parts.declarations);
		throw_if(table.tree_table.size() > MatchData::max_tree_match_count, "too many tree match variables declared");
		throw_if(table.value_table.size() > MatchData::max_value_match_count, "too many value match variables declared");
		PatternBuildFunction build_function = { table };

		PnStore lhs_store;
		PnTypedIdx lhs_head = build_function(lhs_store, parts.lhs);
		table.build_lhs = false;
		PnStore rhs_store;
		PnTypedIdx rhs_head = build_function(rhs_store, parts.rhs);

		tree::combine_layers(lhs_store, lhs_head);
		tree::combine_layers(rhs_store, rhs_head);
		if (const OptComplex lhs_val = tree::combine_values_exact(lhs_store, lhs_head)) {
			tree::free(lhs_store, lhs_head);
			lhs_head = PnTypedIdx(lhs_store.insert(*lhs_val), Type::complex);
		}
		if (const OptComplex rhs_val = tree::combine_values_exact(rhs_store, rhs_head)) {
			tree::free(rhs_store, rhs_head);
			rhs_head = PnTypedIdx(rhs_store.insert(*rhs_val), Type::complex);
		}
		tree::sort(lhs_store, lhs_head);
		tree::sort(rhs_store, rhs_head);

		std::cout << print::to_memory_layout(lhs_store, lhs_head) << "\n\n";
		//std::cout << print::to_memory_layout(rhs_store, rhs_head) << "\n\n";

		const auto result = find_value_match_subtree(lhs_store, lhs_head, table.value_table[0].lhs_instances[0]);
		std::cout << "index of lhs k = " << result.get_index() << "\n\n";
	}

	void change_subtree()
	{
		std::string name = "a+2*b^c";
		Term term(name);
		auto [store, head] = term.data();
		std::cout << term.to_string() << " -> ";

		TypedIdx a_idx = tree::search_variable(*store, head, "a");
		TypedIdx c_idx = tree::search_variable(*store, head, "c");
		tree::change_subtree(*store, head, c_idx, a_idx);
		tree::change_subtree(*store, head, a_idx, c_idx);
		std::cout << term.to_string() << "\n";
	}

} //namespace bmath::intern::test
