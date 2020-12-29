#pragma once

#include <iostream>
#include <charconv>
#include <cassert>
#include <numeric>

#include "termStore.hpp"
#include "arithmeticTerm.hpp"
#include "parseTerm.hpp"
#include "ioArithmetic.hpp"
#include "termVector.hpp"

namespace bmath::intern::debug {

	void enumerate_type()
	{
		using namespace pattern;
		std::cout
			<< "Type(Variadic::sum)            = " << unsigned(Type(Variadic::sum))            << "\n"
			<< "Type(Variadic::product)        = " << unsigned(Type(Variadic::product))        << "\n"
		                                                                                       << "\n"
			<< "Type(NamedFn{})                = " << unsigned(Type(NamedFn{}))                << "\n"
			                                                                                   << "\n"
			<< "Type(Fn::pow)                  = " << unsigned(Type(Fn::pow))                  << "\n"
			<< "Type(Fn::log)                  = " << unsigned(Type(Fn::log))                  << "\n"
			<< "Type(Fn::sqrt)                 = " << unsigned(Type(Fn::sqrt))                 << "\n"
			<< "Type(Fn::exp)                  = " << unsigned(Type(Fn::exp))                  << "\n"
			<< "Type(Fn::ln)                   = " << unsigned(Type(Fn::ln))                   << "\n"
			<< "Type(Fn::sin)                  = " << unsigned(Type(Fn::sin))                  << "\n"
			<< "Type(Fn::cos)                  = " << unsigned(Type(Fn::cos))                  << "\n"
			<< "Type(Fn::tan)                  = " << unsigned(Type(Fn::tan))                  << "\n"
			<< "Type(Fn::sinh)                 = " << unsigned(Type(Fn::sinh))                 << "\n"
			<< "Type(Fn::cosh)                 = " << unsigned(Type(Fn::cosh))                 << "\n"
			<< "Type(Fn::tanh)                 = " << unsigned(Type(Fn::tanh))                 << "\n"
			<< "Type(Fn::asin)                 = " << unsigned(Type(Fn::asin))                 << "\n"
			<< "Type(Fn::acos)                 = " << unsigned(Type(Fn::acos))                 << "\n"
			<< "Type(Fn::atan)                 = " << unsigned(Type(Fn::atan))                 << "\n"
			<< "Type(Fn::asinh)                = " << unsigned(Type(Fn::asinh))                << "\n"
			<< "Type(Fn::acosh)                = " << unsigned(Type(Fn::acosh))                << "\n"
			<< "Type(Fn::atanh)                = " << unsigned(Type(Fn::atanh))                << "\n"
			<< "Type(Fn::abs)                  = " << unsigned(Type(Fn::abs))                  << "\n"
			<< "Type(Fn::arg)                  = " << unsigned(Type(Fn::arg))                  << "\n"
			<< "Type(Fn::re)                   = " << unsigned(Type(Fn::re))                   << "\n"
			<< "Type(Fn::im)                   = " << unsigned(Type(Fn::im))                   << "\n"
			<< "Type(Fn::force)                = " << unsigned(Type(Fn::force))                << "\n"
			<< "Type(Fn::diff)                 = " << unsigned(Type(Fn::diff))                 << "\n"
			                                                                                   << "\n"
			<< "Type(Leaf::variable)           = " << unsigned(Type(Leaf::variable))           << "\n"
			<< "Type(Leaf::complex)            = " << unsigned(Type(Leaf::complex))            << "\n"
			                                                                                   << "\n"
			<< "Type(PnNode::tree_match)       = " << unsigned(Type(PnNode::tree_match))       << "\n"
			<< "Type(PnNode::value_match)      = " << unsigned(Type(PnNode::value_match))      << "\n"
			<< "Type(PnNode::value_proxy)      = " << unsigned(Type(PnNode::value_proxy))      << "\n"
			                                                                                   << "\n"
			<< "Type(MultiPn::summands)        = " << unsigned(Type(MultiPn::summands))        << "\n"
			<< "Type(MultiPn::factors)         = " << unsigned(Type(MultiPn::factors))         << "\n"
			<< "Type(MultiPn::params)          = " << unsigned(Type(MultiPn::params))          << "\n"
			                                                                                   << "\n"
			                                                                                   << "\n"
		;
	} //enumerate_type

	void test_rechner() 
	{
		static const auto patterns = std::to_array<pattern::PnTerm>({ 
			//{ "x :factors | 0 x = 0" },
			//{ "x          | 0^x = 0" },
			//{ "x          | x^0 = 1" },
			//{ "x          | x^1 = x" },
			//
			//{ "x, a, b | (x^a)^b = x^(a*b)" },
			//{ "x       | x x     = x^2" }, 
			//{ "x, a    | x x^a   = x^(a + 1)" },
			//{ "x, a, b | x^a x^b = x^(a + b)" },
			//{ "x :factors, y | exp(x ln(y)) = y^x" },
			//
			//{ "a, b          | a^2 + 2 a b   + b^2 = (a + b)^2" }, 
			//{ "a, b          | a^2 - 2 a b   + b^2 = (a - b)^2" }, 
			//{ "a :complex, b | a^2 + (2 a) b + b^2 = (a + b)^2" }, 
			//
			//{ "a :no_val, bs :factors, cs :factors | a bs + a cs = a (bs + cs)" },
			//{ "a :no_val, bs :factors              | a bs + a    = a (bs + 1)" }, 
			//{ "a :no_val                           | a    + a    = 2 a" }, 
			//{ "a :value, b, cs :summands           | a (b + cs)  = a b + a cs" }, 
			//
			//{ "a, as :summands |  -(a + as) =  -a - as" },
			//{ "a, as :factors  | 1/(a as)   = 1/a 1/as" },
			////{ " as :sum      | -as       =     sum:{ -a | a <- as}" }, //not yet writable, will perhaps never happen :|
			////{ " as :product  | 1/as      = product:{1/a | a <- as}" }, //not yet writable, will perhaps never happen :|
			//
			//{ "x | sin(x)^2 + cos(x)^2 = 1" },
			//
			////roots and extreme points of sin and cos:
			//{ "         cos(            'pi') = -1" },
			//{ "k :int | cos((k + 0.5)   'pi') =  0" },
			//{ "k :int | cos((2 k)       'pi') =  1" },
			//{ "k :int | cos((2 k + 1)   'pi') = -1" },
			//{ "         sin(            'pi') =  0" },
			//{ "k :int | sin(k           'pi') =  0" },
			//{ "k :int | sin((2 k + 0.5) 'pi') =  1" },
			//{ "k :int | sin((2 k + 1.5) 'pi') = -1" },
			//
			////differentiation rules:
			//{ "x :variable                      | diff(x, x)      = 1" },
			//{ "x :variable, a :variable         | diff(a, x)      = 0" },
			//{ "x :variable, a :value            | diff(a, x)      = 0" },
			//{ "x :variable, a :value, f :any    | diff(f^a, x)    = diff(f, x) a f^(a-1)" },
			//{ "x :variable, a :value, f :any    | diff(a^f, x)    = diff(f, x) ln(a) a^f" },
			//{ "x :variable, g :any, h :any      | diff(g^h, x)    = (diff(h, x) ln(g) + h diff(g, x)/g) g^h" },
			//{ "x :variable, u :any, v :summands | diff(u + v, x)  = diff(u, x) + diff(v, x)" },
			//{ "x :variable, u :any, v :factors  | diff(u v, x)    = diff(u, x) v + u diff(v, x)" },
			//{ "x :variable, f :any              | diff(sin(f), x) = diff(f, x) cos(f)" },
			//{ "x :variable, f :any              | diff(cos(f), x) = diff(f, x) (-sin(f))" },
			//{ "x :variable, f :any              | diff(exp(f), x) = diff(f, x) exp(f)" },
			//{ "x :variable, f :any              | diff(ln(f), x)  = diff(f, x) 1/f" },
			
			////exponential runtime fibonacci implementation:
			//{ "fib(0) = 0" },
			//{ "fib(1) = 1" },
			//{ "n :nat | fib(n) = fib(n - 1) + fib(n - 2)" },
			
			//reversing a list:
			{ "xs :params | reverse(list{xs}) = reverse'(list{}, list{xs})" },
			{ "xs :params, y, ys :params | reverse'(list{xs}, list{y, ys}) = reverse'(list{y, xs}, list{ys})" },
			{ "xs :params,               | reverse'(list{xs}, list{})      = list{xs}" },
			
			//listing first n fibonacci numbers:
			{ "n :nat0                    | fib_n(n + 2)                   = reverse(list_fibs(n, list{1, 0}))" },
			{ "n :nat, a, b, tail :params | list_fibs(n, list{a, b, tail}) = list_fibs(n - 1, list{force(a + b), a, b, tail})" },
			{ "              tail :params | list_fibs(0, list{tail})       = list{tail}" },
			
			//sorting numbers:
			{ "cond :not_positive, true_res, false_res | if_positive(cond, true_res, false_res) = false_res" },
			{ "cond :positive,     true_res, false_res | if_positive(cond, true_res, false_res) = true_res" },
			
			{ "p :real, xs :params, y :real, ys :params | filter_le(p, list{xs}, list{y, ys}) = filter_le(p, if_positive[force(p - y), list{xs}, list{xs, y}], list{ys})" },
			{ "p :real, xs :params,                     | filter_le(p, list{xs}, list{})      = list{xs}" },
			
			{ "p :real, xs :params, y :real, ys :params | filter_s(p, list{xs}, list{y, ys}) = filter_s(p, if_positive[force(p - y), list{xs, y}, list{xs}], list{ys})" },
			{ "p :real, xs :params,                     | filter_s(p, list{xs}, list{})      = list{xs}" },
			
			{ "p :real, xs :params | sort(list{p, xs}) = weird_concat(sort(filter_s(p, list{}, list{xs})), p, sort(filter_le(p, list{}, list{xs})))" },
			{ "                    | sort(list{})      = list{}" },
			{ "xs :params, y, zs :params | weird_concat(list{xs}, y, list{zs}) = list{xs, y, zs}" }, 		
		});

		for (const auto& p : patterns) {
			std::cout << p.to_string() << "\n";
			assert(tree::valid_storage(p.lhs_ref()));
			assert(tree::valid_storage(p.rhs_ref()));
			//std::cout << p.lhs_memory_layout() << "\n";
			//std::cout << p.rhs_memory_layout() << "\n";
			//std::cout << "lhs:\n" << p.lhs_tree() << "\n";
			//std::cout << "rhs:\n" << p.rhs_tree() << "\n\n\n";
		}
		std::cout << "\n\n";

		while (true) {
			std::string name;
			std::cout << "> ";
			std::getline(std::cin, name);
			try {
				bmath::Term test(name); 
				std::cout << "input:  " << test.to_string() << "\n";
				test.establish_order();
				bool changed;
				do {
					changed = false;
					for (const auto& p : patterns) {
						if (test.match_and_replace(p)) {
							//std::cout << "matched: " << p.to_string() << "\n";
							changed = true;
							test.establish_order();
							assert(tree::valid_storage(test.ref()));
							//std::cout << "    = " << test.to_string() << "\n";
							//std::cout << test.to_tree() << "\n";
							//std::cout << test.to_memory_layout() << "\n";
							break;
						}
					}
				} while (changed);
				std::cout << "result:   " << test.to_pretty_string() << "\n";
				//std::cout << test.to_memory_layout() << "\n";
				//std::cout << test.to_tree() << "\n";
				std::cout << "\n";

				tree::free(test.mut_ref());
				assert(test.store.storage_occupancy().none());
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
		//{ "x, a    | x x^a   = x^(a + 1)" };
		auto p = pattern::PnTerm("x       | x x     = x^2");
		std::string t_name = "a / 0";
		auto t = Term(t_name);
		t.establish_order();
		auto m = pattern::match::MatchData{};
		std::cout << "match lhs of \"" << p.to_string() << "\" with \"" << t.to_string() << ": " << pattern::match::permutation_equals(p.lhs_ref(), t.ref(), m) << "\n";
	}

	void combine()
	{
		while (true) {
			//difficult case: "5 ((3 a) + 0)"
			std::string name; 
			std::cout << "combine> ";
			std::getline(std::cin, name);
			try {
				bmath::Term version_1(name); 
				std::cout << "as parsed:\n" << version_1.to_tree() << "\n";
				version_1.establish_order();
				std::cout << "standardized:\n" << version_1.to_tree() << "\n";
			}
			catch (bmath::ParseFailure failure) {
				std::cout << "parse failure: " << failure.what << '\n';
				std::cout << name << '\n';
				std::cout << std::string(failure.where, ' ') << "^\n\n";
			}
			std::cout << "--------------------------------------------------------------------------\n";
		}
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
		using Ref_T = BasicNodeRef<StringArray, StringArray, Const::no>;
	
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


} //namespace bmath::intern::test
