#include <iostream>
#include <charconv>
#include <cassert>

#include "termStore.hpp"
#include "termColony.hpp"
#include "arithmeticTerm.hpp"
#include "parseTerm.hpp"

using namespace bmath::intern::arithmetic;
using namespace bmath::intern;

int main()
{
	{
		std::string parentheses = "([[]])()[(){()()}]({}{}{()})";
		TokenString tokens = tokenize(parentheses);
		std::size_t open = parentheses.find_first_of('{', 0);
		std::size_t closed = find_closed_par(open, TokenView(tokens));
		std::cout << parentheses << '\n';
		std::cout << std::string(open, ' ') << "^open\n";
		std::cout << std::string(closed, ' ') << "^closed\n";
	}
	{
		std::string term_name = "-(b+c)*2i-";
		try {
			remove_whitspace(term_name);
			auto tokens = tokenize(term_name);
			allow_implicit_product(tokens, term_name);
			assert(is_arithmetic(TokenView(tokens)));
			ArithmeticStore term(term_name.size());
			auto head = build(term, TokenView(tokens), term_name, 0);

			std::string term_str;
			to_string(term, head, term_str);
			std::cout << "to_string: " << term_str << std::endl;
			std::cout << bmath::ArithmeticTerm{ head, term }.to_debug_view() << '\n';
		}
		catch (ParseFailure failure) {
			switch (failure.what) {
			case ParseFailure::What::illegal_char:
				std::cout << "error while building: encountered illegal char:\n";
				break;
			case ParseFailure::What::poor_grouping:
				std::cout << "error while building: encountered poor grouping:\n";
				break;
			case ParseFailure::What::illegal_ops:
				std::cout << "error while building: encountered illegal operator placement:\n";
				break;
			case ParseFailure::What::illformed_val:
				std::cout << "error while building: encountered illformed value:\n";
				break;
			}
			std::cout << term_name << '\n';
			std::cout << std::string(failure.where, ' ') << "^\n\n";
		}

		double result;
		std::from_chars(term_name.data(), term_name.data() + term_name.size(), result);
		std::cout << result << '\n';
	}
	{
		bmath::ArithmeticTerm term;
		auto val1_idx = term.values.emplace_new(Complex{ {3.0, 0.0} });
		auto val2_idx = term.values.emplace_new(Complex{ {12.0, 0.0} });
		auto val1_ref = TypedIdx(val1_idx, Type::complex);
		auto val2_ref = TypedIdx(val2_idx, Type::complex);
		auto sum_idx = term.values.emplace_new(Sum{ {val1_ref, val2_ref} });
		auto sum_ref = TypedIdx(sum_idx, Type::sum);
		auto var_idx = insert_string(term.values, "DerWeinachtsmannVomNordpol");
		auto var_ref = TypedIdx(var_idx, Type::variable);
		std::uint32_t product_idx = term.values.emplace_new(Product{ {sum_ref, var_ref} });
		auto product_ref = TypedIdx(product_idx, Type::product);
		for (int i = 0; i < 10; i++) {
			std::string term_str;
			to_string(term.values, product_ref, term_str);
			std::cout << term_str << std::endl;
			insert_new<ToProduct>(term.values, product_idx, val1_ref);
		}

		std::string term_str;
		to_string(term.values, product_ref, term_str);
		std::cout << term_str << std::endl;
		std::cout << eval(term.values, sum_ref) << std::endl;
	}
	{
		TermStore<TypesUnion> store;
		std::string_view info = "ich bin bruno und ich bin der kameramann";
		std::uint32_t head = insert_string(store, info);
		insert_new<ToString>(store, head, '.');

		std::string output;
		read<ToConstString>(store, head, output);
		std::cout << output << '\n';
		for (auto& c : range<ToString>(store, head)) {
			if (c % 2 == 0) {
				c = '\0';
			}
		}
		sort<ToString>(store, head, [](auto& c1, auto& c2) { return c1 < c2; });

		output.clear();
		read<ToConstString>(store, head, output);
		std::cout << output << '\n';
	}
}