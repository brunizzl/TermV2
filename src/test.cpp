#include "test.hpp"

#include <iostream>

namespace simp::test {
	void print_native_symbols()
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
		std::cout << "\n\n";
	} //print_native_symbols

	void print_node_types()
	{
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
	} //print_node_types

	void assert_eqivalent_normal_forms(const std::vector<std::pair<std::string, std::string>>& eq_pairs, const RuleSet& rules)
	{
		for (const auto& [name_1, name_2] : eq_pairs) {
			auto term_1 = LiteralTerm(name_1);
			auto term_2 = LiteralTerm(name_2);
			term_1.normalize({});
			term_2.normalize({});

			if (rules.rules.size()) {
				term_1.head = greedy_apply_ruleset(rules, term_1.mut_ref(), {});
				term_2.head = greedy_apply_ruleset(rules, term_2.mut_ref(), {});
			}

			if (compare_tree(term_1.ref(), term_2.ref()) != std::strong_ordering::equal) {
				std::cout << "ASSERTION FAILURE\nwith ruleset:\n\n";
				for (const simp::RuleRef rule : rules) {
					std::cout << rule.to_string() << "\n\n";
				}
				std::cout << "\ncould not reach equality for terms:\n";
				std::cout << LiteralTerm(name_1).to_string() << "\n";
				std::cout << LiteralTerm(name_2).to_string() << "\n";
				std::cout << "\nnormalized to:\n";
				std::cout << term_1.to_string() << "\n";
				std::cout << term_2.to_string() << "\n";

				std::string enter_container;
				std::getline(std::cin, enter_container);
			}

			assert((simp::free_tree(term_1.mut_ref()), term_1.store.nr_used_slots() == 0u));
			assert((simp::free_tree(term_2.mut_ref()), term_2.store.nr_used_slots() == 0u));
		}
	} //assert_eqivalent_normal_forms

	void test_stuff()
	{
		std::cout << "testing...\n";
		
		simp::test::assert_eqivalent_normal_forms({
			{ "tup(conj(3-1i), conj(4+3i), conj(8), conj(-1i))", "tup(3+1i, 4-3i, 8, 1i)" }, 
			{ "tup(3 == 4, 3 != 4, 3 < 4, 3 <= 4, 3 >= 4, 3 > 4)", "tup(false, true, true, true, false, false)" }, 
			{ "tup(4 == 4, 4 != 4, 4 < 4, 4 <= 4, 4 >= 4, 4 > 4)", "tup(true, false, false, true, true, false)" }, 
			{ "tup(x == 4, x != 4, x < 4, x <= 4, x >= 4, x > 4)", "tup(false, true, x < 4, x <= 4, x >= 4, x > 4)" }, 
			{ "tup(floor(4.2), floor(3.9), floor(2-1i), ceil(3.9), ceil(4), ceil(4+1i))", "tup(4, 3, floor(2-1i), 4, 4, ceil(4+1i))" }, 
			{ "set(1, -4, a, 12, 13+3i, 13+4i, 13-1i, 13, 13+1i, -10)", "set(-10, -4, 1, 12, 13-1i, 13, 13+1i, 13+3i, 13+4i, a)" }, 
			{ "true(x, y)", "x" }, 
			{ "false(x, y)", "y" }, 
			{ "4^2", "16" }, 
			{ "10/5", "2" }, 
			{ "\\x.\\y.\\z. x + y + z", "\\a.(\\b.(\\c. a + b + c))" }, 
			{ "sin(x) + 3 + cos(x) - 5", "cos(x) - 2 + sin(x)" }, 
			{ "(\\x y. x y)(a, 4)", "4 a" }, 
			{ "4^(1/2)", "2" }, 
			{ "2^(1/2)", "2^(0.5)" }, 
			{ "map(sum, \\x. -x, sum(a, b, sin(x), 3, 5))", "-3 - 5 - sin(x) - a - b" }, 
			{ "berb && frobbl && true && (false || !false || schmenck) && true && !alf", "and(berb, frobbl, not(alf))" }, 
			{ "set(1, 100, a, b, 50 + 2 * 25, a, (\\x.2 x)(50))", "set(1, 100, a, b)" }, 
			{ "(\\f n. f(f, n))(\\f n.(n <= 1)(1, n * f(f, -1 + n)), 5)", "120" }, 
			{ "(\\f n. f(f, n))(\\f n.(n == 1)(1, n * f(f, -1 + n)), 5)", "120" }, 
			{ "(\\x .\\y .x(y))(\\x .\\y .x(y))(\\x .x)(jens)", "jens" }, 
		}, {});

		simp::test::assert_eqivalent_normal_forms({
			{ "x^2 + y^2 + z^2 + 2 x z", "(x + z)^2 + y^2" },
			{ "x^2 + y^2 + z^2 - 2 x z", "(x - z)^2 + y^2" },
			{ "81 +18 sin(x) + sin(x)^2", "(9 + sin(x))^2" },
			{ "(x y)^2 + z^2 + 2 x y z", "(x y + z)^2" },
		}, {
			{ "_a^2 + 2 _a _b + _b^2 = (_a + _b)^2" },
			{ "_a^2 - 2 _a _b + _b^2 = (_a - _b)^2" },
			{ "$a^2 + 2 $a _b + _b^2 = ($a + _b)^2" },
			{ "$a^2 - 2 $a _b + _b^2 = ($a - _b)^2" },
		});
			
		simp::test::assert_eqivalent_normal_forms({
			{ "cos(pi)", "-1" },
			{ "cos((1e12 + 1/2) pi)", "0" },
			{ "sin(9.5 pi)", "-1" },
		}, {
			{ "cos(   pi)                     = -1" },
			{ "cos(_a pi) | _a + 1/2     :int =  0" },
			{ "sin((2 $k + 1.5) pi) | $k :int = -1" },
		});


		std::cout << "...done!\n\n";
	} //test_stuff

	void read_eval_print_loop(RuleSet&& rules, bool exact, bool show_memory, bool show_tree)
	{
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
			if (name.starts_with("--add")) {
				try {
					std::cout << "\n";
					name.erase(0, std::strlen("--add"));
					auto new_rule = RuleSet({ {name.data(), name.size()} });
					rules.add({ &new_rule });
					for (const simp::RuleRef ref : new_rule) { //a bit bulky, but easiest way to get a RuleRef
						std::cout << "added\n" << ref.to_string() << "\n\n";
					}
				}
				catch (...) {}
				continue;
			}
			try {
				auto term = simp::LiteralTerm(name);
				if (show_memory) std::cout << term.to_memory_layout() << "\n\n\n";
				term.normalize({ .exact = exact });
				term.head = simp::greedy_apply_ruleset(rules, term.mut_ref(), { .exact = exact });
				std::cout << " = " << term.to_string() << "\n\n";
				if (show_memory) std::cout << term.to_memory_layout() << "\n\n\n";

				assert((simp::free_tree(term.mut_ref()), term.store.nr_used_slots() == 0u));
			}
			catch (simp::ParseFailure failure) {
				std::cout << "parse failure: " << failure.what << '\n';
				std::cout << name << '\n';
				std::cout << std::string(failure.where, ' ') << "^\n\n";
			}
			catch (simp::TypeError error) {
				std::cout << "type error: " << error.what << "\n";
				std::cout << simp::print::to_string(error.occurence) << "\n";
			}
		}
	} //read_eval_print_loop

} //namespace simp::test