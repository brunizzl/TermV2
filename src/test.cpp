#include "test.hpp"

#include <vector>
#include <iostream>
#include <functional>
#include <charconv>
#include <type_traits>

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
			Store store_1;
			Store store_2;
			auto term_1 = LiteralTerm(name_1, store_1);
			auto term_2 = LiteralTerm(name_2, store_2);
			term_1.normalize({});
			term_2.normalize({});

			if (rules.heads.size()) {
				//only apply rules to left term of pair
				term_1.head = greedy_lazy_apply_ruleset(rules, term_1.mut_ref(), {}).term;
			}

			if (compare_tree(term_1.ref(), term_2.ref()) != std::strong_ordering::equal) {
				std::cout << "ASSERTION FAILURE\nwith ruleset:\n\n";
				for (const simp::RuleRef& rule : rules) {
					std::cout << rule.to_string() << "\n\n";
				}
				auto unnormalized_1 = LiteralTerm(name_1, store_1);
				auto unnormalized_2 = LiteralTerm(name_2, store_2);
				std::cout << "\ncould not reach equality for terms:\n";
				std::cout << unnormalized_1.to_string() << "\n";
				std::cout << unnormalized_2.to_string() << "\n";
				std::cout << "\nnormalized to:\n";
				std::cout << term_1.to_string() << "\n";
				std::cout << term_2.to_string() << "\n";

				simp::free_tree(unnormalized_1.mut_ref());
				simp::free_tree(unnormalized_2.mut_ref());

				std::string enter_container;
				std::getline(std::cin, enter_container);
			}

			assert((simp::free_tree(term_1.mut_ref()), term_1.store.nr_used_slots() == 0u));
			assert((simp::free_tree(term_2.mut_ref()), term_2.store.nr_used_slots() == 0u));
		}
	} //assert_eqivalent_normal_forms

	void run_tests()
	{
		std::cout << "running tests...\n";
		
		assert_eqivalent_normal_forms({
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

		assert_eqivalent_normal_forms({
			{ "x^2 + y^2 + z^2 + 2 x z", "(x + z)^2 + y^2" },
			{ "x^2 + y^2 + z^2 - 2 x z", "(x - z)^2 + y^2" },
			{ "81 +18 sin(x) + sin(x)^2", "(9 + sin(x))^2" },
			{ "(x y)^2 + z^2 + 2 x y z", "(x y + z)^2" },
		}, {
			{ "_a^2 + 2 _a _b + _b^2 + cs... = (_a + _b)^2 + cs..." },
			{ "_a^2 - 2 _a _b + _b^2 + cs... = (_a - _b)^2 + cs..." },
			{ "$a^2 + 2 $a _b + _b^2 + cs... = ($a + _b)^2 + cs..." },
			{ "$a^2 - 2 $a _b + _b^2 + cs... = ($a - _b)^2 + cs..." },
		});
			
		assert_eqivalent_normal_forms({
			{ "cos(pi)", "-1" },
			{ "cos((1e12 + 1/2) pi)", "0" },
			{ "sin(9.5 pi)", "-1" },
			{ "sin(9.4999 pi)", "sin(9.4999 pi)" },
		}, {
			{ "cos(   pi)                     = -1" },
			{ "cos(_a pi) | _a + 1/2     :int =  0" },
			{ "sin((2 $k + 1.5) pi) | $k :int = -1" },
		});

		assert_eqivalent_normal_forms({
			{ "diff(t t t, t)", "t t + t (t + t)" },
			{ "fdiff(\\x.x x)(t)", "t + t" },
		}, {
			{ "diff(_u + vs..., _x) = diff(_u, _x) + diff(sum(vs...), _x)" },
			{ "diff(_u vs..., _x) = diff(_u, _x) vs... + _u diff(prod(vs...), _x)" },
			{ "diff(_x, _x) = 1" },
			{ "fdiff(\\x ._y) = \\x .diff(_y, x)" },
		});

		assert_eqivalent_normal_forms({
			{ "fibonacci(0)", "0" },
			{ "fibonacci(1)", "1" },
			{ "fibonacci(2)", "1" },
			{ "fibonacci(3)", "2" },
			{ "fibonacci(4)", "3" },
			{ "fibonacci(5)", "5" },
			{ "fibonacci(6)", "8" },
		}, {
			{ "fibonacci(_n) | _n : int  = loop(0, 1, _n)" },
			{ "loop(_curr, _next, _left) = (_left > 0)(loop(_next, _curr + _next, _left - 1), _curr)" },
		});

		assert_eqivalent_normal_forms({
			{ "fibonacci(0)", "0" },
			{ "fibonacci(1)", "1" },
			{ "fibonacci(2)", "1" },
			{ "fibonacci(3)", "2" },
			{ "fibonacci(4)", "3" },
			{ "fibonacci(5)", "5" },
			{ "fibonacci(6)", "8" },
		}, {
			//some more elaborate lambda evaluation using a fix-point-operator.
			//note: "loop" is no entity on its own, thus it needs to always carry itself with it.
			{ "fibonacci = \\n . (\\f . f(f, 0, 1, n))(\\self curr next left . (left > 0)(self(self, next, curr + next, left - 1), curr))" },
		});


		std::cout << "...finished running tests!\n\n";
	} //run_tests

	void read_eval_print_loop(RuleSet&& rules, REPL_Options options)
	{

		struct Command
		{
			std::string_view name;
			std::string_view usage;
			std::string_view description;
			std::function<void(std::string&)> effect;
		};

		const auto parse_int = [](auto& dest, const std::string_view name, unsigned range_end) {
			return [&dest, name, range_end](std::string& in) {
				in.erase(0, in.find_first_not_of(" \t"));
				unsigned val = 0;
				const auto [read_end, err] = std::from_chars(in.data(), in.data() + in.size(), val);
				if (err != std::errc{} || val >= range_end) { 
					std::cout << "could not interpret \"" << in << "\" as unsigned int smaller than " << range_end << ".\n\n";
				}
				else {
					std::cout << "set " << name << " to " << val << "\n\n";
					dest = static_cast<std::remove_reference_t<decltype(dest)>>(val);
				}
			};
		};

		const std::vector<Command> commands = { 
			{ 
				"exact", 
				":exact = <0 | 1>",
				"only evaluate an expression, when the result can be represented exactly as floating point number", 
				parse_int(options.exact, "exact", 2)
			},
			{ 
				"memory",
				":memory = <0 | 1>",
				"print memory layout of input both after parsing and after evaluating", 
				parse_int(options.show_memory, "memory", 2) 
			},
			{
				"verb",
				":verb = <0 | 1 | 2 | 3 | 4>",
				"show intermediate results / + applied rule / + terms tested for rule applicability / + rules tested",
				parse_int(debug_print_level, "verb", 5)
			},
			{ 
				"add",
				":add = <rule>",
				"adds rule to ruleset", 
				[&](std::string& input) {
					const DebugPrintLevel old = std::exchange(debug_print_level, DebugPrintLevel::none);
					try {
						const auto new_rule = RuleSet({ {input.data(), input.size()} });
						rules.add({ &new_rule });
						for (const simp::RuleRef& ref : new_rule) { //a bit bulky, but easiest way to get a RuleRef
							std::cout << "added\n" << ref.to_string() << "\n\n";
						}
					}
					catch (...) {} //exception output done by RuleSet constructor
					debug_print_level = old;
				} 
			},
			{
				"quit",
				":quit",
				"quits the program",
				[&](std::string&) { options.quit = true; }
			},
			{
				"rules",
				":rules",
				"shows all rules in use",
				[&](std::string&) { std::cout << rules.to_string() << "\n"; }
			},
			{ //has to be last command in vector, see below
				"help",
				":<anything not interpreted as another command>",
				"help info showing the avaliable commands", 
				[&](std::string&) {
					std::cout << "could not interpret command. try one of these instead:\n\n";
					for (const Command& c : commands) {
						std::cout << c.name <<
							"\n    usage      : " "\"" << c.usage << "\""
							"\n    description: " << c.description << "\n\n";
					}
					std::cout << "\n";
				} 
			},
		};

		while (!options.quit) {
			std::string input;
			std::cout << "simp> ";
			std::getline(std::cin, input);
			if (input.starts_with(':')) { //a command is entered 
				input.erase(0, std::strlen(":"));

				assert(commands.back().name == "help"); //if an unknown command is entered, help info will be displayed...
				const auto command = std::find_if(commands.begin(), std::prev(commands.end()), //...as an unsuccessful search returns the second parameter
					[&](const Command& c) { return input.starts_with(c.name); });

				input.erase(0, command->name.size());
				command->effect(input);
			}
			else { //no command -> parse as term and try to simplify
				Store store;
				try {
					auto term = simp::LiteralTerm(input, store);
					if (options.show_memory) 
						std::cout << term.to_memory_layout() << "\n\n";

					term.normalize({ .exact = options.exact });
					if (rules.heads.size()) 
						term.head = simp::greedy_lazy_apply_ruleset(rules, term.mut_ref(), { .exact = options.exact }).term;

					std::cout << " = " << term.to_string() << "\n\n";
					if (options.show_memory) 
						std::cout << term.to_memory_layout() << "\n\n";

					assert((simp::free_tree(term.mut_ref()), term.store.nr_used_slots() == 0u));
				}
				catch (simp::ParseFailure failure) {
					std::cout << "parse failure: " << failure.what << '\n';
					std::cout << input << '\n';
					std::cout << std::string(failure.where, ' ') << "^\n\n";
				}
				catch (simp::TypeError error) {
					std::cout << "type error: " << error.what << "\n";
					std::cout << simp::print::term_to_string(error.occurence) << "\n\n";
				}
			}
		}
		debug_print_level = DebugPrintLevel::none;
	} //read_eval_print_loop

} //namespace simp::test