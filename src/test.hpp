#pragma once

#include <vector>
#include <utility>
#include <string>

#include "types.hpp"
#include "rewrite.hpp"

namespace simp::test {

	void print_native_symbols();

	void print_node_types();

	void assert_eqivalent_normal_forms(const std::vector<std::pair<std::string, std::string>>& eq_pairs, const RuleSet& rules);

	void run_tests();

	struct REPL_Options
	{
		bool quit = false;
		bool exact = true;
		bool show_memory = false;
		bool show_tree = false;
	};

	void read_eval_print_loop(RuleSet&& rules = {}, REPL_Options options = {});

} //namespace simp::test