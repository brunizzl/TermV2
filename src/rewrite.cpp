#include "rewrite.hpp"

#include <string>
#include <tuple>

#include "algorithms.hpp"
#include "parseTerm.hpp"
#include "io.hpp"

namespace simp {

	simp::LiteralTerm::LiteralTerm(std::string name)
	{
		using namespace bmath::intern;
		auto parse_str = ParseString(name);
		parse_str.allow_implicit_product(token::sticky_space, ' ');
		parse_str.remove_space();
		parse::name_lookup::LiteralInfos lambda_params;
		this->head = parse::build(this->store, lambda_params, parse_str);
	}

	std::string LiteralTerm::to_string() const noexcept
	{
		std::string res;
		print::append_to_string(this->ref(), res, 0);
		return res;
	}

	std::string LiteralTerm::to_memory_layout() const noexcept
	{
		return print::to_memory_layout(this->store, {this->head});
	}

	RewriteRule::RewriteRule(std::string name)
	{
		parse::PatternPair heads = parse::raw_rule(this->store, std::move(name));
		heads = build_pattern::prime_multi(this->store, heads);
		this->lhs_head = heads.lhs;
		this->rhs_head = heads.rhs;
	}

	std::string RewriteRule::to_string() const noexcept
	{
		std::string res;
		print::append_to_string(this->lhs_ref(), res, 0);
		res.append(" = ");
		print::append_to_string(this->rhs_ref(), res, 0);
		return res;
	}

} //namespace simp
