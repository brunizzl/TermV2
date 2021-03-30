#include "types.hpp"

#include <string>
#include <tuple>

#include "parseTerm.hpp"
#include "io.hpp"

namespace simp {

	simp::Literal::Literal(std::string name)
	{
		auto parse_str = bmath::intern::ParseString(name);
		parse_str.mark_char_space();
		parse_str.remove_space();
		parse::name_lookup::LiteralInfos lambda_params;
		this->head = parse::build(this->store, lambda_params, parse_str);
	}

	std::string Literal::to_string() const noexcept
	{
		std::string res;
		print::append_to_string(this->ref(), res, 0);
		return res;
	}

	RewriteRule::RewriteRule(std::string name)
	{
		{ //value match is not yet functional
			const auto [lhs, rhs] = parse::build_simple_rule(this->store, std::move(name));
			this->lhs_head = lhs;
			this->rhs_head = rhs;
		}
	}

	std::string RewriteRule::to_string() const noexcept
	{
		std::string res;
		print::append_to_string(Ref(this->store, this->lhs_head), res, 0);
		res.append(" = ");
		print::append_to_string(Ref(this->store, this->rhs_head), res, 0);
		return res;
	}

} //namespace simp
