#include "types.hpp"

#include <string>

#include "io.hpp"

namespace simp {

	simp::Literal::Literal(std::string name)
	{
		auto parse = bmath::intern::ParseString(name);
		parse.mark_char_space();
		parse.remove_space();
		name_lookup::LiteralInfos lambda_params;
		this->head = simp::build(this->store, lambda_params, parse);
	}

	std::string Literal::to_string() const noexcept
	{
		std::string res;
		print::append_to_string(this->ref(), res, 0);
		return res;
	}

} //namespace simp