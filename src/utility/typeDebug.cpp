#include "typeDebug.hpp"

namespace bmath::intern {

	std::string compact_type_name(std::string_view type_name) 
	{
		auto result = std::string(type_name);
		{
			std::size_t double_colon = result.find(std::string_view("::"));
			while (double_colon != std::string::npos) {
				const std::size_t namespace_begin = result.find_last_of(' ', double_colon) + 1u;
				result.erase(namespace_begin, double_colon - namespace_begin + 2u);
				double_colon = result.find(std::string_view("::"));
			}
		}

		const auto strip_keyword = [&result](std::string_view keyword_name) {
			std::size_t keyword_pos = result.find(keyword_name);
			while (keyword_pos != std::string::npos) {
				result.erase(keyword_pos, keyword_name.size());
				keyword_pos = result.find(keyword_name);
			}
		};
		strip_keyword("struct ");
		strip_keyword("class ");
		strip_keyword("enum ");
		strip_keyword(" "); //not really a keyword, but you get the idea.

		{
			std::size_t after_comma = result.find_first_of(',') + 1u;
			while (after_comma != (std::string::npos + 1u)) { 
				result.insert(after_comma, 1u, ' ');
				after_comma = result.find_first_of(',', after_comma) + 1u;
			}
		}
		return result;
	}

} //namespace bmath::intern