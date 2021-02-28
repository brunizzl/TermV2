#pragma once

#include <string>
#include <string_view>
#include <typeinfo>
#include <iostream>

namespace bmath::intern {

	std::string compact_type_name(std::string_view type_name, bool multiline = false);

} //namespace bmath::intern

