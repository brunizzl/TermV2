
#include <cassert>
#include <sstream>
#include <array>
#include <algorithm>

#include "termUtility.hpp"
#include "arithmeticTerm.hpp"
#include "termColony.hpp"

/*
	void prototype(const TermStore<TypesUnion> & store, TypedIdx ref)
	{
		const std::uint32_t index = ref.get_index();
		switch (ref.get_type()) {
		case Type::sum: {
			const Sum& sum = store.at(index).sum;
			assert(false);
		} break;
		case Type::product:  {
			const Product& product = store.at(index).product;
			assert(false);
		} break;             
		case Type::known_function: {
			const KnownFunction& known_function = store.at(index).known_function;
			assert(false);
		} break;
		case Type::unknown_function: {
			const UnknownFunction& unknown_function = store.at(index).unknown_function;
			assert(false);
		} break;          
		case Type::variable: {
			const Variable& variable = store.at(index).variable;
			assert(false);
		} break;   
		case Type::complex: {
			const Complex& complex = store.at(index).complex;
			assert(false);
		} break;
		default: assert(false); //if this assert hits, the switch above needs more cases.
		}
	} //prototype
*/

namespace bmath::intern::arithmetic {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////internal to file/////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	namespace print {

		int operator_precedence(Type type) {
			switch (type) {
			case Type::known_function:		return 1;	//lower order, because it already brings its own parentheses.
			case Type::unknown_function:	return 1;	//lower order, because it already brings its own parentheses.
			case Type::sum:					return 2;
			case Type::product:				return 4;
			case Type::variable:			return 6;
			case Type::complex:				return 6;	//may be printed as sum/product itself, then (maybe) has to add parentheses on its own

			case Type::COUNT:               return -1;	//default value -> no parenteses around wohle thing
			}
			assert(false);
			return 0;
		};

		bool needs_parentheses(Type type)
		{
			switch (type) {
			case Type::known_function:		return false;
			case Type::unknown_function:	return false;
			case Type::sum:					return true;
			case Type::product:				return true;
			case Type::variable:			return true;
			case Type::complex:				return false;

			case Type::COUNT:               return false;
			}
			assert(false);
			return 0;
		}

		void append_complex(const std::complex<double> val, std::string& dest, int parent_operator_precedence, bool inverse)
		{
			const auto add_im_to_stream = [](std::stringstream& buffer, const double im, bool showpos) {
				if (im == -1) {
					buffer << '-';
				}
				else if (im == 1) {
					if (showpos) {
						buffer << '+';
					}
				}
				else {
					buffer << (showpos ? std::showpos : std::noshowpos) << im;
				}
				buffer << 'i';
			};

			const double re = inverse ? -(val.real()) : val.real();
			const double im = inverse ? -(val.imag()) : val.imag();
			bool parentheses = false;
			std::stringstream buffer;

			if (re != 0 && im != 0) {
				parentheses = parent_operator_precedence > operator_precedence(Type::sum);
				buffer << re;
				add_im_to_stream(buffer, im, true);		
			}
			else if (re != 0 && im == 0) {
				parentheses = re < 0 && parent_operator_precedence > operator_precedence(Type::sum);	//leading '-'
				buffer << re;
			}
			else if (re == 0 && im != 0) {
				parentheses = im < 0 && parent_operator_precedence > operator_precedence(Type::sum);	//leading '-'	
				parentheses |= parent_operator_precedence > operator_precedence(Type::product);	//*i
				add_im_to_stream(buffer, im, false);
			}
			else {
				buffer << '0';
			}

			if (parentheses) {
				dest.push_back('(');
				dest.append(buffer.str());
				dest.push_back(')');
			}
			else {
				dest.append(buffer.str());
			}
		} //append_complex

	} //namespace print


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////exported in header///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	namespace fn {

		std::string_view name_of(FunctionType type) noexcept
		{
			switch (type) {
			case FunctionType::asinh: return std::string_view("asinh");	
			case FunctionType::acosh: return std::string_view("acosh");
			case FunctionType::atanh: return std::string_view("atanh");	
			case FunctionType::asin:  return std::string_view("asin");	
			case FunctionType::acos:  return std::string_view("acos");	
			case FunctionType::atan:  return std::string_view("atan");	
			case FunctionType::sinh:  return std::string_view("sinh");	
			case FunctionType::cosh:  return std::string_view("cosh");	
			case FunctionType::tanh:  return std::string_view("tanh");	
			case FunctionType::sqrt:  return std::string_view("sqrt");	
			case FunctionType::pow:   return std::string_view("pow");    
			case FunctionType::log:   return std::string_view("log");	
			case FunctionType::exp:   return std::string_view("exp");	
			case FunctionType::sin:   return std::string_view("sin");	
			case FunctionType::cos:   return std::string_view("cos");	
			case FunctionType::tan:   return std::string_view("tan");	
			case FunctionType::abs:   return std::string_view("abs");	
			case FunctionType::arg:   return std::string_view("arg");	
			case FunctionType::ln:    return std::string_view("ln");		
			case FunctionType::re:    return std::string_view("re");		
			case FunctionType::im:    return std::string_view("im");	
			default: 
				assert(false);
				return std::string_view("");
			}
		} //name_of

		void append_name(const Store& store, const UnknownFunction& func, std::string& str)
		{
			if (func.name_size == UnknownFunction::NameSize::small) {
				str.append(func.short_name);
			}
			else {
				read<ToConstString>(store, func.long_name_idx, str);
			}
		}

		  //only expects actual name part of function, e.g. "asin", NOT "asin(...)"
		  //if name is one of FunctionType, that is returned, else FunctionType::UNKNOWN
		FunctionType type_of(const ParseView input) noexcept
		{
			const auto name = input.to_string_view();
			for (FunctionType type = static_cast<FunctionType>(0); 
				type != FunctionType::UNKNOWN; 
				type = static_cast<FunctionType>(static_cast<int>(type) + 1)) 
			{
				if (name_of(type) == name) [[unlikely]] {
					return type;
				}
			}
			//special syntax allowed for log, to also accept "logn()" for any natural n
			if (name.starts_with("log") && 
				input.tokens.find_first_not_of(std::strlen("log"), token::number) == TokenView::npos) {
				return FunctionType::log;
			}
			return FunctionType::UNKNOWN;
		} //type_of

		std::size_t param_count(FunctionType type) noexcept
		{
			switch (type) {
			case FunctionType::asinh: return 1;
			case FunctionType::acosh: return 1;
			case FunctionType::atanh: return 1;
			case FunctionType::asin:  return 1;
			case FunctionType::acos:  return 1;
			case FunctionType::atan:  return 1;
			case FunctionType::sinh:  return 1;
			case FunctionType::cosh:  return 1;
			case FunctionType::tanh:  return 1;
			case FunctionType::sqrt:  return 1;
			case FunctionType::pow:   return 2; //<- only for these fuckers >:(
			case FunctionType::log:   return 2;	//<- only for these fuckers >:(
			case FunctionType::exp:   return 1;
			case FunctionType::sin:   return 1;
			case FunctionType::cos:   return 1;
			case FunctionType::tan:   return 1;
			case FunctionType::abs:   return 1;
			case FunctionType::arg:   return 1;
			case FunctionType::ln:    return 1;	
			case FunctionType::re:    return 1;	
			case FunctionType::im:    return 1;
			default: 
				assert(false);
				return 0;
			}
		} //param_count

	} //namespace fn

	std::complex<double> eval(const Store& store, TypedIdx ref)
	{
		const std::uint32_t index = ref.get_index();
		switch (ref.get_type()) {
		case Type::sum: {
			std::complex<double> value = 0.0;
			for (const auto elem : vd::range(store, index)) {
				value += eval(store, elem);
			}
			return value;
		} break;
		case Type::product:  {
			std::complex<double> value = 1.0;
			for (const auto elem : vd::range(store, index)) {
				value *= eval(store, elem);
			}
			return value;
		} break;             
		case Type::known_function: {
			const KnownFunction& known_function = store.at(index).known_function;
			assert(false);
		} break;
		case Type::unknown_function: {
			const UnknownFunction& unknown_function = store.at(index).unknown_function;
			assert(false);
		} break;      
		case Type::variable: {
			throw std::exception("eval found variable in term");
		} break;   
		case Type::complex: {
			const Complex& complex = store.at(index).complex;
			return complex;
		} break;
		default: assert(false); //if this assert hits, the switch above needs more cases.
		}
		return std::complex<double>(0.0, 0.0);
	} //eval

	void to_string(const Store& store, TypedIdx ref, std::string& str, const int parent_precedence)
	{
		const int own_precedence = print::operator_precedence(ref.get_type());
		if (own_precedence < parent_precedence && print::needs_parentheses(ref.get_type())) {
			str.push_back('(');
		}

		const std::uint32_t index = ref.get_index();
		switch (ref.get_type()) {
		case Type::sum: {
			bool first = true;
			for (const auto elem : vd::range(store, index)) {
				if (!std::exchange(first, false)) {
					str.push_back('+');
				}
				to_string(store, elem, str, own_precedence);
			}
		} break;
		case Type::product:  {
			bool first = true;
			for (const auto elem : vd::range(store, index)) {
				if (!std::exchange(first, false)) {
					str.push_back('*');
				}
				to_string(store, elem, str, own_precedence);
			}
		} break;        
		case Type::known_function: {
			const KnownFunction& known_function = store.at(index).known_function;
			str.append(fn::name_of(known_function.type));
			str.push_back('(');
			bool first = true;
			for (const auto param : fn::range(known_function)) {
				if (!std::exchange(first, false)) {
					str.push_back(',');
				}
				to_string(store, param, str, own_precedence);
			}
			str.push_back(')');
		} break;
		case Type::unknown_function: {
			const UnknownFunction& unknown_function = store.at(index).unknown_function;
			fn::append_name(store, unknown_function, str);
			str.push_back('(');
			bool first = true;
			for (const auto param : fn::range(store, unknown_function)) {
				if (!std::exchange(first, false)) {
					str.push_back(',');
				}
				to_string(store, param, str, own_precedence);
			}
			str.push_back(')');
		} break;         
		case Type::variable: {
			const Variable& variable = store.at(index).variable;
			read<ToConstString>(store, index, str);
		} break;
		case Type::complex: {
			const Complex& complex = store.at(index).complex;
			print::append_complex(complex, str, parent_precedence, false);
		} break;
		default: assert(false); //if this assert hits, the switch above needs more cases.
		}

		if (own_precedence < parent_precedence && print::needs_parentheses(ref.get_type())) {
			str.push_back(')');
		}
	} //to_string

	void to_memory_layout(const Store& store, TypedIdx ref, std::vector<std::string>& content)
	{
		const std::uint32_t index = ref.get_index();
		std::string& current_str = content[index];
		switch (ref.get_type()) {
		case Type::sum: {
			current_str.append("sum      : {");
			bool first = true;
			for (const auto elem : vd::range(store, index)) {
				if (!std::exchange(first, false)) {
					current_str.append(", ");
				}
				current_str.append(std::to_string(elem.get_index()));
				to_memory_layout(store, elem, content);
			}
			current_str.push_back('}');
			const Sum* sum = &store.at(index).sum;
			while (sum->next_idx != Sum::null_index) {
				content[sum->next_idx].append("(sum node part of index " + std::to_string(index) + ')');
				sum = &store.at(sum->next_idx).sum;
			}
		} break;
		case Type::product:  {
			current_str.append("product  : {");
			bool first = true;
			for (const auto elem : vd::range(store, index)) {
				if (!std::exchange(first, false)) {
					current_str.append(", ");
				}
				current_str.append(std::to_string(elem.get_index()));
				to_memory_layout(store, elem, content);
			}
			current_str.push_back('}');
			const Product* product = &store.at(index).product;
			while (product->next_idx != Product::null_index) {
				content[product->next_idx].append("(product node part of index " + std::to_string(index) + ')');
				product = &store.at(product->next_idx).sum;
			}
		} break;             
		case Type::known_function: {
			const KnownFunction& known_function = store.at(index).known_function;
			current_str.append(fn::name_of(known_function.type));
			current_str.append(9 - fn::name_of(known_function.type).size(), ' ');
			current_str.append(": {");
			bool first = true;
			for (const auto param : fn::range(known_function)) {
				if (!std::exchange(first, false)) {
					current_str.append(", ");
				}
				current_str.append(std::to_string(param.get_index()));
				to_memory_layout(store, param, content);
			}
			current_str.push_back('}');
		} break;
		case Type::unknown_function: {
			const UnknownFunction& unknown_function = store.at(index).unknown_function;
			fn::append_name(store, unknown_function, current_str);
			current_str.append(": {");
			bool first = true;
			for (const auto param : fn::range(store, unknown_function)) {
				if (!std::exchange(first, false)) {
					current_str.append(", ");
				}
				current_str.append(std::to_string(param.get_index()));
				to_memory_layout(store, param, content);
			}
			current_str.push_back('}');
		} break;         
		case Type::variable: {
			current_str.append("variable : ");
			read<ToConstString>(store, index, current_str);
			const Variable* variable = &store.at(index).variable;
			while (variable->next_idx != TermString128::null_index) {
				std::size_t var_idx = variable->next_idx;
				content[var_idx].append("(string node part of index " + std::to_string(index) + ')');
				variable = &store.at(variable->next_idx).variable;
			}
		} break;   
		case Type::complex: {
			const Complex& complex = store.at(index).complex;
			current_str.append("value    : ");
			print::append_complex(complex, current_str, -1, false);
		} break;
		default: assert(false); //if this assert hits, the switch above needs more cases.
		}
	} //to_memory_layout

} //namespace bmath::intern::arithmetic

namespace bmath {
	std::string bmath::ArithmeticTerm::show_memory_layout(const bool show_first_table) const
	{
		std::vector<std::string> elements;
		elements.reserve(this->values.size() + 1);
		for (std::size_t i = 0; i < this->values.size(); i++) {
			elements.push_back("");
			if (i < 10) {
				elements[i] = " ";
			}
			elements[i].append(std::to_string(i));
			elements[i].append(" | ");
		}
		to_memory_layout(this->values, this->head, elements);
		elements.push_back("head at: " + std::to_string(this->head.get_index()));

		if (show_first_table) {
			const auto fst_table = this->values.first_table();
			elements.front().append("fst table: ");
			elements.front().append(fst_table.to_string());
			for (std::size_t i = 0; i < std::min(this->values.size(), std::size_t(128)); i++) {
				if (!fst_table.test(i)) {
					elements[i].append("-----free slot-----");
				}
			}
		}

		for (auto& elem : elements) {
			elem.push_back('\n');
		}
		std::string result;
		result.reserve(this->values.size() * 15);
		for (auto& elem : elements) {
			result.append(elem);
		}
		return result;
	}
} //namespace bmath