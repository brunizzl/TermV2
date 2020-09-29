
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
		const auto [index, type] = ref.split();
		switch (type) {
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
		case Type::generic_function: {
			const GenericFunction& generic_function = store.at(index).generic_function;
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
			case Type::generic_function:	return 1;	//lower order, because it already brings its own parentheses.
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
			case Type::generic_function:	return false;
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

		void append_name(const Store& store, const GenericFunction& func, std::string& str)
		{
			if (func.name_size == GenericFunction::NameSize::small) {
				str.append(func.short_name);
			}
			else {
				read<ToConstString>(store, func.long_name_idx, str);
			}
		} //append_name

		  //only expects actual name part of function, e.g. "asin", NOT "asin(...)"
		  //if name is one of FnType, that is returned, else FnType::UNKNOWN
		FnType type_of(const ParseView input) noexcept
		{
			const auto name = input.to_string_view();
			//special syntax allowed for log, to also accept "logn()" for any natural n
			if (input.tokens.find_first_not_of(std::strlen("log"), token::number) == TokenView::npos && 
				name.starts_with("log")) [[unlikely]] 
			{
				return FnType::log;
			}
			return search(type_table, name, FnType::UNKNOWN);
		} //type_of

	} //namespace fn

	std::complex<double> eval(const Store& store, TypedIdx ref)
	{
		const auto [index, type] = ref.split();
		switch (type) {
		case Type::sum: {
			std::complex<double> value = 0.0;
			for (const auto elem : vdc::range(store, index)) {
				value += eval(store, elem);
			}
			return value;
		} break;
		case Type::product:  {
			std::complex<double> value = 1.0;
			for (const auto elem : vdc::range(store, index)) {
				value *= eval(store, elem);
			}
			return value;
		} break;             
		case Type::known_function: {
			const KnownFunction& known_function = store.at(index).known_function;
			assert(false);
		} break;
		case Type::generic_function: {
			const GenericFunction& generic_function = store.at(index).generic_function;
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
		const auto [index, type] = ref.split();
		const int own_precedence = print::operator_precedence(type);
		const bool print_parentheses = own_precedence <= parent_precedence && print::needs_parentheses(type);
		if (print_parentheses) {
			str.push_back('(');
		}

		switch (type) {
		case Type::sum: {
			bool first = true;
			for (const auto elem : vdc::range(store, index)) {
				if (!std::exchange(first, false)) {
					str.push_back('+');
				}
				to_string(store, elem, str, own_precedence);
			}
		} break;
		case Type::product:  {
			bool first = true;
			for (const auto elem : vdc::range(store, index)) {
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
		case Type::generic_function: {
			const GenericFunction& generic_function = store.at(index).generic_function;
			fn::append_name(store, generic_function, str);
			str.push_back('(');
			bool first = true;
			for (const auto param : fn::range(store, generic_function)) {
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

		if (own_precedence <= parent_precedence && print::needs_parentheses(type)) {
			str.push_back(')');
		}
	} //to_string

	void to_memory_layout(const Store& store, TypedIdx ref, std::vector<std::string>& content)
	{
		const auto [index, type] = ref.split();
		std::string& current_str = content[index];
		switch (type) {
		case Type::sum: {
			current_str.append("sum      : {");
			bool first = true;
			for (const auto elem : vdc::range(store, index)) {
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
			for (const auto elem : vdc::range(store, index)) {
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
		case Type::generic_function: {
			const GenericFunction& generic_function = store.at(index).generic_function;
			fn::append_name(store, generic_function, current_str);
			current_str.append(": {");
			bool first = true;
			for (const auto param : fn::range(store, generic_function)) {
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
				variable = &store.at(variable->next_idx).variable;
				content[var_idx].append("(string node part of index " + std::to_string(index) + ": \"" + std::string(variable->values, Variable::array_size) + "\")");
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

	void combine_variadic(Store& store, TypedIdx ref)
	{
		const auto [index, type] = ref.split();
		switch (type) {
		case Type::sum: 
			[[fallthrough]];
		case Type::product: {
			std::size_t current_append_node = index;
			for (auto& elem : vdc::range(store, index)) {
				const auto [elem_idx, elem_type] = elem.split();
				if (elem_type == type) {
					elem = TypedIdxColony::null_value;
					current_append_node = append<ToIndexSLC>(store, current_append_node, elem_idx);
				}
				else {
					combine_variadic(store, elem);
				}
			}
		} break;
		case Type::known_function: {
			const KnownFunction& known_function = store.at(index).known_function;
			for (const auto param : fn::range(known_function)) {
				combine_variadic(store, param);
			}
		} break;
		case Type::generic_function: {
			const GenericFunction& generic_function = store.at(index).generic_function;
			for (const auto param : fn::range(store, generic_function)) {
				combine_variadic(store, param);
			}
		} break;          
		case Type::variable: 
			break;
		case Type::complex: 
			break;
		default: assert(false); //if this assert hits, the switch above needs more cases.
		}
	} //combine_variadic

} //namespace bmath::intern::arithmetic

namespace bmath {
	std::string bmath::ArithmeticTerm::show_memory_layout() const
	{
		std::vector<std::string> elements;
		elements.reserve(this->values.size() + 1);
		for (std::size_t i = 0; i < this->values.size(); i++) {
			elements.push_back("");
			if (i < 10) { //please std::format, i need you :(
				elements[i] = " ";
			}
			elements[i].append(std::to_string(i));
			elements[i].append(" | ");
		}
		to_memory_layout(this->values, this->head, elements);
		elements.push_back("head at index: " + std::to_string(this->head.get_index()));

		for (const auto i : this->values.free_slots()) {
			elements[i].append("-----free slot-----");
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