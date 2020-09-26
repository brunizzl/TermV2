
#include <cassert>
#include <sstream>
#include <array>

#include "termUtility.hpp"
#include "arithmeticTerm.hpp"

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

	std::complex<double> eval(const ArithmeticStore& store, TypedIdx ref)
	{
		const std::uint32_t index = ref.get_index();
		switch (ref.get_type()) {
		case Type::sum: {
			std::complex<double> value = 0.0;
			for (const auto elem : range<ToConstSum>(store, index)) {
				value += eval(store, elem);
			}
			return value;
		} break;
		case Type::product:  {
			std::complex<double> value = 1.0;
			for (const auto elem : range<ToConstProduct>(store, index)) {
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

	void to_string(const ArithmeticStore& store, TypedIdx ref, std::string& str, const int parent_precedence)
	{
		const int own_precedence = print::operator_precedence(ref.get_type());
		if (own_precedence < parent_precedence) {
			str.push_back('(');
		}

		const std::uint32_t index = ref.get_index();
		switch (ref.get_type()) {
		case Type::sum: {
			bool first = true;
			for (const auto elem : range<ToConstSum>(store, index)) {
				if (!std::exchange(first, false)) {
					str.push_back('+');
				}
				to_string(store, elem, str, own_precedence);
			}
		} break;
		case Type::product:  {
			bool first = true;
			for (const auto elem : range<ToConstProduct>(store, index)) {
				if (!std::exchange(first, false)) {
					str.push_back('*');
				}
				to_string(store, elem, str, own_precedence);
			}
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
			read<ToConstString>(store, index, str);
		} break;
		case Type::complex: {
			const Complex& complex = store.at(index).complex;
			print::append_complex(complex, str, parent_precedence, false);
		} break;
		default: assert(false); //if this assert hits, the switch above needs more cases.
		}

		if (own_precedence < parent_precedence) {
			str.push_back(')');
		}
	} //to_string

	void to_debug_view_(const ArithmeticStore& store, TypedIdx ref, std::vector<std::string>& content)
	{
		const std::uint32_t index = ref.get_index();
		std::string& current_str = content[index];
		switch (ref.get_type()) {
		case Type::sum: {
			current_str.append("     sum: {");
			bool first = true;
			for (const auto elem : range<ToConstSum>(store, index)) {
				if (!std::exchange(first, false)) {
					current_str.append(", ");
				}
				current_str.append(std::to_string(elem.get_index()));
				to_debug_view_(store, elem, content);
			}
			current_str.push_back('}');
		} break;
		case Type::product:  {
			current_str.append(" product: {");
			bool first = true;
			for (const auto elem : range<ToConstProduct>(store, index)) {
				if (!std::exchange(first, false)) {
					current_str.append(", ");
				}
				current_str.append(std::to_string(elem.get_index()));
				to_debug_view_(store, elem, content);
			}
			current_str.push_back('}');
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
			current_str.append("variable:  ");
			read<ToConstString>(store, index, current_str);
			const Variable* variable = &store.at(index).variable;
			while (variable->next_idx != TermString128::null_index) {
				std::size_t var_idx = variable->next_idx;
				content[var_idx].append("string node part of index " + std::to_string(index) + ": ");
				variable = &store.at(variable->next_idx).variable;
				content[var_idx].append(std::string(variable->values, variable->values + TermString128::array_size));
			}
		} break;   
		case Type::complex: {
			const Complex& complex = store.at(index).complex;
			current_str.append("   value:  ");
			print::append_complex(complex, current_str, -1, false);
		} break;
		default: assert(false); //if this assert hits, the switch above needs more cases.
		}
	}


} //namespace bmath::intern::arithmetic

namespace bmath {
	std::string bmath::ArithmeticTerm::to_debug_view() const
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
		to_debug_view_(this->values, this->head, elements);
		elements.push_back("head at: " + std::to_string(this->head.get_index()));
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