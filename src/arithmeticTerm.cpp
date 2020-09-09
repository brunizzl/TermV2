
#include <cassert>
#include <sstream>

#include "arithmeticTerm.hpp"

/*
	void pattern(const TermStore<TypesUnion> & store, TypedRef ref)
	{
		const std::size_t index = ref.get_index();
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
		case Type::power: {
			const Power& power = store.at(index).power;
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
		}
		assert(false);	//if this assert hits, the switch above needs more cases.
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
			case Type::power:				return 5;
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
				else if (im == 1 && showpos) {
					buffer << '+';
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

	}


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////eported in header////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	std::complex<double> eval(const TermStore<TypesUnion> & store, TypedRef ref)
	{
		const std::size_t index = ref.get_index();
		switch (ref.get_type()) {
		case Type::sum: {
			const Sum& sum = store.at(index).sum;
			std::complex<double> value = 0.0;
			for (const auto elem : range<ToConstSum>(store, sum)) {
				value += eval(store, elem);
			}
			return value;
		} break;
		case Type::product:  {
			const Product& product = store.at(index).product;
			std::complex<double> value = 1.0;
			for (const auto elem : range<ToConstProduct>(store, product)) {
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
		case Type::power: {
			const Power& power = store.at(index).power;
			const std::complex<double> base = eval(store, power.base);
			const std::complex<double> expo = eval(store, power.expo);
			return std::pow(base, expo);
		} break;            
		case Type::variable: {
			throw std::exception("eval found variable in term");
		} break;   
		case Type::complex: {
			const Complex& complex = store.at(index).complex;
			return complex;
		} break;
		}
		assert(false);	//if this assert hits, the switch above needs more cases.
		return std::complex<double>(0.0, 0.0);
	} //eval

	void to_string(const TermStore<TypesUnion>& store, TypedRef ref, std::string& str, const int parent_precedence)
	{
		const int own_precedence = print::operator_precedence(ref.get_type());
		if (own_precedence < parent_precedence) {
			str.push_back('(');
		}

		const std::size_t index = ref.get_index();
		switch (ref.get_type()) {
		case Type::sum: {
			const Sum& sum = store.at(index).sum;
			bool first = true;
			for (const auto elem : range<ToConstSum>(store, sum)) {
				if (!std::exchange(first, false)) {
					str.push_back('+');
				}
				to_string(store, elem, str, own_precedence);
			}
		} break;
		case Type::product:  {
			const Product& product = store.at(index).product;
			bool first = true;
			for (const auto elem : range<ToConstProduct>(store, product)) {
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
		case Type::power: {
			const Power& power = store.at(index).power;
			to_string(store, power.base, str, own_precedence);
			str.push_back('^');
			to_string(store, power.expo, str, own_precedence);
		} break;            
		case Type::variable: {
			const Variable& variable = store.at(index).variable;
			read<ToConstString>(store, index, str);
		} break;
		case Type::complex: {
			const Complex& complex = store.at(index).complex;
			print::append_complex(complex, str, parent_precedence, false);
		} break;
		}

		if (own_precedence < parent_precedence) {
			str.push_back(')');
		}
	} //to_string
} //namespace bmath::intern::arithmetic