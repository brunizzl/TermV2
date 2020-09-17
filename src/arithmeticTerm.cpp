
#include <cassert>
#include <sstream>
#include <array>

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

	} //namespace print

	namespace build {

		const char* const allowed_chars = "1234567890.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_+-*/^[]()!";	//reserving '#', '$', and braces for internal stuff
		const char* const letter_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"; //everything allowed to occur in names of variables / functions
		const char* const number_chars = "1234567890.";

		void preprocess_str(std::string& str)
		{
			int par_diff = 0;	//counts '(' and ')' (parentheses)
			int brac_diff = 0;	//counts '[' and ']' (brackets)
			std::size_t last_operator = std::string::npos - 1;	//remembers the position of last '+', '-', '*', '/', '^'  (-1 to allow operator at str[0])

			for (std::size_t i = 0; i < str.length(); i++) {	//deleting whitespace and counting parentheses
				switch (str[i]) {
				case '\t':
				case '\n':
				case ' ': 
					str.erase(i--, 1);	//erase this char -> set i one back, as string got shorter
					break;
				case '(': 
					par_diff++;
					break;
				case ')': 
					if (--par_diff < 0) {	//one can not have more closing, than opening parentheses at any point
						throw std::exception("the parentheses or brackets of string do not obey the syntax rules");
					}
					break;
				case '[': 
					brac_diff++;
					str[i] = '(';	//later functions expect only parentheses, not brackets
					break;
				case ']':
					if (--brac_diff < 0) {	//one can not have more closing, than opening brackets at any point
						throw std::exception("the parentheses or brackets of string do not obey the syntax rules");
					}
					str[i] = ')';	//later functions expect only parentheses, not brackets
					break;
				case '+':
				case '*':
				case '/':
				case '^': 
					if (i == 0 || i == str.length() - 1) {
						throw std::exception("found binary operator (+ * / ^) at beginning or end of string");
					}
					else if (str[i - 1] == '(' || str[i + 1] == ')') {
						throw std::exception("found binary operator (+ * / ^) next to enclosing bracket or parenthesis");
					}
					[[fallthrough]];
				case '-':
					if (last_operator == i - 1) {
						throw std::exception("found two operators (+ - * / ^) in direct succession");
					}
					last_operator = i;
					break;
				default:
					break; //nothing to do this character
				}
			}
			if (par_diff != 0 || brac_diff != 0) {
				throw std::exception("the parentheses or brackets of string do not obey the syntax rules");
			}
			if (str.find_first_not_of(allowed_chars) != std::string::npos) {
				throw std::exception("string contains characters other than: 1234567890.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_+-*/^[]()!");
			}
		} //preprocess_str

		//searches from clsd_par to front, as term is constructed from the right.
		std::size_t find_open_par(std::size_t clsd_par, const std::string_view name)
		{
			unsigned int deeper_clsd_par = 0;	//counts how many closed parentheses have been encountered minus how many open parentheses
			std::size_t nxt_par = clsd_par;
			while (true) {
				nxt_par = name.find_last_of("()", nxt_par - 1);
				assert(nxt_par != std::string::npos);	//function preprocess_str() already guarantees valid parentheses.

				switch (name[nxt_par]) {
				case ')':
					deeper_clsd_par++;
					break;
				case '(':
					if (deeper_clsd_par-- == 0) {
						return nxt_par;
					}
					break;
				}
			}
		} //find_open_par

		//searches from open_par to back
		std::size_t find_closed_par(std::size_t open_par, const std::string_view name)
		{
			unsigned int deeper_open_par = 0;	//counts how many opened parentheses have been encountered witch have not yet been closed
			std::size_t nxt_par = open_par;
			while (true) {
				nxt_par = name.find_first_of("()", nxt_par + 1);
				assert(nxt_par != std::string::npos);	//function preprocess_str() already guarantees valid parentheses.

				switch (name[nxt_par]) {
				case '(':
					deeper_open_par++;
					break;
				case ')':
					if (deeper_open_par-- == 0) {
						return nxt_par;
					}
					break;
				}
			}
		} //find_open_par


		std::size_t find_last_of_skip_pars(const std::string_view name, const char* const characters)
		{
			std::size_t open_par = name.length() - 1;
			std::size_t clsd_par = name.find_last_of(')');
			while (clsd_par != std::string::npos) {
				const std::string_view search_view(name.data() + clsd_par, open_par - clsd_par);
				const std::size_t found = search_view.find_last_of(characters);
				if (found != std::string::npos) {
					return found + clsd_par;	//search_view starts with offset of clsd_par (two lines above). this offset has to be added to get distance from begin of name
				} 
				else {
					open_par = find_open_par(clsd_par, name);
					clsd_par = name.find_last_of(')', open_par);	//one could start the search with an offset of one, only to have an underflow of open_par == 0
				}
			}
			return name.find_last_of(characters, open_par);
		} //find_last_of_skip_pars (c-string)

		std::size_t find_last_of_skip_pars(const std::string_view name, const char character)
		{
			std::size_t open_par = name.length() - 1;
			std::size_t clsd_par = name.find_last_of(')');
			while (clsd_par != std::string::npos) {
				const std::string_view search_view(name.data() + clsd_par, open_par - clsd_par);
				const std::size_t found = search_view.find_last_of(character);
				if (found != std::string::npos) {
					return found + clsd_par;	//search_view starts with offset of clsd_par (two lines above). this offset has to be added to get distance from begin of name
				}
				else {
					open_par = find_open_par(clsd_par, name);
					clsd_par = name.find_last_of(')', open_par);	//one could start the search with an offset of one, only to have an underflow of open_par == 0
				}
			}
			return name.find_last_of(character, open_par);
		} //find_last_of_skip_pars (single char)

		std::size_t find_first_of_skip_pars(const std::string_view name, const char character)
		{
			std::size_t open_par = name.find_first_of('(');
			std::size_t clsd_par = 0;
			while (open_par != std::string::npos) {
				const std::string_view search_view = name.substr(clsd_par + 1, open_par - clsd_par - 1); //only "...)here(..."
				const std::size_t found = search_view.find_first_of(character);
				if (found != std::string::npos) {
					return found + clsd_par;
				}
				else {
					clsd_par = find_closed_par(open_par, name);
					open_par = name.find_first_of('(', clsd_par);
				}
			}
			return name.find_first_of(character, clsd_par);
		} //find_first_of_skip_pars (single char)

		enum class HeadType
		{
			sum,
			product,
			power,
			variable,
			function,
			parentheses,
			value,
		};

		HeadType head_type(const std::string_view name, std::size_t& op)
		{
			op = find_last_of_skip_pars(name, "+-");
			if (op != std::string::npos) {
				return HeadType::sum;
			}
			op = find_last_of_skip_pars(name, "*/");
			if (op != std::string::npos) {
				return HeadType::product;
			}
			op = find_last_of_skip_pars(name, '^');
			if (op != std::string::npos) {
				return HeadType::power;
			}

			const std::size_t next_not_letter = name.find_first_not_of(letter_chars);
			if (next_not_letter == std::string::npos) {
				return HeadType::variable;
			}
			if (next_not_letter != 0 && name[next_not_letter] == '(') {
				assert(name.ends_with(')') && "expected function call to end with closed parenthesis");
				return HeadType::function;
			}
			if (name.starts_with('(') && name.ends_with(')')) {
				return HeadType::parentheses;
			}
			assert(name.find_first_not_of(number_chars) == std::string::npos);
			return HeadType::value;
		} //head_type

		//may reject exact computation, but will (almost) never accept inexact computtation
		//it is assumed, that integers occuring in terms are always small enough to fit inside
		//the mantissa of double.
		bool exactly_computable(const std::string_view name)
		{
			const char* const exact_int_arithmetic_1 = "()+*^123456789";
			const char* const exact_int_arithmetic_2 = "()+*-123456789";
			return name.find_first_not_of(exact_int_arithmetic_1) == std::string::npos ||
				name.find_first_not_of(exact_int_arithmetic_2) == std::string::npos;
		}



		TypedRef new_number(TermStore<TypesUnion>& store, double re, double im = 0.0)
		{
			return TypedRef(store.emplace_new(Complex{ std::complex<double>(re, im) }), Type::complex);
		}

		TypedRef new_arithmetic(TermStore<TypesUnion>& store, std::string_view name)
		{
			std::size_t op = std::string::npos;	//op standing for operator
			HeadType head = head_type(name, op);
			while (head == HeadType::parentheses) {
				name.remove_prefix(1);
				name.remove_suffix(1);
				HeadType head = head_type(name, op);
			}
			switch (head) {
			case HeadType::sum: {
				if (op == 0) {	//FALLUNTERSCHEIDUNG FUER MEHRERE ZEICHEN NUR BEGINNENDES MINUS MUSS HIER NOCH HIN
					assert(name[op] == '-' && "no plus may lead");
					name.remove_prefix(1);
					const Product name_times_minus_one(Product{ { new_number(store, -1.0), new_arithmetic(store, name) } });
					return TypedRef(store.emplace_new(name_times_minus_one), Type::product);
				}
				else {
					while (op != std::string::npos) {
						const std::string_view summand = name.substr(op + 1); //create substring of only summand, not op
						name.remove_suffix(name.length() - op);	//remove last summand und op
					}
				}
			} break;
			case HeadType::product: {

			} break;
			case HeadType::power: {

			} break;
			case HeadType::function: {

			} break;
			case HeadType::variable: {

			} break;
			case HeadType::value: {

			} break;

			}

			return TypedRef(0);
		}


	} //namespace build


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////exported in header///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	std::complex<double> eval(const TermStore<TypesUnion> & store, TypedRef ref)
	{
		const std::size_t index = ref.get_index();
		switch (ref.get_type()) {
		case Type::sum: {
			const Sum& sum = store.at(index).sum;
			std::complex<double> value = 0.0;
			for (const auto elem : range<ConstSummands>(store, sum.summands)) {
				value += eval(store, elem);
			}
			return value;
		} break;
		case Type::product:  {
			const Product& product = store.at(index).product;
			std::complex<double> value = 1.0;
			for (const auto elem : range<ConstFactors>(store, product.factors)) {
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
			for (const auto elem : range<ConstSummands>(store, sum.summands)) {
				if (!std::exchange(first, false)) {
					str.push_back('+');
				}
				to_string(store, elem, str, own_precedence);
			}
		} break;
		case Type::product:  {
			const Product& product = store.at(index).product;
			bool first = true;
			for (const auto elem : range<ConstFactors>(store, product.factors)) {
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