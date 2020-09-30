
#include <cassert>
#include <sstream>
#include <array>
#include <algorithm>

#include "termUtility.hpp"
#include "arithmeticTerm.hpp"
#include "termColony.hpp"
#include "parseArithmetic.hpp"

/*
	void prototype(const Store & store, const TypedIdx ref)
	{
		const auto [index, type] = ref.split();
		switch (type) {
		case Type::sum: {
			for (const auto elem : vdc::range(store, index)) {
			}
			assert(false);
		} break;
		case Type::product:  {
			for (const auto elem : vdc::range(store, index)) {
			}
			assert(false);
		} break;            
		case Type::known_function: {
			const KnownFunction& known_function = store.at(index).known_function;
			for (const auto param : fn::range(known_function)) {
			}
			assert(false);
		} break;
		case Type::generic_function: {
			const GenericFunction& generic_function = store.at(index).generic_function;
			for (const auto param : fn::range(store, generic_function)) {
			}
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

namespace bmath::in::arm {

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

		Complex eval(FnType type, const std::array<Complex, 3>& params)
		{
			switch (type) {
			case FnType::asinh: return std::asinh(params[0]);
			case FnType::acosh: return std::acosh(params[0]);
			case FnType::atanh: return std::atanh(params[0]);
			case FnType::asin : return std::asin (params[0]);
			case FnType::acos : return std::acos (params[0]);
			case FnType::atan : return std::atan (params[0]);
			case FnType::sinh : return std::sinh (params[0]);
			case FnType::cosh : return std::cosh (params[0]);
			case FnType::tanh : return std::tanh (params[0]);
			case FnType::sqrt : return std::sqrt (params[0]);
			case FnType::pow  : return std::pow  (params[0], params[1]);
			case FnType::log  : return std::log  (params[1]) / std::log(params[0]); //https://en.wikipedia.org/wiki/Complex_logarithm#Generalizations
			case FnType::exp  : return std::exp  (params[0]);
			case FnType::sin  : return std::sin  (params[0]);
			case FnType::cos  : return std::cos  (params[0]);
			case FnType::tan  : return std::tan  (params[0]);
			case FnType::abs  : return std::abs  (params[0]);
			case FnType::arg  : return std::arg  (params[0]);
			case FnType::ln   : return std::log  (params[0]);
			case FnType::re   : return std::real (params[0]);
			case FnType::im   : return std::imag (params[0]);
			default: assert(false);
				return Complex(0.0, 0.0);
			}
		} //eval

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
			if (name.find_first_not_of("0123456789", std::strlen("log")) == TokenView::npos && 
				name.starts_with("log")) [[unlikely]] 
			{
				return FnType::_logn;
			}
			return search(type_table, name, FnType::UNKNOWN);
		} //type_of

	} //namespace fn

	void free_tree(Store& store, const TypedIdx ref)
	{
		const auto [index, type] = ref.split();
		switch (type) {
		case Type::sum: {
			for (const auto elem : vdc::range(store, index)) {
				free_tree(store, elem);
			}
			free_slc<ToSum>(store, index);
		} break;
		case Type::product:  {
			for (const auto elem : vdc::range(store, index)) {
				free_tree(store, elem);
			}
			free_slc<ToProduct>(store, index);
		} break;            
		case Type::known_function: {
			const KnownFunction& known_function = store.at(index).known_function;
			for (const auto param : fn::range(known_function)) {
				free_tree(store, param);
			}
			store.free(index);
		} break;
		case Type::generic_function: {
			const GenericFunction& generic_function = store.at(index).generic_function;
			for (const auto param : fn::range(store, generic_function)) {
				free_tree(store, param);
			}
			free_slc<ToIndexSLC>(store, generic_function.params_idx);
			if (generic_function.name_size == GenericFunction::NameSize::longer) {
				free_slc<ToString>(store, generic_function.long_name_idx);
			}
			store.free(index);
		} break;             
		case Type::variable: {
			free_slc<ToString>(store, index);
		} break;   
		case Type::complex: {
			store.free(index);
		} break;
		default: assert(false); //if this assert hits, the switch above needs more cases.
		}
	}

	Complex eval_tree(const Store& store, const TypedIdx ref)
	{
		const auto [index, type] = ref.split();
		switch (type) {
		case Type::sum: {
			Complex value = 0.0;
			for (const auto elem : vdc::range(store, index)) {
				value += eval_tree(store, elem);
			}
			return value;
		} break;
		case Type::product:  {
			Complex value = 1.0;
			for (const auto elem : vdc::range(store, index)) {
				value *= eval_tree(store, elem);
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
			throw std::exception("eval_tree found variable in term");
		} break;   
		case Type::complex: {
			return store.at(index).complex;
		} break;
		default: assert(false); //if this assert hits, the switch above needs more cases.
		}
		return Complex(0.0, 0.0);
	} //eval_tree

	void append_to_string(const Store& store, const TypedIdx ref, std::string& str, const int parent_precedence)
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
				append_to_string(store, elem, str, own_precedence);
			}
		} break;
		case Type::product:  {
			bool first = true;
			for (const auto elem : vdc::range(store, index)) {
				if (!std::exchange(first, false)) {
					str.push_back('*');
				}
				append_to_string(store, elem, str, own_precedence);
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
				append_to_string(store, param, str, own_precedence);
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
				append_to_string(store, param, str, own_precedence);
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
	} //append_to_string

	void to_memory_layout(const Store& store, const TypedIdx ref, std::vector<std::string>& content)
	{
		const auto [index, type] = ref.split();

		auto show_typedidx_col_nodes = [&store, &content, index](std::uint32_t idx, bool show_first) {
			const TypedIdxColony* col = &store.at(idx).index_slc;
			if (show_first) {
				content[idx].append("(SLC node part of index " + std::to_string(index) + ')');
			}
			while (col->next_idx != TypedIdxColony::null_index) {
				content[col->next_idx].append("(SLC node part of index " + std::to_string(index) + ')');
				col = &store.at(col->next_idx).index_slc;
			}
		};
		auto show_string_nodes = [&store, &content, index](std::uint32_t idx, bool show_first) {
			const TermString128* str = &store.at(idx).string;
			if (show_first) {
				content[idx].append("(str node part of index " + std::to_string(index) + ": \"" 
					+ std::string(str->values, TermString128::array_size) + "\")");
			}
			while (str->next_idx != TermString128::null_index) {
				const std::size_t str_idx = str->next_idx;
				str = &store.at(str->next_idx).string;
				content[str_idx].append("(str node part of index " + std::to_string(index) + ": \"" 
					+ std::string(str->values, TermString128::array_size) + "\")");
			}
		};

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
			show_typedidx_col_nodes(index, false);
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
			show_typedidx_col_nodes(index, false);
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
			show_typedidx_col_nodes(generic_function.params_idx, true);
			if (generic_function.name_size == GenericFunction::NameSize::longer) {
				show_string_nodes(generic_function.long_name_idx, true);
			}
		} break;         
		case Type::variable: {
			current_str.append("variable : ");
			read<ToConstString>(store, index, current_str);
			show_string_nodes(index, false);
		} break;   
		case Type::complex: {
			const Complex& complex = store.at(index).complex;
			current_str.append("value    : <");
			print::append_complex(complex, current_str, -1, false);
			current_str.push_back('>');
		} break;
		default: assert(false); //if this assert hits, the switch above needs more cases.
		}
	} //to_memory_layout

	void flatten_variadic(Store& store, const TypedIdx ref)
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
					flatten_variadic(store, elem);
				}
			}
		} break;
		case Type::known_function: {
			const KnownFunction& known_function = store.at(index).known_function;
			for (const auto param : fn::range(known_function)) {
				flatten_variadic(store, param);
			}
		} break;
		case Type::generic_function: {
			const GenericFunction& generic_function = store.at(index).generic_function;
			for (const auto param : fn::range(store, generic_function)) {
				flatten_variadic(store, param);
			}
		} break;          
		case Type::variable: 
			break;
		case Type::complex: 
			break;
		default: assert(false); //if this assert hits, the switch above needs more cases.
		}
	} //flatten_variadic

	std::optional<Complex> combine_values_unexact(Store& store, const TypedIdx ref)
	{
		const auto [index, type] = ref.split();
		switch (type) {
		case Type::sum: {
			Complex result_val = 0.0;
			bool only_values = true;
			for (auto& elem : vdc::range(store, index)) {
				if (const auto elem_res = combine_values_unexact(store, elem)) {
					result_val += *elem_res;
					elem = Sum::null_value;
				}
				else {
					only_values = false;
				}
			}
			if (only_values) {
				free_slc<ToSum>(store, index); //all summands have already been freed in the loop above
				return { result_val };
			}
			else if (result_val != 0.0) {
				const auto new_summand = TypedIdx(store.insert(result_val), Type::complex);
				insert_new<ToSum>(store, index, new_summand);
			}
			return {};
		} break;
		case Type::product:  {
			Complex result_val = 1.0;
			bool only_values = true;
			for (auto& elem : vdc::range(store, index)) {
				if (const auto elem_res = combine_values_unexact(store, elem)) {
					result_val *= *elem_res;
					elem = Product::null_value;
				}
				else {
					only_values = false;
				}
			}
			if (only_values) {
				free_slc<ToProduct>(store, index); //all factors have already been freed in the loop above
				return { result_val };
			}
			else if (result_val != 1.0) {
				const auto new_factor = TypedIdx(store.insert(result_val), Type::complex);
				insert_new<ToProduct>(store, index, new_factor);
			}
			return {};
		} break;             
		case Type::known_function: {
			KnownFunction& known_function = store.at(index).known_function;
			std::array<Complex, 3> results_values;
			std::bitset<3> results_computable = 0;
			for (std::size_t i = 0; i < fn::param_count(known_function.type); i++) {
				if (const auto param_res = combine_values_unexact(store, known_function.params[i])) {
					results_values[i] = *param_res;
					results_computable.set(i);
				}
			}
			if (results_computable.count() == fn::param_count(known_function.type)) {
				const FnType type = known_function.type;
				store.free(index);
				return { fn::eval(type, results_values) };
			}
			else {
				for (std::size_t i = 0; i < fn::param_count(known_function.type); i++) {
					if (results_computable.test(i)) {
						known_function.params[i] = TypedIdx(store.insert(results_values[i]), Type::complex);
					}
				}
				return {};
			}
		} break;
		case Type::generic_function: {
			return {};
		} break;          
		case Type::variable: {
			return {};
		} break;   
		case Type::complex: {
			const Complex value = store.at(index).complex;
			store.free(index);
			return { value };
		} break;
		default: assert(false); //if this assert hits, the switch above needs more cases.
			return {};
		}
	}

} //namespace bmath::in::arm

namespace bmath {

	void ArithmeticTerm::flatten_variadic() noexcept
	{
		in::arm::flatten_variadic(this->store, this->head);
	}

	void ArithmeticTerm::combine_values_unexact() noexcept
	{
		if (const auto val = in::arm::combine_values_unexact(this->store, this->head)) {
			this->head = in::arm::TypedIdx(this->store.insert(*val), in::arm::Type::complex);
		}	
	}

	ArithmeticTerm::ArithmeticTerm(std::string name)
		:store(name.size() / 2)
	{
		auto parse_string = in::ParseString(std::move(name));
		parse_string.allow_implicit_product();
		parse_string.remove_space();
		const std::size_t error_pos = in::arm::find_first_not_arithmetic(in::TokenView(parse_string.tokens));
		in::throw_if<ParseFailure>(error_pos != in::TokenView::npos, error_pos, ParseFailure::What::illegal_char);
		this->head = in::arm::build(this->store, parse_string);
	} //ArithmeticTerm

	std::string bmath::ArithmeticTerm::show_memory_layout() const
	{
		std::vector<std::string> elements;
		elements.reserve(this->store.size() + 1);
		for (std::size_t i = 0; i < this->store.size(); i++) {
			elements.push_back("");
			if (i < 10) { //please std::format, i need you :(
				elements[i] = " ";
			}
			elements[i].append(std::to_string(i));
			elements[i].append(" | ");
		}
		to_memory_layout(this->store, this->head, elements);

		for (const auto i : this->store.free_slots()) {
			elements[i].append("-----free slot-----");
		}		

		for (auto& elem : elements) {
			elem.push_back('\n');
		}
		std::string result("   | head at index: " + std::to_string(this->head.get_index()) + '\n');
		result.reserve(this->store.size() * 15);
		for (auto& elem : elements) {
			result.append(elem);
		}
		return result;
	} //show_memory_layout

} //namespace bmath