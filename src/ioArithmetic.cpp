
#include <charconv>
#include <algorithm>

#include "ioArithmetic.hpp"
#include "termUtility.hpp"

namespace bmath::intern::arithmetic {


	std::size_t find_first_not_arithmetic(const TokenView view)
	{
		using namespace token;
		const char allowed_tokens[] = { character, number, open_grouping, clse_grouping, 
			unary_minus, sum, product, ',', '^', '\0' }; //'\0' only as end symbol for allowed_tokens
		return view.find_first_not_of(allowed_tokens);
	}

	Head find_head_type(const TokenView token_view, std::size_t offset)
	{
		std::size_t op;
		if ((op = find_first_of_skip_pars(token_view, token::sum)) != TokenView::npos) {
			return Head{ op, Head::Type::sum };
		}
		if (token_view.front() == token::unary_minus) {
			return Head{ 0, Head::Type::negate };
		}
		if ((op = find_first_of_skip_pars(token_view, token::product)) != TokenView::npos) {
			return Head{ op, Head::Type::product };
		}
		if ((op = find_first_of_skip_pars(token_view, '^')) != TokenView::npos) {
			return Head{ op, Head::Type::power };
		}
		if (token_view.find_first_not_of(token::number) == TokenView::npos) {
			return Head{ 0, Head::Type::value };
		}
		if ((op = token_view.find_first_not_of(token::character)) == TokenView::npos) {
			return Head{ 0, Head::Type::variable };
		}
		throw_if<ParseFailure>(token_view[op] != '(', op + offset, ParseFailure::What::illegal_char);
		throw_if<ParseFailure>(!token_view.ends_with(')'), token_view.length() + offset, ParseFailure::What::poor_grouping);
		if (op == 0) {
			return Head{ 0, Head::Type::group };
		}
		else {
			return Head{ op, Head::Type::function };
		}
	} //find_head_type

	TypedIdx build_value(Store& store, double re, double im)
	{
		return TypedIdx(store.insert(Complex{ std::complex<double>(re, im) }), Type::complex);
	}

	TypedIdx build(Store& store, ParseView input)
	{
		throw_if<ParseFailure>(input.size() == 0, input.offset, ParseFailure::What::illegal_ops);
		Head head = find_head_type(input.tokens, input.offset);
		while (head.type == Head::Type::group) {
			input.remove_prefix(1);
			input.remove_suffix(1);
			head = find_head_type(input.tokens, input.offset);
		}
		switch (head.type) {
		case Head::Type::sum: {
			return build_variadic<vdc::SumTraits, TypedIdx>(store, input, head.where, 
				[](Store& store, TypedIdx to_invert) {
					const TypedIdx minus_1 = build_value(store, -1.0);
					return TypedIdx(store.insert(Product({ minus_1, to_invert })), Type::product);
				},
				build);
		} break;
		case Head::Type::negate: {
			input.remove_prefix(1);  //remove minus sign
			const TypedIdx to_negate = build(store, input);
			const TypedIdx minus_1 = build_value(store, -1.0);
			return TypedIdx(store.insert(Product({ to_negate, minus_1 })), Type::product);
		} break;
		case Head::Type::product: {
			return build_variadic<vdc::ProductTraits, TypedIdx>(store, input, head.where, 
				[](Store& store, TypedIdx to_invert) {
					const TypedIdx minus_1 = build_value(store, -1.0);
					return TypedIdx(store.insert(
						KnownFunction{ FnType::pow, to_invert, minus_1, TypedIdx() }), Type::known_function);
				},
				build);
		} break;
		case Head::Type::power: {
			const auto base_view = input.steal_prefix(head.where);
			input.remove_prefix(1);
			const TypedIdx base = build(store, base_view);
			const TypedIdx expo = build(store, input);
			return TypedIdx(store.insert(KnownFunction{ FnType::pow, base, expo, TypedIdx() }), Type::known_function);
		} break;
		case Head::Type::value: {
			double val;
			const auto [ptr, error] = std::from_chars(input.chars, input.chars + input.size(), val);
			throw_if<ParseFailure>(error == std::errc::invalid_argument, input.offset, ParseFailure::What::illformed_val);
			throw_if<ParseFailure>(ptr != input.chars + input.size(), std::size_t(input.offset + ptr - input.chars + 1), ParseFailure::What::illformed_val);
			return build_value(store, val);
		} break;
		case Head::Type::function: {
			return build_function(store, input, head.where);
		} break;
		case Head::Type::variable: {
			if (input.chars[0] == 'i' && input.size() == 1) {
				return build_value(store, 0.0, 1.0);
			}
			else {
				return TypedIdx(insert_string(store, input.to_string_view()), Type::variable);
			}
		} break;
		default: 
			assert(false); 
			return TypedIdx();
		}
	} //build

	template<typename VariadicTraits, typename TypedIdx_T, typename TermStore_T, typename BuildInverse, typename BuildAny>
	TypedIdx_T build_variadic(TermStore_T& store, ParseView input, std::size_t op_idx, BuildInverse build_inverse, BuildAny build_any)
	{
		using Result_T = VariadicTraits::Object_T;

		const auto subterm_view = input.steal_prefix(op_idx);
		const TypedIdx_T subterm = build_any(store, subterm_view);
		const std::size_t variadic_idx = store.insert(Result_T(subterm));
		std::size_t last_node_idx = variadic_idx;
		while (input.size()) {
			const char current_operator = input.chars[0];
			input.remove_prefix(1); //remove current_operator;
			op_idx = find_first_of_skip_pars(input.tokens, VariadicTraits::operator_token);
			const auto subterm_view = input.steal_prefix(op_idx);
			const TypedIdx_T subterm = build_any(store, subterm_view);
			switch (current_operator) {
			case VariadicTraits::operator_char:
				last_node_idx = Result_T::insert_new(store, last_node_idx, subterm);
				break;
			case VariadicTraits::inverse_operator_char:
				last_node_idx = Result_T::insert_new(store, last_node_idx, build_inverse(store, subterm));
				break;
			default: assert(false);
			}
		}
		return TypedIdx_T(variadic_idx, VariadicTraits::type_name);
	} //build_variadic

	TypedIdx build_function(Store& store, ParseView input, const std::size_t open_par)
	{
		const auto type = fn::type_of(input.substr(0, open_par));
		if (type == FnType::UNKNOWN) { //build generic function
			GenericFunction result;
			{//writing name in result
				const auto name = std::string_view(input.chars, open_par);
				if (name.size() > GenericFunction::short_name_max) [[unlikely]] {
					result.name_size = GenericFunction::NameSize::longer;
					result.long_name_idx = insert_string(store, name);
				}
				else {
					result.name_size = GenericFunction::NameSize::small;
					for (std::size_t i = 0; i < name.size(); i++) {
						result.short_name[i] = name[i]; //maybe go over bound of short_name and into short_name_extension (undefined behavior oh wee!)
					}
				}
			}

			//writing parameters in result
			input.remove_suffix(1);            //"pow(2,4)" -> "pow(2,4"
			input.remove_prefix(open_par + 1); //"pow(2,4" ->      "2,4"

			{
				const std::size_t comma = find_first_of_skip_pars(input.tokens, ',');
				const auto param_view = input.steal_prefix(comma); //now input starts with comma
				const TypedIdx param = build(store, param_view);
				result.params_idx = store.insert(TypedIdxColony(param));
			}
			std::size_t last_node_idx = result.params_idx;
			while (input.size()) {
				input.remove_prefix(1); //erase comma
				const std::size_t comma = find_first_of_skip_pars(input.tokens, ',');
				const auto param_view = input.steal_prefix(comma);
				const TypedIdx param = build(store, param_view);
				last_node_idx = TypedIdxColony::insert_new(store, last_node_idx, param);
			}

			return TypedIdx(store.insert(result), Type::generic_function);
		}
		//known function, but with extra syntax (as "logn(...)" is allowed, where n is any natural number)
		else if (type == fn::SpecialParseSyntax::logn) { 
			double base_val;
			const auto [ptr, error] = std::from_chars(input.chars + std::strlen("log"), input.chars + open_par, base_val);
			throw_if<ParseFailure>(error == std::errc::invalid_argument, input.offset + std::strlen("log"), ParseFailure::What::illformed_val);
			throw_if<ParseFailure>(ptr != input.chars + open_par, input.offset + std::strlen("log"), ParseFailure::What::illformed_val);
			const TypedIdx base = build_value(store, base_val);
			input.remove_suffix(1u);
			input.remove_prefix(open_par + 1u);
			const TypedIdx expo = build(store, input);
			return TypedIdx(store.insert(KnownFunction{ FnType::log, base, expo, TypedIdx() }), Type::known_function);
		}
		else { //generic known function
			KnownFunction result{ FnType(type), TypedIdx(), TypedIdx(), TypedIdx() };
			input.remove_suffix(1u);
			input.remove_prefix(open_par + 1u);	//only arguments are left
			std::size_t comma = find_first_of_skip_pars(input.tokens, ',');
			auto param_view = input.steal_prefix(comma);
			for (auto& param : fn::range(result)) {
				throw_if<ParseFailure>(param_view.size() == 0u, input.offset, ParseFailure::What::wrong_param_count);
				param = build(store, param_view);
				comma = find_first_of_skip_pars(input.tokens, ',');
				param_view = input.steal_prefix(comma);
			}
			throw_if<ParseFailure>(param_view.size() > 0u, input.offset, ParseFailure::What::wrong_param_count);
			return TypedIdx(store.insert(result), Type::known_function);
		}
			
	} //build_function

	namespace print {

		void append_complex(const std::complex<double> val, std::string& dest, int parent_operator_precedence)
		{
			enum class Flag { showpos, noshowpos };
			const auto add_im_to_stream = [](std::stringstream& buffer, const double im, Flag flag) {
				if (im == -1) {
					buffer << '-';
				}
				else if (im == 1) {
					if (flag == Flag::showpos) {
						buffer << '+';
					}
				}
				else {
					buffer << (flag == Flag::showpos ? std::showpos : std::noshowpos) << im;
				}
				buffer << 'i';
			};

			bool parentheses = false;
			std::stringstream buffer;

			if (val.real() != 0 && val.imag() != 0) {
				parentheses = parent_operator_precedence > infixr(Type::sum);
				buffer << val.real();
				add_im_to_stream(buffer, val.imag(), Flag::showpos);		
			}
			else if (val.real() != 0 && val.imag() == 0) {
				parentheses = val.real() < 0 && parent_operator_precedence > infixr(Type::sum);	//leading '-'
				buffer << val.real();
			}
			else if (val.real() == 0 && val.imag() != 0) {
				parentheses = val.imag() < 0 && parent_operator_precedence > infixr(Type::sum);	//leading '-'	
				parentheses |= parent_operator_precedence > infixr(Type::product);	//*i
				add_im_to_stream(buffer, val.imag(), Flag::noshowpos);
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

		void append_real(double val, std::string& dest)
		{
			std::stringstream buffer;
			buffer << val;
			dest.append(buffer.str());
		}

		std::optional<double> get_negative_real (const Store& store, const TypedIdx ref) {
			const auto [index, type] = ref.split();
			if (type == Type::complex) {
				const Complex& complex = store.at(index).complex;
				if (complex.real() < 0.0 && complex.imag() == 0.0) {
					return { complex.real() };
				}
			}
			return {};
		} //get_negative_real

		std::optional<TypedIdx> get_pow_neg1(const Store& store, const TypedIdx ref)
		{
			const auto [index, type] = ref.split();
			if (type == Type::known_function) {
				const KnownFunction& function = store.at(index).known_function;
				if (function.type == FnType::pow) {
					if (const auto expo = get_negative_real(store, function.params[1])) {
						if (expo == -1.0) {
							return { function.params[0] };
						}
					}
				}
			}
			return {};
		} //get_pow_neg1

		std::optional<GetNegativeProductResult> get_negative_product(const Store& store, const TypedIdx ref)
		{
			const auto [index, type] = ref.split();
			if (type == Type::product) {
				std::vector<TypedIdx> other_factors;
				double negative_factor;
				bool found_negative_factor = false;
				for (const auto factor : vdc::range(store, index)) {
					if (!found_negative_factor) {
						if (const auto negative_val = get_negative_real(store, factor)) {
							negative_factor = *negative_val;
							found_negative_factor = true;
							continue;
						}
					}
					other_factors.push_back(factor);
				}
				if (found_negative_factor) {
					return { { negative_factor, other_factors} };
				}
			}
			return {};
		} //get_negative_product

		void append_to_string(const Store& store, const TypedIdx ref, std::string& str, const int parent_infixr)
		{
			const auto [index, type] = ref.split();
			const int own_infixr = infixr(type);
			if (own_infixr <= parent_infixr) {
				str.push_back('(');
			}

			switch (type) {
			case Type::sum: {
				bool first = true;
				for (const auto summand : vdc::range(store, index)) {
					if (!std::exchange(first, false)) {
						str.push_back('+');
					}
					append_to_string(store, summand, str, own_infixr);
				}
			} break;
			case Type::product: {
				bool first = true;
				for (const auto factor : vdc::range(store, index)) {
					if (!std::exchange(first, false)) {
						str.push_back('*');
					}
					append_to_string(store, factor, str, own_infixr);
				}
			} break;
			case Type::known_function: {
				const KnownFunction& known_function = store.at(index).known_function;
				str.pop_back(); //pop open paren
				str.append(fn::name_of(known_function.type));
				str.push_back('(');
				bool first = true;
				for (const auto param : fn::range(known_function)) {
					if (!std::exchange(first, false)) {
						str.push_back(',');
					}
					append_to_string(store, param, str, own_infixr);
				}
			} break;
			case Type::generic_function: {
				const GenericFunction& generic_function = store.at(index).generic_function;
				str.pop_back(); //pop open paren
				fn::append_name(store, generic_function, str);
				str.push_back('(');
				bool first = true;
				for (const auto param : fn::range(store, generic_function)) {
					if (!std::exchange(first, false)) {
						str.push_back(',');
					}
					append_to_string(store, param, str, own_infixr);
				}
			} break;
			case Type::variable: {
				const Variable& variable = store.at(index).variable;
				read(store, index, str);
			} break;
			case Type::complex: {
				const Complex& complex = store.at(index).complex;
				append_complex(complex, str, parent_infixr);
			} break;
			default: assert(false); //if this assert hits, the switch above needs more cases.
			}

			if (own_infixr <= parent_infixr) {
				str.push_back(')');
			}
		} //append_to_string

		std::string to_pretty_string(const Store& store, const TypedIdx ref, const int parent_infixr)
		{
			const auto reverse_elems = [](auto range) {
				std::vector<TypedIdx> result;
				for (const auto elem : range) {
					result.push_back(elem);
				}
				std::reverse(result.begin(), result.end());
				return result;
			};

			const auto [index, type] = ref.split();
			bool need_parentheses = infixr(type) <= parent_infixr;
			std::string str;

			switch (type) {
			case Type::sum: {
				bool first = true;
				for (const auto summand : reverse_elems(vdc::range(store, index))) {
					if (const auto product = get_negative_product(store, summand)) {
						append_real(product->negative_factor, str);
						str.push_back('*');
						for (const auto factor : product->other_factors) {
							str += to_pretty_string(store, factor, infixr(Type::product));
						}
					}
					else if (const auto val = get_negative_real(store, summand)) {
						append_real(*val, str);
					}
					else {
						if (!first) {
							str.push_back('+');
						}
						str += to_pretty_string(store, summand, infixr(type));
					}
					first = false;
				}
			} break;
			case Type::product: {
				bool first = true;
				for (const auto elem : reverse_elems(vdc::range(store, index))) {
					if (auto val = get_negative_real(store, elem)) {
						if (*val == -1.0 && first && Product::slow_size(store, index) > 1) {
							str.push_back('-');
							continue;
						}
					}
					if (const auto pow = get_pow_neg1(store, elem)) {
						str.append(first ? "1/" : "/");
						str += to_pretty_string(store, *pow, infixr(Type::product));
					}
					else {
						if (!first) {
							str.push_back('*');
						}
						str += to_pretty_string(store, elem, infixr(type));
					}
					first = false;
				}
			} break;
			case Type::known_function: {
				const KnownFunction& function = store.at(index).known_function;
				if (function.type == FnType::pow) {
					need_parentheses = infixr(PrintExtras::pow) <= parent_infixr;
					str += to_pretty_string(store, function.params[0], infixr(PrintExtras::pow));
					str.push_back('^');
					str += to_pretty_string(store, function.params[1], infixr(PrintExtras::pow));
				}
				else {
					need_parentheses = false;
					str.append(fn::name_of(function.type));
					str.push_back('(');
					bool first = true;
					for (const auto param : fn::range(function)) {
						if (!std::exchange(first, false)) {
							str.push_back(',');
						}
						str += to_pretty_string(store, param, infixr(type));
					}
					str.push_back(')');
				}
			} break;
			case Type::generic_function: {
				need_parentheses = false;
				const GenericFunction& generic_function = store.at(index).generic_function;
				fn::append_name(store, generic_function, str);
				str.push_back('(');
				bool first = true;
				for (const auto param : fn::range(store, generic_function)) {
					if (!std::exchange(first, false)) {
						str.push_back(',');
					}
					str += to_pretty_string(store, param, infixr(type));
				}
				str.push_back(')');
			} break;
			case Type::variable: {
				const Variable& variable = store.at(index).variable;
				read(store, index, str);
			} break;
			case Type::complex: {
				const Complex& complex = store.at(index).complex;
				append_complex(complex, str, parent_infixr);
			} break;
			default: assert(false); //if this assert hits, the switch above needs more cases.
			}

			if (need_parentheses) {
				return '(' + str + ')';
			}
			else {
				return str;
			}
		} //to_pretty_string

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
			case Type::product: {
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
				current_str.append("function : {");
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
				current_str.append("function?: {");
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
				show_string_nodes(index, false);
			} break;
			case Type::complex: {
				const Complex& complex = store.at(index).complex;
				current_str.append("value    : ");
			} break;
			default: assert(false); //if this assert hits, the switch above needs more cases.
			}

			//append name of subterm to line
			current_str.append(std::max(0, 35 - (int)current_str.size()), ' ');
			append_to_string(store, ref, current_str, 0);
		} //to_memory_layout

	} //namespace print

} //namespace bmath::intern::arithmetic