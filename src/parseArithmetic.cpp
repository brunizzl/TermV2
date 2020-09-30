
#include <charconv>
#include <algorithm>

#include "parseArithmetic.hpp"
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
			return build_variadic<vdc::SumTraits, ToSum, TypedIdx>(store, input, head.where, 
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
			return build_variadic<vdc::ProductTraits, ToProduct, TypedIdx>(store, input, head.where, 
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

	TypedIdx build_function(Store& store, ParseView input, const std::size_t open_par)
	{
		const FnType type = fn::type_of(input.substr(0, open_par));
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
				last_node_idx = insert_new<ToIndexSLC>(store, last_node_idx, param);
			}

			return TypedIdx(store.insert(result), Type::generic_function);
		}
		//known function, but with extra syntax (as "logn(...)" is allowed, where n is any natural number)
		else if (type == FnType::_logn) { 
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
			KnownFunction result{ type, TypedIdx(), TypedIdx(), TypedIdx() };
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

} //namespace bmath::intern::arithmetic