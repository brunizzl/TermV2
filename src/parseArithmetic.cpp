
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

	void allow_implicit_product(TokenString& tokens, std::string& name)
	{
		assert(tokens.length() == name.length());
		auto tokens_iter = tokens.begin();
		auto name_iter = name.begin();
		for (; std::next(tokens_iter) != tokens.end(); tokens_iter++, name_iter++) {
			if (*tokens_iter == token::number && *std::next(tokens_iter) == token::character) {
				tokens_iter = tokens.insert(std::next(tokens_iter), token::product);
				name_iter = name.insert(std::next(name_iter), '*');
			}
		}
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
			return build_variadic<SumTraits, ToSum, TypedIdx>(store, input, head.where, 
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
			return build_variadic<ProductTraits, ToProduct, TypedIdx>(store, input, head.where, 
				[](Store& store, TypedIdx to_invert) {
					const TypedIdx minus_1 = build_value(store, -1.0);
					return TypedIdx(store.insert(
						KnownFunction{ FunctionType::pow, to_invert, minus_1, TypedIdx() }), Type::known_function);
				},
				build);
		} break;
		case Head::Type::power: {
			const auto base_view = input.substr(0, head.where);
			input.remove_prefix(head.where + 1);
			const TypedIdx base = build(store, base_view);
			const TypedIdx expo = build(store, input);
			return TypedIdx(store.insert(KnownFunction{ FunctionType::pow, base, expo, TypedIdx() }), Type::known_function);
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
		const FunctionType type = function::type_of(input.substr(0, open_par));
		if (type == FunctionType::UNKNOWN) { //build unknown function
			UnknownFunction result;
			{//writing name in result
				const auto name = std::string_view(input.chars, open_par);
				if (name.size() > UnknownFunction::short_name_max) [[unlikely]] {
					result.name_size = UnknownFunction::NameSize::longer;
					result.name_idx = insert_string(store, name);
				}
				else {
					result.name_size = UnknownFunction::NameSize::small;
					for (std::size_t i = 0; i < name.size(); i++) {
						result.short_name[i] = name[i]; //maybe go over bound of short_name and into short_name_extension (undefined behavior oh wee!)
					}
				}
			}

			//writing parameters in result
			input.remove_suffix(1);            //"pow(2,4)" -> "pow(2,4"
			input.remove_prefix(open_par + 1); //"pow(2,4" ->      "2,4"
			if (input.size()) [[likely]] {
				const std::size_t nr_params = count_skip_pars(input.tokens, ',') + 1; //commas only seperate params -> one more param than commas
				switch (nr_params) {
				case 1: {
					result.param_count = UnknownFunction::ParamCount::one;
					result.short_params[0] = build(store, input);
				} break;
				case 2: {
					result.param_count = UnknownFunction::ParamCount::two;
					const std::size_t comma = find_first_of_skip_pars(input.tokens, ',');
					result.short_params[0] = build(store, input.substr(0, comma));
					result.short_params[1] = build(store, input.substr(comma + 1));
				} break;
				default: {
					result.param_count = UnknownFunction::ParamCount::more;
					{
						const std::size_t comma = find_first_of_skip_pars(input.tokens, ',');
						const auto first_param_view = input.substr(0, comma);
						const TypedIdx first_param = build(store, first_param_view);
						input.remove_prefix(comma + 1);
						result.params_idx = store.insert(TypedIdxColony(first_param));
					}
					while (input.size()) {
						const std::size_t comma = find_first_of_skip_pars(input.tokens, ',');
						const auto next_param_view = input.substr(0, comma);
						const TypedIdx nex_param = build(store, next_param_view);
						input.remove_prefix(comma == TokenView::npos ? input.size() : comma + 1);
						insert_new<ToIndexSLC>(store, result.params_idx, nex_param);
					}
				} break;
				}
			} //else no parameters to parse -> already done
			return TypedIdx(store.insert(result), Type::unknown_function);
		}
		//known function, but with extra syntax (as "logn(...)" is allowed, where n is any natural number)
		else if (type == FunctionType::log && open_par > std::strlen("log")) { 
			double base_val;
			const auto [ptr, error] = std::from_chars(input.chars + std::strlen("log"), input.chars + open_par, base_val);
			throw_if<ParseFailure>(error == std::errc::invalid_argument, input.offset + std::strlen("log"), ParseFailure::What::illformed_val);
			throw_if<ParseFailure>(ptr != input.chars + open_par, input.offset + std::strlen("log"), ParseFailure::What::illformed_val);
			const TypedIdx base = build_value(store, base_val);
			input.remove_suffix(1u);
			input.remove_prefix(open_par + 1u);
			const TypedIdx expo = build(store, input);
			return TypedIdx(store.insert(KnownFunction{ FunctionType::log, base, expo, TypedIdx() }), Type::known_function);
		}
		else { //generic known function
			KnownFunction result{ type, TypedIdx(), TypedIdx(), TypedIdx() };
			input.remove_suffix(1u);
			input.remove_prefix(open_par + 1u);	//only arguments are left
			std::size_t comma = find_first_of_skip_pars(input.tokens, ',');
			auto param_view = input.substr(0u, comma);
			input.remove_prefix(comma == TokenView::npos ? input.size() : comma + 1u);
			for (auto& param : function::range(result)) {
				throw_if<ParseFailure>(param_view.size() == 0u, input.offset, ParseFailure::What::wrong_param_count);
				param = build(store, param_view);
				comma = find_first_of_skip_pars(input.tokens, ',');
				param_view = input.substr(0u, comma);
				input.remove_prefix(comma == TokenView::npos ? input.size() : comma + 1u);
			}
			throw_if<ParseFailure>(param_view.size() > 0u, input.offset, ParseFailure::What::wrong_param_count);
			return TypedIdx(store.insert(result), Type::known_function);
		}
			
	} //build_function

} //namespace bmath::intern::arithmetic