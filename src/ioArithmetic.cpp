
#include <charconv>
#include <algorithm>
#include <numeric>

#include "ioArithmetic.hpp"
#include "termUtility.hpp"

namespace bmath::intern {

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////local definitions//////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	namespace vc {

		struct SumTraits
		{
			static constexpr Type type_name = Type(Op::sum);
			static constexpr char operator_char = '+';
			static constexpr char inverse_operator_char = '-';
			static constexpr Token operator_token = token::sum;
		};

		struct ProductTraits
		{
			static constexpr Type type_name = Type(Op::product);
			static constexpr char operator_char = '*';
			static constexpr char inverse_operator_char = '/';
			static constexpr Token operator_token = token::product;
		};

	} //namespace vc

	//VariadicTraits must include:
	//unsing declaration Object_T: type to construct (e.g. Sum)
	//<Enum type> type_name: name of operation in enum representing all types in store
	//char operator_char: the character symbolizing the operation, e.g. '+'
	//char inverse_operator_char: the character symbolizing the inverse operation, e.g. '-'
	//Token operator_token: the token symbolizing both normal and inverse operation, e.g. token::sum

	//BuildInverse recieves an already build term (by TypedIdx) and returns the inverse (by TypedIdx)
	//  e.g. for sum, it should turn "a" -> "a*(-1)", for product "a" -> "a^(-1)"
	//BuildAny can buld any type of term, this function will very likely already call build_variadic.
	template<typename VariadicTraits, typename Store_T, typename BuildInverse, typename BuildAny>
	[[nodiscard]] TypedIdx build_variadic(Store_T& store, ParseView input, std::size_t op_idx, BuildInverse build_inverse, BuildAny build_any);

	template<typename Store_T, typename BuildAny>
	[[nodiscard]] TypedIdx build_function(Store_T& store, ParseView input, const std::size_t open_par, BuildAny build_any);

	namespace pattern {

		//using Unknown = UnitEnum<"Unknown">;
		UNIT_ENUM(Unknown);

		using PnVariablesType = SumEnum<Restriction, Form, MultiPn, Unknown>;

		struct TypeProps
		{
			PnVariablesType type = Unknown{};
			std::string_view name = "";
		};

		constexpr auto type_table = std::to_array<TypeProps>({
			{ Op::sum             , "sum"           },
			{ Op::product         , "product"       },
			{ Leaf::variable      , "variable"      },
			{ Leaf::complex       , "value"         }, //not to be mistaken for Form::complex
			{ Restr::function     , "fn"            },
			{ Form::natural       , "nat"           },
			{ Form::natural_0     , "nat0"          },
			{ Form::integer       , "int"           },
			{ Form::real          , "real"          },
			{ Form::complex       , "complex"       }, //not to be mistaken for Leaf::complex
			{ Form::negative      , "negative"      },
			{ Form::not_negative  , "not_negative"  },
			{ Form::positive      , "positive"      },
			{ Form::not_positive  , "not_positive"  },
			{ Restr::any          , "any"           },
			{ Restr::nn1          , "nn1"           },
			{ Restr::no_val       , "no_val"        },
			{ MultiPn::summands   , "summands"      },
			{ MultiPn::factors    , "factors"       },
			{ MultiPn::params     , "params"        },
		});

		constexpr std::string_view name_of(const PnVariablesType r) noexcept { return find(type_table, &TypeProps::type, r).name; }
		constexpr PnVariablesType type_of(const std::string_view s) noexcept { return search(type_table, &TypeProps::name, s).type; }

	} //namespace pattern

	namespace print {

		//operator precedence (used to decide if parentheses are nessecary in out string)
		constexpr auto infixr_table = std::to_array<std::pair<Type, int>>({
			{ Type(Op::named_fn            ), 0 },
			{ Type(Op::sum                 ), 2 },
			{ Type(Op::product             ), 4 },	
			{ Type(Fn::pow                 ), 5 }, //not between other function types -> assumed to be printed with '^'  
			{ Type(Leaf::variable          ), 6 },
			{ Type(Leaf::complex           ), 6 }, //may be printed as sum/product itself, then (maybe) has to add parentheses on its own
			{ Type(PnNode::tree_match       ), 6 },
			{ Type(PnNode::value_match      ), 6 },
			{ Type(PnNode::value_proxy), 6 },
			{ Type(MultiPn::summands    ), 6 },
			{ Type(MultiPn::factors     ), 6 },
			{ Type(MultiPn::params      ), 6 },
		});
		static_assert(std::is_sorted(infixr_table.begin(), infixr_table.end(), [](auto a, auto b) { return a.second < b.second; }));

		constexpr int infixr(Type type) 
		{ 
			if (type.is<Fn>() && type != Fn::pow) {
				return 0;
			}
			else {
				return find(infixr_table, &std::pair<Type, int>::first, type).second;
			}
		}

		void append_complex(const std::complex<double> val, std::string& dest, int parent_operator_precedence)
		{
			std::stringstream buffer;

			enum class Flag { showpos, noshowpos };
			const auto add_im_to_stream = [&buffer](const double im, Flag flag) {
				if (im == -1.0) {
					buffer << '-';
				}
				else if (im == 1.0) {
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

			if (val.real() != 0.0 && val.imag() != 0.0) {
				parentheses = parent_operator_precedence > infixr(Type(Op::sum));
				buffer << val.real();
				add_im_to_stream(val.imag(), Flag::showpos);		
			}
			else if (val.real() != 0.0 && val.imag() == 0.0) {
				parentheses = val.real() < 0.0 && parent_operator_precedence >= infixr(Type(Op::sum));	//leading '-'
				buffer << val.real();
			}
			else if (val.real() == 0.0 && val.imag() != 0.0) {
				parentheses = val.imag() < 0.0 && parent_operator_precedence >= infixr(Type(Op::sum));	//leading '-'	
				parentheses |= parent_operator_precedence > infixr(Type(Op::product));	//*i
				add_im_to_stream(val.imag(), Flag::noshowpos);
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

	} //namespace print

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////exported in header/////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	namespace compute {

		Result exactly_computable(const ParseView view) noexcept
		{
			if (view.tokens.find_first_of(token::character) == TokenView::npos) {
				if (view.to_string_view().find_first_of("/-.ie") == std::string_view::npos) { //with prohibiting e, it it harder to easily surpass 2^53-1 (largest save integer stored as double)
					return Result::natural; //found no minus or i -> natural number is result
				}
				if (view.to_string_view().find_first_of("/^.e")  == std::string_view::npos) { //e is forbidden as it otherwise enables non-integer numbers (e.g. "1e-20") 
					return Result::complex; //found no power -> complex with { a + bi | a, b in Z } is result
				}
			}
			return Result::not_exactly_computable;
		} //exactly_computable

		std::complex<double> eval_complex(ParseView view)
		{
			const auto split = [](const ParseView view, const std::size_t at) {
				return std::make_pair(view.substr(0, at), view.substr(at + 1));
			};

			do {
				if (view.size() == 1u && view.tokens.starts_with(token::imag_unit)) { 
					return std::complex<double>(0.0, 1.0); 
				}				
				if (const std::size_t sum_pos = find_first_of_skip_pars(view.tokens, token::sum); sum_pos != TokenView::npos) {
					const auto [lhs, rhs] = split(view, sum_pos);
					switch (view.chars[sum_pos]) {
					case '+': return eval_complex(lhs) + eval_complex(rhs);
					case '-': return eval_complex(lhs) - eval_complex(rhs);
					}
				}				
				if (const std::size_t product_pos = find_first_of_skip_pars(view.tokens, token::product); product_pos != TokenView::npos) {
					const auto [lhs, rhs] = split(view, product_pos);
					return eval_complex(lhs) * eval_complex(rhs);
				}
				if (view.tokens.starts_with(token::unary_minus)) {
					view.remove_prefix(1u);
					return - eval_complex(view);
				}
				{
					const std::size_t not_number_pos = view.tokens.find_first_not_of(token::number);
					if (not_number_pos == TokenView::npos) {
						return parse_value(view);
					}
					else if (not_number_pos + 1u == view.size() && view.tokens.ends_with(token::imag_unit)) {
						view.remove_suffix(1u);
						return std::complex<double>(0.0, parse_value(view));
					}
				}
				if (!view.tokens.starts_with(token::open_grouping)) [[unlikely]] throw ParseFailure{ view.offset, "expected '(' or the like" };
				if (!view.tokens.ends_with(token::clse_grouping)) [[unlikely]] throw ParseFailure{ view.offset + view.size(), "expected ')' or the like" };
				view.remove_prefix(1u);
				view.remove_suffix(1u);
			} while (view.size());
			throw ParseFailure{ view.offset, "run out of characters" };
		} //eval_complex

		double eval_natural(ParseView view)
		{
			const auto split = [](const ParseView view, const std::size_t at) {
				return std::make_pair(view.substr(0, at), view.substr(at + 1));
			};

			do {			
				if (const auto sum_pos = find_first_of_skip_pars(view.tokens, token::sum); sum_pos != TokenView::npos) {
					const auto [lhs, rhs] = split(view, sum_pos);
					return eval_natural(lhs) + eval_natural(rhs);
				}				
				if (const auto product_pos = find_first_of_skip_pars(view.tokens, token::product); product_pos != TokenView::npos) {
					const auto [lhs, rhs] = split(view, product_pos);
					return eval_natural(lhs) * eval_natural(rhs);
				}				
				if (const auto pow_pos = find_first_of_skip_pars(view.tokens, token::hat); pow_pos != TokenView::npos) {
					const auto [lhs, rhs] = split(view, pow_pos);
					return std::pow(eval_natural(lhs), eval_natural(rhs));
				}
				if (view.tokens.find_first_not_of(token::number) == TokenView::npos) {
					return parse_value(view);
				}
				if (!view.tokens.starts_with(token::open_grouping)) [[unlikely]] throw ParseFailure{ view.offset, "expected '(' or the like" };
				if (!view.tokens.ends_with(token::clse_grouping)) [[unlikely]] throw ParseFailure{ view.offset + view.size(), "expected ')' or the like" };
				view.remove_prefix(1u);
				view.remove_suffix(1u);
			} while (view.size());
			throw ParseFailure{ view.offset, "run out of characters" };
		} //eval_natural

		double parse_value(const ParseView view)
		{
			double value;
			const auto [ptr, error] = std::from_chars(view.chars, view.chars + view.size(), value);
			if (error != std::errc()) [[unlikely]] throw ParseFailure{ view.offset, "value syntax is illformed or value out of bounds" };
			if (ptr != view.chars + view.size()) [[unlikely]] throw ParseFailure{ std::size_t(view.offset + ptr - view.chars + 1u), "value syntax is illformed" };
			return value;
		} //parse_value

	} //namespace compute

	std::size_t find_first_not_arithmetic(const TokenView view) noexcept
	{
		using namespace token;
		//'\0' only as end symbol for allowed_tokens, not as part of aritmetic symbols
		const Token allowed_tokens[] = { character, comma, hat, unary_minus, sum, product, number, imag_unit, open_grouping, clse_grouping, '\0' }; 
		return view.find_first_not_of(allowed_tokens);
	}

	Head find_head_type(const ParseView view)
	{
		switch (compute::exactly_computable(view)) {
		case compute::Result::complex: return Head{ 0u, Head::Type::complex_computable };
		case compute::Result::natural: return Head{ 0u, Head::Type::natural_computable };
		}

		if (const std::size_t sum_pos = find_first_of_skip_pars(view.tokens, token::sum); sum_pos != TokenView::npos) {
			return Head{ sum_pos, Head::Type::sum };
		}
		if (view.tokens.starts_with(token::unary_minus)) {
			return Head{ 0u, Head::Type::negate };
		}
		if (const std::size_t product_pos = find_first_of_skip_pars(view.tokens, token::product); product_pos != TokenView::npos) {
			return Head{ product_pos, Head::Type::product };
		}
		if (const std::size_t pow_pos = find_first_of_skip_pars(view.tokens, token::hat); pow_pos != TokenView::npos) {
			return Head{ pow_pos, Head::Type::power };
		}
		{
			const std::size_t not_number_pos = view.tokens.find_first_not_of(token::number);
			if (not_number_pos == TokenView::npos) {
				return Head{ 0u, Head::Type::real_value };
			}
			else if (not_number_pos + 1u == view.size() && view.tokens.ends_with(token::imag_unit)) {
				return Head{ 0u, Head::Type::imag_value };
			}
		}
		const std::size_t first_not_character = view.tokens.find_first_not_of(token::character);
		if (first_not_character == TokenView::npos) {
			return Head{ 0u, Head::Type::variable };
		}
		if (view.tokens[first_not_character] != token::open_grouping) [[unlikely]] throw ParseFailure{ first_not_character + view.offset, "illegal character, expected '('" };
		if (!view.tokens.ends_with(token::clse_grouping)) [[unlikely]] throw ParseFailure{ view.size() + view.offset, "poor grouping, expected ')'" };
		if (first_not_character == 0u) {
			return Head{ 0u, Head::Type::group };
		}
		else {
			return Head{ first_not_character, Head::Type::function };
		}
	} //find_head_type

	TypedIdx build(Store& store, ParseView input)
	{
		if (input.size() == 0u) [[unlikely]] throw ParseFailure{ input.offset, "recieved empty substring" };
		Head head = find_head_type(input);
		while (head.type == Head::Type::group) {
			input.remove_prefix(1u);
			input.remove_suffix(1u);
			head = find_head_type(input);
		}
		switch (head.type) {
		case Head::Type::sum: {
			return build_variadic<vc::SumTraits>(store, input, head.where, build_negated<Store>, build);
		} break;
		case Head::Type::negate: {
			input.remove_prefix(1u);  //remove minus sign
			const TypedIdx to_negate = build(store, input);
			return build_negated(store, to_negate);
		} break;
		case Head::Type::product: {
			return build_variadic<vc::ProductTraits>(store, input, head.where, build_inverted<Store>, build);
		} break;
		case Head::Type::power: {
			const auto base_view = input.steal_prefix(head.where);
			input.remove_prefix(1u); //remove hat
			const TypedIdx base = build(store, base_view);
			const TypedIdx expo = build(store, input);
			return TypedIdx(store.insert(FnParams{ base, expo, TypedIdx(), TypedIdx() }), Type(Fn::pow));
		} break;
		case Head::Type::complex_computable: {
			return build_value(store, compute::eval_complex(input));
		} break;
		case Head::Type::natural_computable: { 
			return build_value(store, compute::eval_natural(input));
		} break;
		case Head::Type::real_value: {
			return build_value(store, compute::parse_value(input));
		} break;
		case Head::Type::imag_value: {
			input.remove_suffix(1u); //remove token::imag_unit
			return build_value(store, Complex(0.0, compute::parse_value(input)));
		} break;
		case Head::Type::function: {
			return build_function(store, input, head.where, build);
		} break;
		case Head::Type::variable: {
			return TypedIdx(Variable::build(store, input.to_string_view()), Type(Leaf::variable));
		} break;
		default: 
			assert(false); 
			return TypedIdx();
		}
	} //build

	template<typename VariadicTraits, typename Store_T, typename BuildInverse, typename BuildAny>
	TypedIdx build_variadic(Store_T& store, ParseView input, std::size_t op_idx, BuildInverse build_inverse, BuildAny build_any)
	{
		const std::size_t variadic_idx = store.insert(TypedIdxSLC());
		std::size_t last_node_idx;
		{
			const ParseView subterm_view = input.steal_prefix(op_idx);
			const TypedIdx subterm = build_any(store, subterm_view);
			last_node_idx = TypedIdxSLC::insert_new(store, variadic_idx, subterm);
		}
		while (input.size()) {
			const char current_operator = input.chars[0u];
			input.remove_prefix(1u); //remove current_operator;
			op_idx = find_first_of_skip_pars(input.tokens, VariadicTraits::operator_token);
			const ParseView subterm_view = input.steal_prefix(op_idx);
			const TypedIdx subterm = build_any(store, subterm_view);
			switch (current_operator) {
			case VariadicTraits::operator_char:
				last_node_idx = TypedIdxSLC::insert_new(store, last_node_idx, subterm);
				break;
			case VariadicTraits::inverse_operator_char:
				last_node_idx = TypedIdxSLC::insert_new(store, last_node_idx, build_inverse(store, subterm));
				break;
			default: assert(false);
			}
		}
		return TypedIdx(variadic_idx, VariadicTraits::type_name);
	} //build_variadic

	template<typename Store_T, typename BuildAny>
	[[nodiscard]] TypedIdx build_function(Store_T& store, ParseView input, const std::size_t open_par, BuildAny build_any)
	{
		const auto type = fn::type_of(input.to_string_view(0u, open_par));
		if (type == Fn::COUNT) { //build generic function
			const std::size_t result_index = store.allocate(); //allocate function node first -> better positioning in store
			NamedFn result;
			{//writing name in result
				const auto new_name = std::string_view(input.chars, open_par);
				if (new_name.size() > NamedFn::max_name_size) [[unlikely]] throw ParseFailure{ input.offset, "named_fn name exceeds max length" };
				std::copy(new_name.begin(), new_name.end(), &result.name[0]);
			}
			//writing parameters in result
			input.remove_suffix(1u);            //"pow(2,4)" -> "pow(2,4"
			input.remove_prefix(open_par + 1u); //"pow(2,4" ->      "2,4"
			result.params_idx = store.insert(TypedIdxSLC());
			std::size_t last_node_idx = result.params_idx;
			if (input.size()) { //else no parameters at all
				const std::size_t comma = find_first_of_skip_pars(input.tokens, token::comma);
				const auto param_view = input.steal_prefix(comma); //now input starts with comma
				const TypedIdx param = build_any(store, param_view);
				last_node_idx = TypedIdxSLC::insert_new(store, last_node_idx, param);
			}
			while (input.size()) {
				input.remove_prefix(1u); //erase comma
				const std::size_t comma = find_first_of_skip_pars(input.tokens, token::comma);
				const auto param_view = input.steal_prefix(comma);
				const TypedIdx param = build_any(store, param_view);
				last_node_idx = TypedIdxSLC::insert_new(store, last_node_idx, param);
			}
			store.at(result_index) = result;
			return TypedIdx(result_index, Type(Op::named_fn));
		}
		else { //build known function
			const std::size_t result_index = store.allocate(); //allocate function node first -> better positioning in store
			FnParams result{ TypedIdx(), TypedIdx(), TypedIdx(), TypedIdx() };
			input.remove_suffix(1u);
			input.remove_prefix(open_par + 1u);	//only arguments are left
			std::size_t comma = find_first_of_skip_pars(input.tokens, token::comma);
			auto param_view = input.steal_prefix(comma);
			for (auto& param : fn::range(result, Type(type))) {
				if (param_view.size() == 0u) [[unlikely]] throw ParseFailure{ input.offset, "too few function parameters" };
				param = build_any(store, param_view);
				input.remove_prefix(1u); // remove comma
				comma = find_first_of_skip_pars(input.tokens, token::comma);
				param_view = input.steal_prefix(comma);
			}
			if (param_view.size() > 0u) [[unlikely]] throw ParseFailure{ input.offset, "too many function parameters" };
			store.at(result_index) = result;
			return TypedIdx(result_index, Type(type));
		}
	} //build_function

	namespace pattern {

		PatternParts::PatternParts(const ParseView input)
		{
			const std::size_t bar = find_first_of_skip_pars(input.tokens, token::bar);
			if (count_skip_pars(input.tokens, token::bar) > 1u) [[unlikely]] throw ParseFailure{ input.offset + bar, "expected only this '|', no further ones at top grouping level" };
			const std::size_t equals = find_first_of_skip_pars(input.tokens, token::equals);
			if (count_skip_pars(input.tokens, token::equals) > 1u) [[unlikely]] throw ParseFailure{ input.offset + equals, "expected only this '=', no further ones at top grouping level" };
			if (equals == TokenView::npos) [[unlikely]] throw ParseFailure{ input.size() - 1u, "expected '=' at top grouping level" };

			if (bar != TokenView::npos) [[likely]] {
				this->declarations = input.substr(0u, bar);
				this->lhs          = input.substr(bar + 1u, equals - bar - 1u);
				this->rhs          = input.substr(equals + 1u);
			}
			else {
				this->declarations = input.substr(0u, 0u);
				this->lhs          = input.substr(0u, equals);
				this->rhs          = input.substr(equals + 1u);
			}
		} //PatternParts::PatternParts

		NameLookupTable::NameLookupTable(ParseView declarations)
		{
			const auto parse_declaration = [this](ParseView var_view) {
				const std::size_t colon = find_first_of_skip_pars(var_view.tokens, token::colon);
				if (colon != TokenView::npos) {
					const PnVariablesType type = type_of(var_view.to_string_view(colon + 1u));
					if (type.is<Unknown>()) [[unlikely]] throw ParseFailure{ var_view.offset + colon + 1u, "unknown restriction" };

					if (type.is<Form>()) {
						this->value_table.emplace_back(var_view.to_string_view(0, colon), type.to<Form>());
					}
					else if (type.is<MultiPn>()) {
						this->multi_table.emplace_back(var_view.to_string_view(0, colon), type.to<MultiPn>());
					}
					else {
						assert(type.is<Restriction>());
						this->tree_table.emplace_back(var_view.to_string_view(0, colon), type.to<Restriction>());
					}
				}
				else {
					this->tree_table.emplace_back(var_view.to_string_view(), Restriction(Restr::any));
				}
			};

			if (declarations.size()){
				const std::size_t comma = find_first_of_skip_pars(declarations.tokens, token::comma);
				parse_declaration(declarations.steal_prefix(comma));
			}
			while (declarations.size()) {
				declarations.remove_prefix(1); //erase comma
				const std::size_t comma = find_first_of_skip_pars(declarations.tokens, token::comma);
				parse_declaration(declarations.steal_prefix(comma));
			}
		} //NameLookupTable::NameLookupTable

		TypedIdx NameLookupTable::insert_instance(Store& store, const ParseView input)
		{
			const auto name = input.to_string_view();
			const auto search_name = [name](auto& vec) { 
				return std::find_if(vec.begin(), vec.end(), [name](const auto& x) { return x.name == name; });
			};

			auto var_idx = TypedIdx();
			if (const auto iter = search_name(this->tree_table); iter != this->tree_table.end()) {
				const std::uint32_t match_data_idx = std::distance(this->tree_table.begin(), iter);
				const TreeMatchVariable var = { match_data_idx, iter->restr };
				var_idx = TypedIdx(store.insert(var), PnNode::tree_match);
				(this->build_lhs ? iter->lhs_instances : iter->rhs_instances).push_back(var_idx);
			}
			else if (const auto iter = search_name(this->value_table); iter != this->value_table.end()) {
				const std::uint32_t match_data_idx = std::distance(this->value_table.begin(), iter);
				const ValueMatchVariable var(match_data_idx, iter->form);
				var_idx = TypedIdx(store.insert(var), PnNode::value_match);
				(this->build_lhs ? iter->lhs_instances : iter->rhs_instances).push_back(var_idx);
			}
			else if (const auto iter = search_name(this->multi_table); iter != this->multi_table.end()) {
				const std::uint32_t match_data_idx = std::distance(this->multi_table.begin(), iter);
				var_idx = TypedIdx(match_data_idx, iter->type);
			}
			if (var_idx == TypedIdx()) [[unlikely]] throw ParseFailure{ input.offset, "match variable has not been declared" };
			return var_idx;
		} //NameLookupTable::insert_instance


		TypedIdx PatternBuildFunction::operator()(Store& store, ParseView input)
		{
			if (input.size() == 0u) [[unlikely]] throw ParseFailure{ input.offset, "recieved empty substring" };
			Head head = find_head_type(input);
			while (head.type == Head::Type::group) {
				input.remove_prefix(1u);
				input.remove_suffix(1u);
				head = find_head_type(input);
			}
			switch (head.type) {
			case Head::Type::sum: {
				return build_variadic<vc::SumTraits>(store, input, head.where, build_negated<Store>, *this);
			} break;
			case Head::Type::negate: {
				input.remove_prefix(1u);  //remove minus sign
				const TypedIdx to_negate = this->operator()(store, input);
				return build_negated(store, to_negate);
			} break;
			case Head::Type::product: {
				return build_variadic<vc::ProductTraits>(store, input, head.where, build_inverted<Store>, *this);
			} break;
			case Head::Type::power: {
				const auto base_view = input.steal_prefix(head.where);
				input.remove_prefix(1u); //remove hat
				const TypedIdx base = this->operator()(store, base_view);
				const TypedIdx expo = this->operator()(store, input);
				return TypedIdx(store.insert(FnParams{ base, expo, TypedIdx(), TypedIdx() }), Type(Fn::pow));
			} break;
			case Head::Type::complex_computable: {
				return build_value(store, compute::eval_complex(input));
			} break;
			case Head::Type::natural_computable: { 
				return build_value(store, compute::eval_natural(input));
			} break;
			case Head::Type::real_value: {
				return build_value(store, compute::parse_value(input));
			} break;
			case Head::Type::imag_value: {
				input.remove_suffix(1u); //remove token::imag_unit
				return build_value(store, Complex(0.0, compute::parse_value(input)));
			} break;
			case Head::Type::function: {
				return build_function(store, input, head.where, *this);
			} break;
			case Head::Type::variable: {
				if (input.chars[0u] == '\'') {
					if (input.chars[input.size() - 1u] != '\'') [[unlikely]] throw ParseFailure{ input.offset + 1u, "found no matching \"'\"" };
					return TypedIdx(Variable::build(store, input.to_string_view(1u, input.size() - 1u)), Type(Leaf::variable));
				}
				else {
					return this->table.insert_instance(store, input);
				}
			} break;
			default: 
				assert(false); 
				return TypedIdx();
			}
		} //PatternBuildFunction::operator()

	} //namespace pattern

	namespace print {

		void append_to_string(const Ref ref, std::string& str, const int parent_infixr)
		{
			if (!ref.store->valid_idx(ref.index) && ref.type != PnNode::value_proxy && !ref.type.is<MultiPn>()) {
				str.append("ERROR");
				return;
			}

			const int own_infixr = infixr(ref.type);
			if (own_infixr <= parent_infixr) {
				str.push_back('(');
			}

			switch (ref.type) {
			case Type(Op::sum): {
				const char* seperator = "";
				for (const auto summand : vc::range(ref)) {
					str.append(std::exchange(seperator, "+"));
					print::append_to_string(ref.new_at(summand), str, own_infixr);
				}
			} break;
			case Type(Op::product): {
				const char* seperator = "";
				for (const auto factor : vc::range(ref)) {
					str.append(std::exchange(seperator, "*"));
					print::append_to_string(ref.new_at(factor), str, own_infixr);
				}
			} break;
			case Type(Fn::pow): {
				const FnParams& params = *ref;
				print::append_to_string(ref.new_at(params[0]), str, own_infixr);
				str.push_back('^');
				print::append_to_string(ref.new_at(params[1]), str, own_infixr);
			} break;
			case Type(Op::named_fn): {
				const NamedFn& named_fn = *ref;
				str.pop_back(); //pop open parenthesis
				str.append(named_fn.name_view());
				str.push_back('(');
				const char* seperator = "";
				for (const auto param : fn::range(ref)) {
					str.append(std::exchange(seperator, ", "));
					print::append_to_string(ref.new_at(param), str, own_infixr);
				}
			} break;
			default: {
				assert(ref.type.is<Fn>());
				str.pop_back(); //pop '('
				str.append(fn::name_of(ref.type.to<Fn>()));
				str.push_back('(');
				const char* seperator = "";
				for (const auto param : fn::range(ref->fn_params, ref.type)) {
					str.append(std::exchange(seperator, ", "));
					print::append_to_string(ref.new_at(param), str, own_infixr);
				}
			} break;
			case Type(Leaf::variable): {
				str += std::string_view(ref->variable.data, ref->variable.size);
			} break;
			case Type(Leaf::complex): {
				append_complex(ref->complex, str, parent_infixr);
			} break;
			case Type(PnNode::tree_match): {
				const pattern::TreeMatchVariable& var = *ref;
				str.append("{T");
				str.append(std::to_string(var.match_data_idx));
				if (var.restr != pattern::Restr::any) {
					str.push_back(':');
					str.append(name_of(var.restr));
				}
				str.push_back('}');
			} break;
			case Type(PnNode::value_match): {
				const pattern::ValueMatchVariable& var = *ref;
				str.append("{V");
				str.append(std::to_string(var.match_data_idx));
				str.push_back(':');
				str.append(name_of(var.form));
				str.append(", ");
				print::append_to_string(ref.new_at(var.mtch_idx), str);
				str.append(", ");
				print::append_to_string(ref.new_at(var.copy_idx), str);
				str.push_back('}');
			} break;
			case Type(PnNode::value_proxy): {
				str.push_back('P');
			} break;
			case Type(MultiPn::summands): {
				str.push_back('S');
				str.append(std::to_string(ref.index));
				str.append("...");
			} break;
			case Type(MultiPn::factors): {
				str.push_back('F');
				str.append(std::to_string(ref.index));
				str.append("...");
			} break;
			case Type(MultiPn::params): {
				str.push_back('P');
				str.append(std::to_string(ref.index));
				str.append("...");
			} break;
			}

			if (own_infixr <= parent_infixr) {
				str.push_back(')');
			}
		} //append_to_string

		std::string to_pretty_string(const Ref ref, const int parent_infixr)
		{
			std::string str;

			bool need_parentheses = infixr(ref.type) <= parent_infixr;

			const auto get_negative_real = [](const Ref ref) ->OptDouble {
				if (ref.type == Leaf::complex) {
					const Complex& complex = *ref;
					if (complex.real() < 0.0 && complex.imag() == 0.0) {
						return { complex.real() };
					}
				}
				return {};
			}; //get_negative_real

			 //returns base, if ref is actually <base>^(-1)
			const auto get_pow_neg1 = [get_negative_real](const Ref ref) -> std::optional<TypedIdx> {
				if (ref.type == Fn::pow) {
					const FnParams& params = *ref;
					if (const auto expo = get_negative_real(ref.new_at(params[1]))) {
						if (*expo == -1.0) {
							return { params[0] };
						}
					}
				}
				return {};
			}; //get_pow_neg1

			struct GetNegativeProductResult { double negative_factor; StupidBufferVector<TypedIdx, 8> other_factors; };
			const auto get_negative_product = [get_negative_real](const Ref ref) -> std::optional<GetNegativeProductResult> {
				if (ref.type == Op::product) {
					StupidBufferVector<TypedIdx, 8> other_factors;
					double negative_factor;
					bool found_negative_factor = false;
					for (const auto factor : vc::range(ref)) {
						if (!found_negative_factor) {
							if (const auto negative_val = get_negative_real(ref.new_at(factor))) {
								negative_factor = *negative_val;
								found_negative_factor = true;
								continue;
							}
						}
						other_factors.push_back(factor);
					}
					if (found_negative_factor) {
						return { { negative_factor, std::move(other_factors)} };
					}
				}
				return {};
			}; //get_negative_product

			const auto append_product = [get_negative_real, get_pow_neg1, &ref, &str](const auto& vec) {
				bool first = true;
				for (const auto elem : vec) {
					if (auto val = get_negative_real(ref.new_at(elem)); val && first && *val == -1.0) {
						str += "-";
					}
					else if (const auto base = get_pow_neg1(ref.new_at(elem))) {
						str += (first ? "1/" : "/"); 
						str += print::to_pretty_string(ref.new_at(*base), infixr(Type(Op::product)));
						first = false;
					}
					else {
						str += (first ? "" : " ");
						//str += (first ? "" : "*");
						str += print::to_pretty_string(ref.new_at(elem), infixr(Type(Op::product)));
						first = false;
					}
				}
				assert(!first && "found product with only single factor -1 or zero factors");
			}; //append_product

			switch (ref.type) {
			case Type(Op::sum): {
				bool first = true;
				for (const auto summand : vc::range(ref)) {
					if (const auto val = get_negative_real(ref.new_at(summand))) {
						str += (first ? "" : " ");
						append_real(*val, str);
					}
					else if (auto product = get_negative_product(ref.new_at(summand))) {
						if (product->negative_factor != -1.0) {
							str += (first ? "" : " ");
							append_real(product->negative_factor, str);
							str += " ";
							//str += "*";
						}
						else {
							str +=  (first ? "-" : " - ");
							//str += "-";
						}
						append_product(product->other_factors);
					}
					else {
						str += (first ? "" : " + ");
						//str += (first ? "" : "+");
						str += print::to_pretty_string(ref.new_at(summand), infixr(ref.type));
					}
					first = false;
				}
				assert(!first && "found sum with zero summands");
			} break;
			case Type(Op::product): {
				append_product(vc::range(ref));
			} break;
			case Type(Fn::pow): {
				const FnParams& params = *ref;
				if (const OptDouble negative_expo = get_negative_real(ref.new_at(params[1]))) {
					str += "1/";
					str += print::to_pretty_string(ref.new_at(params[0]), infixr(Type(Fn::pow)));
					if (*negative_expo != -1.0) {
						str += "^";
						append_real(-*negative_expo, str);
					}
				}
				else {
					str += print::to_pretty_string(ref.new_at(params[0]), infixr(ref.type));
					str += "^";
					str += print::to_pretty_string(ref.new_at(params[1]), infixr(ref.type));
				}
			} break;
			case Type(Op::named_fn): {
				need_parentheses = false;
				str.append(ref->named_fn.name_view());
				str.push_back('(');
				bool first = true;
				for (const auto param : fn::range(ref)) {
					if (!std::exchange(first, false)) {
						str += ", ";
					}
					str += print::to_pretty_string(ref.new_at(param), infixr(ref.type));
				}
				str.push_back(')');
			} break;
			default: {
				assert(ref.type.is<Fn>());
				need_parentheses = false;
				str.append(fn::name_of(ref.type.to<Fn>()));
				str.push_back('(');
				const char* separator = "";
				for (const auto param : fn::range(ref->fn_params, ref.type)) {
					str += std::exchange(separator, ", ");
					str += print::to_pretty_string(ref.new_at(param), infixr(ref.type));
				}
				str.push_back(')');
			} break;
			case Type(Leaf::variable): {
				str += std::string_view(ref->variable.data, ref->variable.size);
			} break;
			case Type(Leaf::complex): {
				append_complex(ref->complex, str, parent_infixr);
			} break;
			}

			if (need_parentheses) {
				return '(' + str + ')';
			}
			else {
				return str;
			}
		} //to_pretty_string

		void append_memory_row(const Ref ref, std::vector<std::string>& rows)
		{
			const auto show_typedidx_col_nodes = [&ref, &rows](std::uint32_t idx, bool show_first) {
				const TypedIdxSLC* col = &ref.store->at(idx).index_slc;
				if (show_first) {
					rows[idx].append("(SLC node part of index " + std::to_string(ref.index) + ')');
				}
				while (col->next_idx != TypedIdxSLC::null_index) {
					rows[col->next_idx].append("(SLC node part of index " + std::to_string(ref.index) + ')');
					col = &ref.store->at(col->next_idx).index_slc;
				}
			};
			const auto show_string_nodes = [&ref, &rows](std::uint32_t idx, bool show_first) {
				const Variable& var = ref.store->at(idx);
				const std::size_t end = idx + Variable::node_count(var.capacity);
				if (!show_first) {
					idx++;
				}
				while (idx < end) {
					rows[idx].append("(str node part of index " + std::to_string(ref.index));
					idx++;
				}
			};

			if (!ref.store->valid_idx(ref.index) && ref.type != PnNode::value_proxy && !ref.type.is<MultiPn>()) {
				rows.front().append("ERROR");
				return;
			}

			std::string& current_str = rows[ref.index];
			switch (ref.type) {
			case Type(Op::sum): {
				current_str.append("sum        : {");
				const char* separator = "";
				for (const auto elem : vc::range(ref)) {
					current_str.append(std::exchange(separator, ", "));
					current_str.append(std::to_string(elem.get_index()));
					print::append_memory_row(ref.new_at(elem), rows);
				}
				current_str.push_back('}');
				show_typedidx_col_nodes(ref.index, false);
			} break;
			case Type(Op::product): {
				current_str.append("product    : {");
				const char* separator = "";
				const TypedIdxSLC test = *ref;
				for (const auto elem : vc::range(ref)) {
					current_str.append(std::exchange(separator, ", "));
					current_str.append(std::to_string(elem.get_index()));
					print::append_memory_row(ref.new_at(elem), rows);
				}
				current_str.push_back('}');
				show_typedidx_col_nodes(ref.index, false);
			} break;
			case Type(Op::named_fn): {
				const NamedFn& named_fn = *ref;
				current_str.append("function?  : {");
				const char* separator = "";
				for (const auto param : fn::range(ref)) {
					current_str.append(std::exchange(separator, ", "));
					current_str.append(std::to_string(param.get_index()));
					print::append_memory_row(ref.new_at(param), rows);
				}
				current_str.push_back('}');
				show_typedidx_col_nodes(named_fn.params_idx, true);
			} break;
			default: {
				assert(ref.type.is<Fn>());
				current_str.append("function   : {");
				const char* separator = "";
				for (const auto param : fn::range(ref->fn_params, ref.type)) {
					current_str.append(std::exchange(separator, ", "));
					current_str.append(std::to_string(param.get_index()));
					print::append_memory_row(ref.new_at(param), rows);
				}
				current_str.push_back('}');
			} break;
			case Type(Leaf::variable): {
				current_str.append("variable   : ");
				show_string_nodes(ref.index, false);
			} break;
			case Type(Leaf::complex): {
				current_str.append("value      : ");
			} break;
			case Type(PnNode::tree_match): {
				current_str.append("tree_match : ");
			} break;
			case Type(PnNode::value_match): {
				const pattern::ValueMatchVariable& var = *ref;
				current_str.append("value: {m:");
				current_str.append(std::to_string(var.mtch_idx.get_index()));
				current_str.append(" c:");
				current_str.append(std::to_string(var.copy_idx.get_index()));
				current_str.push_back('}');
				print::append_memory_row(ref.new_at(var.mtch_idx), rows);
				print::append_memory_row(ref.new_at(var.copy_idx), rows);
			} break;
			case Type(PnNode::value_proxy): 
				[[fallthrough]];
			case Type(MultiPn::summands):
				[[fallthrough]];
			case Type(MultiPn::factors):
				[[fallthrough]];
			case Type(MultiPn::params):
				return;
			}

			//append name of subterm to line
			current_str.append(std::max(0, 35 - (int)current_str.size()), ' ');
			print::append_to_string(ref, current_str, 0);
		} //append_memory_row

		std::string to_memory_layout(const Store& store, const std::initializer_list<const TypedIdx> heads)
		{
			std::vector<std::string> rows(store.size(), "");

			std::string result((heads.size() == 1u) ? "   | head at index: " : "   | heads at indices: ");
			result.reserve(store.size() * 15);
			{
				const char* separator = "";
				for (const auto head : heads) {
					result += std::exchange(separator, ", ");
					result += std::to_string(head.get_index());
					print::append_memory_row(Ref(store, head), rows);
				}
				result += "\n";
			}
			for (const auto i : store.enumerate_free_slots()) {
				rows[i].append("-----free slot-----");
			}
			{
				int i = 0;
				for (auto& elem : rows) {
					if (i < 10) { //please std::format, i need you :(
						result += " ";
					}
					result += std::to_string(i);
					result += " | ";
					result += elem;
					result += "\n";
					i++;
				}
			}
			return result;
		} //to_memory_layout

		//line name assumes '\0' as last character
		void append_tree_row(const Ref ref, std::vector<std::string>& rows, const std::size_t offset)
		{
			constexpr std::size_t tab_width = 3u; //specifies how may more characters the subtrees of ref are shifted right, compared to ref

			rows.push_back(std::string(offset, ' '));
			std::string& current_str = rows.back();

			//appending current_str to previously inserted parts of tree visually
			{
				constexpr signed char bar_up_down = -77;		//(179)
				constexpr signed char bar_up_right = -64;		//(192)
				constexpr signed char bar_up_right_down = -61;	//(195)
				constexpr signed char bar_left_right = -60;	    //(196)

				{
					assert(rows.size() > 1u); //"head" is first row
					std::size_t bar_row_i = rows.size() - 2u; //start one row above current_str
					for (; rows[bar_row_i][offset] == ' '; bar_row_i--) {
						rows[bar_row_i][offset] = bar_up_down;
					}
					if (rows[bar_row_i][offset] == bar_up_right) {
						rows[bar_row_i][offset] = bar_up_right_down;
					}
				}

				current_str += bar_up_right;
				static_assert(tab_width >= 2u);
				current_str += std::string(tab_width - 2u, bar_left_right);
			}

			switch (ref.type) {
			case Type(Op::sum): {
				current_str += "sum";
				for (const TypedIdx summand : vc::range(ref)) {
					print::append_tree_row(ref.new_at(summand), rows, offset + tab_width);
				}
			} break;
			case Type(Op::product): {
				current_str += "product";
				for (const TypedIdx factor : vc::range(ref)) {
					print::append_tree_row(ref.new_at(factor), rows, offset + tab_width);
				}
			} break;
			case Type(Op::named_fn): {
				current_str.append(ref->named_fn.name_view());
				for (const TypedIdx param : fn::range(ref)) {
					print::append_tree_row(ref.new_at(param), rows, offset + tab_width);
				}
			} break;
			default: {
				assert(ref.type.is<Fn>());
				current_str += fn::name_of(ref.type.to<Fn>());
				for (const TypedIdx param : fn::range(ref->fn_params, ref.type)) {
					print::append_tree_row(ref.new_at(param), rows, offset + tab_width);
				}
			} break;
			case Type(Leaf::variable): {
				current_str += ' ';
				current_str += std::string_view(ref->variable.data, ref->variable.size);
			} break;
			case Type(Leaf::complex): {
				current_str += ' ';
				print::append_complex(*ref, current_str, 0u);
			} break;
			case Type(PnNode::tree_match): {
				const pattern::TreeMatchVariable& var = *ref;
				current_str += "T";
				current_str += std::to_string(var.match_data_idx);
				if (var.restr != pattern::Restr::any) {
					current_str += " :";
					current_str += name_of(var.restr);
				}
			} break;
			case Type(PnNode::value_match): {
				const pattern::ValueMatchVariable& var = *ref;
				current_str += 'V';
				current_str += std::to_string(var.match_data_idx);
				current_str += " :";
				current_str += name_of(var.form);
				print::append_tree_row(ref.new_at(var.mtch_idx), rows, offset + tab_width);
				print::append_tree_row(ref.new_at(var.copy_idx), rows, offset + tab_width);
			} break;
			case Type(PnNode::value_proxy): {
				current_str += 'P';			
			} break;
			case Type(MultiPn::summands): {
				current_str += 'S';		
				current_str += std::to_string(ref.index);
				current_str += "...";
			} break;
			case Type(MultiPn::factors): {
				current_str += 'F';		
				current_str += std::to_string(ref.index);
				current_str += "...";
			} break;
			case Type(MultiPn::params): {
				current_str += 'P';		
				current_str += std::to_string(ref.index);
				current_str += "...";
			} break;
			}
		} //append_tree_row

		std::string to_tree(const Ref ref, const std::size_t offset)
		{
			std::vector<std::string> rows;
			rows.push_back(std::string(offset, ' ') + "head");
			print::append_tree_row(ref, rows, offset);

			std::string result;
			for (const auto& row : rows) {
				result.append(row);
				result.push_back('\n');
			}

			return result;
		} //to_tree

	} //namespace print

} //namespace bmath::intern
