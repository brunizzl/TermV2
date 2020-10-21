
#include <charconv>
#include <algorithm>
#include <numeric>

#include "ioArithmetic.hpp"
#include "termUtility.hpp"

namespace bmath::intern {

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////local definitions//////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	//utility for both Function and GenericFunction
	namespace fn {

		constexpr auto name_table = std::to_array<std::pair<FnType, std::string_view>>({
			{ FnType::asinh, "asinh" },	
			{ FnType::acosh, "acosh" },
			{ FnType::atanh, "atanh" },	
			{ FnType::asin , "asin"  },	
			{ FnType::acos , "acos"  },	
			{ FnType::atan , "atan"  },	
			{ FnType::sinh , "sinh"  },	
			{ FnType::cosh , "cosh"  },	
			{ FnType::tanh , "tanh"  },	
			{ FnType::sqrt , "sqrt"  },	
			{ FnType::pow  , "pow"   },   
			{ FnType::log  , "log"   },	
			{ FnType::exp  , "exp"   },	
			{ FnType::sin  , "sin"   },	
			{ FnType::cos  , "cos"   },	
			{ FnType::tan  , "tan"   },	
			{ FnType::abs  , "abs"   },	
			{ FnType::arg  , "arg"   },	
			{ FnType::ln   , "ln"    },	
			{ FnType::re   , "re"    },	
			{ FnType::im   , "im"    },	
		});

		constexpr std::string_view name_of(const FnType type) noexcept { return find_snd(name_table, type); }
		constexpr FnType type_of(const std::string_view name) noexcept { return search_fst(name_table, name, FnType::COUNT); }

		//appends only name, no parentheses or anything fancy
		template<typename Store_T>
		void append_name(const Store_T& store, const GenericFunction& func, std::string& str)
		{
			if (func.name_size == GenericFunction::NameSize::small) {
				str.append(func.short_name);
			}
			else {
				read(store, func.long_name_idx, str);
			}
		} //append_name 

	} //namespace fn

	namespace vdc {

		struct BasicSumTraits
		{
			static constexpr Type type_name = Node::sum;
			static constexpr char operator_char = '+';
			static constexpr char inverse_operator_char = '-';
			static constexpr Token operator_token = token::sum;
		};

		struct SumTraits : public BasicSumTraits { using Object_T = Sum; };
		struct PnSumTraits : public BasicSumTraits { using Object_T = pattern::PnSum; };



		struct BasicProductTraits
		{
			static constexpr Type type_name = Node::product;
			static constexpr char operator_char = '*';
			static constexpr char inverse_operator_char = '/';
			static constexpr Token operator_token = token::product;
		};

		struct ProductTraits : public BasicProductTraits { using Object_T = Product; };
		struct PnProductTraits : public BasicProductTraits { using Object_T = pattern::PnProduct; };

	} //namespace vdc

	//VariadicTraits must include:
	//unsing declaration Object_T: type to construct (e.g. Sum)
	//<Enum type> type_name: name of operation in enum representing all types in store
	//char operator_char: the character symbolizing the operation, e.g. '+'
	//char inverse_operator_char: the character symbolizing the inverse operation, e.g. '-'
	//Token operator_token: the token symbolizing both normal and inverse operation, e.g. token::sum

	//BuildInverse recieves an already build term (by TypedIdx_T) and returns the inverse (by TypedIdx_T)
	//  e.g. for sum, it should turn "a" -> "a*(-1)", for product "a" -> "a^(-1)"
	//BuildAny can buld any type of term, this function will very likely already call build_variadic.
	template<typename VariadicTraits, typename TypedIdx_T, typename TermStore_T, typename BuildInverse, typename BuildAny>
	[[nodiscard]] TypedIdx_T build_variadic(TermStore_T& store, ParseView input, std::size_t op_idx, BuildInverse build_inverse, BuildAny build_any);

	template<typename TypedIdx_T, typename TermStore_T, typename BuildAny>
	[[nodiscard]] TypedIdx_T build_function(TermStore_T& store, ParseView input, const std::size_t open_par, BuildAny build_any);

	namespace pattern {

		constexpr auto form_name_table = std::to_array<std::pair<ParseRestriction, std::string_view>>({
			{ Type(Node::sum     ), "sum"           },
			{ Type(Node::product ), "product"       },
			{ Type(Leaf::variable), "variable"      },
			{ Type(Leaf::complex ), "complex"       },
			{ Restr::function     , "fn"            },
			{ Form::natural       , "nat"           },
			{ Form::natural_0     , "nat0"          },
			{ Form::integer       , "int"           },
			{ Form::real          , "real"          },
			{ Form::not_minus_one , "not_minus_one" },
			{ Form::negative      , "negative"      },
			{ Form::not_negative  , "not_negative"  },
			{ Form::positive      , "positive"      },
			{ Form::not_positive  , "not_positive"  },
			{ Restr::any          , "any"           },
		});

		constexpr std::string_view name_of(const ParseRestriction r) noexcept { return find_snd(form_name_table, r); }
		constexpr ParseRestriction form_type(const std::string_view s) noexcept { return search_fst(form_name_table, s, ParseRestriction(Restr::unknown)); }

	} //namespace pattern

	namespace print {

		//operator precedence (used to decide if parentheses are nessecary in out string)
		constexpr auto infixr_table = std::to_array<std::pair<pattern::PnType, int>>({
			{ Type(FnType::asinh          )     , 0 },	
			{ Type(FnType::acosh          )     , 0 },
			{ Type(FnType::atanh          )     , 0 },	
			{ Type(FnType::asin           )     , 0 },	
			{ Type(FnType::acos           )     , 0 },	
			{ Type(FnType::atan           )     , 0 },	
			{ Type(FnType::sinh           )     , 0 },	
			{ Type(FnType::cosh           )     , 0 },	
			{ Type(FnType::tanh           )     , 0 },	
			{ Type(FnType::sqrt           )     , 0 },
			{ Type(FnType::log            )     , 0 }, 
			{ Type(FnType::exp            )     , 0 },
			{ Type(FnType::sin            )     , 0 },	
			{ Type(FnType::cos            )     , 0 },	
			{ Type(FnType::tan            )     , 0 },	
			{ Type(FnType::abs            )     , 0 },	
			{ Type(FnType::arg            )     , 0 },	
			{ Type(FnType::ln             )     , 0 },	
			{ Type(FnType::re             )     , 0 },	
			{ Type(FnType::im             )     , 0 },	
			{ Type(Node::generic_function )     , 0 },
			{ Type(Node::sum              )     , 2 },
			{ Type(Node::product          )     , 4 },	
			{ Type(FnType::pow            )     , 5 }, //not between other function types -> assumed to be printed with '^'  
			{ Type(Leaf::variable         )     , 6 },
			{ Type(Leaf::complex          )     , 6 }, //may be printed as sum/product itself, then (maybe) has to add parentheses on its own
			{ pattern::PnVariable::tree_match   , 6 },
			{ pattern::PnVariable::value_match  , 6 },
			{ pattern::PnVariable::value_proxy  , 6 },
			});
		constexpr int infixr(pattern::PnType type) { return find_snd(infixr_table, type); }

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
				parentheses = parent_operator_precedence > infixr(Type(Node::sum));
				buffer << val.real();
				add_im_to_stream(val.imag(), Flag::showpos);		
			}
			else if (val.real() != 0.0 && val.imag() == 0.0) {
				parentheses = val.real() < 0.0 && parent_operator_precedence >= infixr(Type(Node::sum));	//leading '-'
				buffer << val.real();
			}
			else if (val.real() == 0.0 && val.imag() != 0.0) {
				parentheses = val.imag() < 0.0 && parent_operator_precedence >= infixr(Type(Node::sum));	//leading '-'	
				parentheses |= parent_operator_precedence > infixr(Type(Node::product));	//*i
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
				throw_if<ParseFailure>(!view.tokens.starts_with(token::open_grouping), view.offset, "expected '(' or the like");
				throw_if<ParseFailure>(!view.tokens.ends_with(token::clse_grouping), view.offset + view.size(), "expected ')' or the like");
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
				throw_if<ParseFailure>(!view.tokens.starts_with(token::open_grouping), view.offset, "expected '(' or the like");
				throw_if<ParseFailure>(!view.tokens.ends_with(token::clse_grouping), view.offset + view.size(), "expected ')' or the like");
				view.remove_prefix(1u);
				view.remove_suffix(1u);
			} while (view.size());
			throw ParseFailure{ view.offset, "run out of characters" };
		} //eval_natural

		double parse_value(const ParseView view)
		{
			double value;
			const auto [ptr, error] = std::from_chars(view.chars, view.chars + view.size(), value);
			throw_if<ParseFailure>(error != std::errc(), view.offset, "value syntax is illformed or value out of bounds");
			throw_if<ParseFailure>(ptr != view.chars + view.size(), std::size_t(view.offset + ptr - view.chars + 1u), "value syntax is illformed");
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
		throw_if<ParseFailure>(view.tokens[first_not_character] != token::open_grouping, first_not_character + view.offset, "illegal character, expected '('");
		throw_if<ParseFailure>(!view.tokens.ends_with(token::clse_grouping), view.size() + view.offset, "poor grouping, expected ')'");
		if (first_not_character == 0u) {
			return Head{ 0u, Head::Type::group };
		}
		else {
			return Head{ first_not_character, Head::Type::function };
		}
	} //find_head_type

	TypedIdx build(Store& store, ParseView input)
	{
		throw_if<ParseFailure>(input.size() == 0u, input.offset, "recieved empty substring");
		Head head = find_head_type(input);
		while (head.type == Head::Type::group) {
			input.remove_prefix(1u);
			input.remove_suffix(1u);
			head = find_head_type(input);
		}
		switch (head.type) {
		case Head::Type::sum: {
			return build_variadic<vdc::SumTraits, TypedIdx>(store, input, head.where, build_negated<Store, TypedIdx>, build);
		} break;
		case Head::Type::negate: {
			input.remove_prefix(1u);  //remove minus sign
			const TypedIdx to_negate = build(store, input);
			return build_negated(store, to_negate);
		} break;
		case Head::Type::product: {
			return build_variadic<vdc::ProductTraits, TypedIdx>(store, input, head.where, build_inverted<Store, TypedIdx>, build);
		} break;
		case Head::Type::power: {
			const auto base_view = input.steal_prefix(head.where);
			input.remove_prefix(1u); //remove hat
			const TypedIdx base = build(store, base_view);
			const TypedIdx expo = build(store, input);
			return TypedIdx(store.insert(FnParams<TypedIdx>{ base, expo}), FnType::pow);
		} break;
		case Head::Type::complex_computable: {
			return build_value<TypedIdx>(store, compute::eval_complex(input));
		} break;
		case Head::Type::natural_computable: { 
			return build_value<TypedIdx>(store, compute::eval_natural(input));
		} break;
		case Head::Type::real_value: {
			return build_value<TypedIdx>(store, compute::parse_value(input));
		} break;
		case Head::Type::imag_value: {
			input.remove_suffix(1u); //remove token::imag_unit
			return build_value<TypedIdx>(store, Complex(0.0, compute::parse_value(input)));
		} break;
		case Head::Type::function: {
			return build_function<TypedIdx>(store, input, head.where, build);
		} break;
		case Head::Type::variable: {
			return TypedIdx(insert_string(store, input.to_string_view()), Leaf::variable);
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
			const char current_operator = input.chars[0u];
			input.remove_prefix(1u); //remove current_operator;
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

	template<typename TypedIdx_T, typename TermStore_T, typename BuildAny>
	[[nodiscard]] TypedIdx_T build_function(TermStore_T& store, ParseView input, const std::size_t open_par, BuildAny build_any)
	{
		using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3u>;

		const auto type = fn::type_of(input.to_string_view(0u, open_par));
		if (type == FnType::COUNT) { //build generic function
			GenericFunction result;
			{//writing name in result
				const auto name = std::string_view(input.chars, open_par);
				if (name.size() > GenericFunction::short_name_max) [[unlikely]] {
					result.name_size = GenericFunction::NameSize::longer;
					result.long_name_idx = insert_string(store, name);
				}
				else {
					result.name_size = GenericFunction::NameSize::small;
					for (std::size_t i = 0u; i < name.size(); i++) {
						result.short_name[i] = name[i]; //maybe go over bound of short_name and into short_name_extension (undefined behavior oh wee!)
					}
				}
			}
			//writing parameters in result
			input.remove_suffix(1u);            //"pow(2,4)" -> "pow(2,4"
			input.remove_prefix(open_par + 1u); //"pow(2,4" ->      "2,4"
			{
				const std::size_t comma = find_first_of_skip_pars(input.tokens, token::comma);
				const auto param_view = input.steal_prefix(comma); //now input starts with comma
				const TypedIdx_T param = build_any(store, param_view);
				result.params_idx = store.insert(TypedIdxSLC_T(param));
			}
			std::size_t last_node_idx = result.params_idx;
			while (input.size()) {
				input.remove_prefix(1u); //erase comma
				const std::size_t comma = find_first_of_skip_pars(input.tokens, token::comma);
				const auto param_view = input.steal_prefix(comma);
				const TypedIdx_T param = build_any(store, param_view);
				last_node_idx = TypedIdxSLC_T::insert_new(store, last_node_idx, param);
			}
			return TypedIdx_T(store.insert(result), Type(Node::generic_function));
		}
		else { //build known function
			FnParams<TypedIdx_T> result{ TypedIdx_T(), TypedIdx_T(), TypedIdx_T(), TypedIdx_T() };
			input.remove_suffix(1u);
			input.remove_prefix(open_par + 1u);	//only arguments are left
			std::size_t comma = find_first_of_skip_pars(input.tokens, token::comma);
			auto param_view = input.steal_prefix(comma);
			for (auto& param : fn::range(result, type)) {
				throw_if<ParseFailure>(param_view.size() == 0u, input.offset, "too few function parameters");
				param = build_any(store, param_view);
				comma = find_first_of_skip_pars(input.tokens, token::comma);
				param_view = input.steal_prefix(comma);
			}
			throw_if<ParseFailure>(param_view.size() > 0u, input.offset, "too many function parameters");
			return TypedIdx_T(store.insert(result), Type(type));
		}
	} //build_function

	namespace pattern {

		PatternParts split(const ParseView input)
		{
			const std::size_t bar = find_first_of_skip_pars(input.tokens, token::bar);
			throw_if<ParseFailure>(count_skip_pars(input.tokens, token::bar) > 1u, input.offset + bar, "expected only this '|', no further ones at top grouping level");
			const std::size_t equals = find_first_of_skip_pars(input.tokens, token::equals);
			throw_if<ParseFailure>(count_skip_pars(input.tokens, token::equals) > 1u, input.offset + equals, "expected only this '=', no further ones at top grouping level");
			throw_if<ParseFailure>(equals == TokenView::npos, input.size() - 1u, "expected '=' at top grouping level");

			if (bar != TokenView::npos) {
				return PatternParts{ input.substr(0u, bar), input.substr(bar + 1u, equals - bar - 1u), input.substr(equals + 1u) };
			}
			else {
				return PatternParts{ input.substr(0u, 0u), input.substr(0u, equals), input.substr(equals + 1u) };
			}
		}

		PnTypedIdx NameLookupTable::insert_instance(PnStore& store, const ParseView input)
		{
			const auto crop = [](std::string_view name) -> std::array<char, 4u> { //this is very stupid, however i dont care.
				if (name.size() >= 3u) {
					return { name[0u], name[1u], name[2u], '\0' };
				}
				if (name.size() == 2u) {
					return { name[0u], name[1u], '\0', '\0' };
				}
				if (name.size() == 1u) {
					return { name[0u], '\0', '\0', '\0' };
				}
				assert(false && "expected positive match_variable name size");
				return {};
			};

			const auto name = input.to_string_view();
			auto var_idx = PnTypedIdx();
			if (const auto iter = std::find_if(this->tree_table.begin(), this->tree_table.end(), [name](const auto& x) { return x.name == name; }); 
				iter != this->tree_table.end()) 
			{
				const std::uint32_t match_data_idx = std::distance(this->tree_table.begin(), iter);
				const TreeMatchVariable var = { match_data_idx, iter->restr, crop(name) };
				var_idx = PnTypedIdx(store.insert(var), PnVariable::tree_match);
				(this->build_lhs ? iter->lhs_instances : iter->rhs_instances).push_back(var_idx);
			}
			if (const auto iter = std::find_if(this->value_table.begin(), this->value_table.end(), [name](const auto& x) { return x.name == name; }); 
				iter != this->value_table.end()) 
			{
				const std::uint32_t match_data_idx = std::distance(this->value_table.begin(), iter);
				const ValueMatchVariable var(match_data_idx, iter->form);
				var_idx = PnTypedIdx(store.insert(var), PnVariable::value_match);
				(this->build_lhs ? iter->lhs_instances : iter->rhs_instances).push_back(var_idx);
			}
			throw_if<ParseFailure>(var_idx == PnTypedIdx(), input.offset, "match variable has not been declared");
			return var_idx;
		} //NameLookupTable::insert_instance

		NameLookupTable parse_declarations(ParseView declarations)
		{
			NameLookupTable result;

			const auto parse_declaration = [&result](ParseView var_view) {
				const std::size_t colon = find_first_of_skip_pars(var_view.tokens, token::colon);
				if (colon != TokenView::npos) {
					const ParseRestriction restr = form_type(var_view.to_string_view(colon + 1u));
					throw_if<ParseFailure>(restr == Restr::unknown, var_view.offset + colon + 1u, "unknown restriction");
					if (restr.is<Form>()) {
						result.value_table.emplace_back(var_view.to_string_view(0, colon), restr.operator bmath::intern::pattern::Form());
					}
					else {
						assert(restr.is<Restriction>());
						result.tree_table.emplace_back(var_view.to_string_view(0, colon), restr.operator bmath::intern::pattern::Restriction());
					}
				}
				else {
					result.tree_table.emplace_back(var_view.to_string_view(), Restriction(Restr::any));
				}
			};

			{
				const std::size_t comma = find_first_of_skip_pars(declarations.tokens, token::comma);
				parse_declaration(declarations.steal_prefix(comma));
			}
			while (declarations.size()) {
				declarations.remove_prefix(1); //erase comma
				const std::size_t comma = find_first_of_skip_pars(declarations.tokens, token::comma);
				parse_declaration(declarations.steal_prefix(comma));
			}
			return result;
		} //parse_declarations

		PnTypedIdx PatternBuildFunction::operator()(PnStore& store, ParseView input)
		{
			throw_if<ParseFailure>(input.size() == 0u, input.offset, "recieved empty substring");
			Head head = find_head_type(input);
			while (head.type == Head::Type::group) {
				input.remove_prefix(1u);
				input.remove_suffix(1u);
				head = find_head_type(input);
			}
			switch (head.type) {
			case Head::Type::sum: {
				return build_variadic<vdc::PnSumTraits, PnTypedIdx>(store, input, head.where, build_negated<PnStore, PnTypedIdx>, *this);
			} break;
			case Head::Type::negate: {
				input.remove_prefix(1u);  //remove minus sign
				const PnTypedIdx to_negate = this->operator()(store, input);
				return build_negated(store, to_negate);
			} break;
			case Head::Type::product: {
				return build_variadic<vdc::PnProductTraits, PnTypedIdx>(store, input, head.where, build_inverted<PnStore, PnTypedIdx>, *this);
			} break;
			case Head::Type::power: {
				const auto base_view = input.steal_prefix(head.where);
				input.remove_prefix(1u); //remove hat
				const PnTypedIdx base = this->operator()(store, base_view);
				const PnTypedIdx expo = this->operator()(store, input);
				return PnTypedIdx(store.insert(FnParams<PnTypedIdx>{ base, expo }), Type(FnType::pow));
			} break;
			case Head::Type::complex_computable: {
				return build_value<PnTypedIdx>(store, compute::eval_complex(input));
			} break;
			case Head::Type::natural_computable: { 
				return build_value<PnTypedIdx>(store, compute::eval_natural(input));
			} break;
			case Head::Type::real_value: {
				return build_value<PnTypedIdx>(store, compute::parse_value(input));
			} break;
			case Head::Type::imag_value: {
				input.remove_suffix(1u); //remove token::imag_unit
				return build_value<PnTypedIdx>(store, Complex(0.0, compute::parse_value(input)));
			} break;
			case Head::Type::function: {
				return build_function<PnTypedIdx>(store, input, head.where, *this);
			} break;
			case Head::Type::variable: {
				if (input.chars[0u] == '\'') {
					throw_if<ParseFailure>(input.chars[input.size() - 1u] != '\'', input.offset + 1u, "found no matching \"'\"");
					return PnTypedIdx(insert_string(store, input.to_string_view(1u, input.size() - 1u)), Type(Leaf::variable));
				}
				else {
					return this->table.insert_instance(store, input);
				}
			} break;
			default: 
				assert(false); 
				return PnTypedIdx();
			}
		} //PatternBuildFunction::operator()

	} //namespace pattern

	namespace print {

		template<typename Store_T, typename TypedIdx_T>
		void append_to_string(const Store_T& store, const TypedIdx_T ref, std::string& str, const int parent_infixr)
		{
			using Type_T = TypedIdx_T::Enum_T;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			const auto [index, type] = ref.split();
			const int own_infixr = infixr(type);
			if (own_infixr <= parent_infixr) {
				str.push_back('(');
			}

			switch (type) {
			case Type_T(Node::sum): {
				const char* seperator = "";
				for (const auto summand : vdc::range(store, index)) {
					str.append(std::exchange(seperator, "+"));
					print::append_to_string(store, summand, str, own_infixr);
				}
			} break;
			case Type_T(Node::product): {
				const char* seperator = "";
				for (const auto factor : vdc::range(store, index)) {
					str.append(std::exchange(seperator, "*"));
					print::append_to_string(store, factor, str, own_infixr);
				}
			} break;
			case Type_T(FnType::pow): {
				const FnParams<TypedIdx_T>& params = store.at(index).fn_params;
				print::append_to_string(store, params[0], str, own_infixr);
				str.push_back('^');
				print::append_to_string(store, params[1], str, own_infixr);
			} break;
			case Type_T(Node::generic_function): {
				const GenericFunction& generic_function = store.at(index).generic_function;
				str.pop_back(); //pop open parenthesis
				fn::append_name(store, generic_function, str);
				str.push_back('(');
				const char* seperator = "";
				for (const auto param : fn::range(store, generic_function)) {
					str.append(std::exchange(seperator, ", "));
					print::append_to_string(store, param, str, own_infixr);
				}
			} break;
			default: {
				assert(type.is<FnType>());
				const FnParams<TypedIdx_T>& params = store.at(index).fn_params;
				str.pop_back(); //pop '('
				str.append(fn::name_of(type.to<FnType>()));
				str.push_back('(');
				const char* seperator = "";
				for (const auto param : fn::range(params, type)) {
					str.append(std::exchange(seperator, ", "));
					print::append_to_string(store, param, str, own_infixr);
				}
			} break;
			case Type_T(Leaf::variable): {
				const Variable& variable = store.at(index).string;
				if constexpr (pattern) {
					str.push_back('\'');
					read(store, index, str);
					str.push_back('\'');
				}
				else {
					read(store, index, str);
				}
			} break;
			case Type_T(Leaf::complex): {
				const Complex& complex = store.at(index).complex;
				append_complex(complex, str, parent_infixr);
			} break;
			case Type_T(pattern::_tree_match): if constexpr (pattern) {
				const pattern::TreeMatchVariable& var = store.at(index).tree_match;
				str.push_back('{');
				str.append(var.name.data());
				if (var.restr != pattern::Restr::any) {
					str.push_back(':');
					str.append(name_of(var.restr));
				}
				str.append(", [");
				str.append(std::to_string(var.match_data_idx));
				str.append("]}");
			} break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				const pattern::ValueMatchVariable& var = store.at(index).value_match;
				str.push_back('{');
				str.append(name_of(var.form));
				str.append(", ");
				print::append_to_string(store, var.match_idx, str);
				str.append(", ");
				print::append_to_string(store, var.copy_idx, str);
				str.push_back('}');
			} break;
			case Type_T(pattern::_value_proxy): if constexpr (pattern) {
				str.push_back('<');
				str.append(std::to_string(index));
				str.push_back('>');
			} break;
			}

			if (own_infixr <= parent_infixr) {
				str.push_back(')');
			}
		} //append_to_string
		template void append_to_string<Store, TypedIdx>(const Store& store, const TypedIdx ref, std::string& str, const int parent_infixr);
		template void append_to_string<pattern::PnStore, pattern::PnTypedIdx>(const pattern::PnStore& store, const pattern::PnTypedIdx ref, std::string& str, const int parent_infixr);

		std::string print::to_pretty_string(const Store& store, const TypedIdx ref, const int parent_infixr)
		{
			std::string str;

			const auto [index, type] = ref.split();
			bool need_parentheses = infixr(type) <= parent_infixr;

			const auto get_negative_real = [&store](const TypedIdx ref) ->OptDouble {
				const auto [index, type] = ref.split();
				if (type == Leaf::complex) {
					const Complex& complex = store.at(index).complex;
					if (complex.real() < 0.0 && complex.imag() == 0.0) {
						return { complex.real() };
					}
				}
				return {};
			}; //get_negative_real

			 //returns base, if ref is actually <base>^(-1)
			const auto get_pow_neg1 = [get_negative_real, &store](const TypedIdx ref) -> std::optional<TypedIdx> {
				const auto [index, type] = ref.split();
				if (type == FnType::pow) {
					const FnParams<TypedIdx>& params = store.at(index).fn_params;
					if (const auto expo = get_negative_real(params[1])) {
						if (*expo == -1.0) {
							return { params[0] };
						}
					}
				}
				return {};
			}; //get_pow_neg1

			struct GetNegativeProductResult { double negative_factor; StupidBufferVector<TypedIdx, 8> other_factors; };
			const auto get_negative_product = [get_negative_real, &store](const TypedIdx ref) -> std::optional<GetNegativeProductResult> {
				const auto [index, type] = ref.split();
				if (type == Node::product) {
					StupidBufferVector<TypedIdx, 8> other_factors;
					double negative_factor;
					bool found_negative_factor = false;
					for (const auto factor : vdc::range(store, index)) {
						if (!found_negative_factor) {
							if (const auto negative_val = get_negative_real(factor)) {
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

			const auto append_product = [get_negative_real, get_pow_neg1, &store, &str](const auto& vec) {
				bool first = true;
				for (const auto elem : vec) {
					if (auto val = get_negative_real(elem); val && first && *val == -1.0) {
						str += "-";
					}
					else if (const auto base = get_pow_neg1(elem)) {
						//str += (first ? "1 / " : " / "); 
						str += (first ? "1/" : "/"); 
						str += print::to_pretty_string(store, *base, infixr(Type(Node::product)));
						first = false;
					}
					else {
						//str += (first ? "" : " * ");
						str += (first ? "" : "*");
						str += print::to_pretty_string(store, elem, infixr(Type(Node::product)));
						first = false;
					}
				}
				assert(!first && "found product with only single factor -1 or zero factors");
			}; //append_product
			
			const auto reverse_elems = [](auto range) {
				StupidBufferVector<TypedIdx, 16> result;
				for (const auto elem : range) {
					result.push_back(elem);
				}
				std::reverse(result.begin(), result.end());
				return result;
			};

			switch (type) {
			case Type(Node::sum): {
				bool first = true;
				for (const auto summand : reverse_elems(vdc::range(store, index))) {
					if (const auto val = get_negative_real(summand)) {
						//str += (first ? "" : " ");
						append_real(*val, str);
					}
					else if (auto product = get_negative_product(summand)) {
						if (product->negative_factor != -1.0) {
							//str += (first ? "" : " ");
							append_real(product->negative_factor, str);
							//str += " * ";
							str += "*";
						}
						else {
							//str +=  (first ? "-" : " -");
							str += "-";
						}
						std::reverse(product->other_factors.begin(), product->other_factors.end());
						append_product(product->other_factors);
					}
					else {
						//str += (first ? "" : " + ");
						str += (first ? "" : "+");
						str += print::to_pretty_string(store, summand, infixr(type));
					}
					first = false;
				}
				assert(!first && "found sum with zero summands");
			} break;
			case Type(Node::product): {
				append_product(reverse_elems(vdc::range(store, index)));
			} break;
			case Type(FnType::pow): {
				const FnParams<TypedIdx>& params = store.at(index).fn_params;
				str += print::to_pretty_string(store, params[0], infixr(type));
				//str += " ^ ";
				str += "^";
				str += print::to_pretty_string(store, params[1], infixr(type));

			} break;
			case Type(Node::generic_function): {
				need_parentheses = false;
				const GenericFunction& generic_function = store.at(index).generic_function;
				fn::append_name(store, generic_function, str);
				str.push_back('(');
				bool first = true;
				for (const auto param : fn::range(store, generic_function)) {
					if (!std::exchange(first, false)) {
						//str += ", ";
						str += ",";
					}
					str += print::to_pretty_string(store, param, infixr(type));
				}
				str.push_back(')');
			} break;
			default: {
				assert(type.is<FnType>());
				const FnParams<TypedIdx>& params = store.at(index).fn_params;
				need_parentheses = false;
				str.append(fn::name_of(type.to<FnType>()));
				str.push_back('(');
				const char* separator = "";
				for (const auto param : fn::range(params, type)) {
					//str += std::exchange(separator, ", ");
					str += std::exchange(separator, ",");
					str += print::to_pretty_string(store, param, infixr(type));
				}
				str.push_back(')');
			} break;
			case Type(Leaf::variable): {
				const Variable& variable = store.at(index).string;
				read(store, index, str);
			} break;
			case Type(Leaf::complex): {
				const Complex& complex = store.at(index).complex;
				append_complex(complex, str, parent_infixr);
			} break;
			}

			if (need_parentheses) {
				return '(' + str + ')';
			}
			else {
				return str;
			}
		} //to_pretty_string

		template<typename Store_T, typename TypedIdx_T>
		void append_memory_row(const Store_T& store, const TypedIdx_T ref, std::vector<std::string>& content)
		{
			using Type_T = TypedIdx_T::Enum_T;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			const auto [index, type] = ref.split();

			auto show_typedidx_col_nodes = [&store, &content, index](std::uint32_t idx, bool show_first) {
				const TypedIdxSLC_T* col = &store.at(idx).index_slc;
				if (show_first) {
					content[idx].append("(SLC node part of index " + std::to_string(index) + ')');
				}
				while (col->next_idx != TypedIdxSLC_T::null_index) {
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
			case Type_T(Node::sum): {
				current_str.append("sum        : {");
				bool first = true;
				for (const auto elem : vdc::range(store, index)) {
					if (!std::exchange(first, false)) {
						current_str.append(", ");
					}
					current_str.append(std::to_string(elem.get_index()));
					print::append_memory_row(store, elem, content);
				}
				current_str.push_back('}');
				show_typedidx_col_nodes(index, false);
			} break;
			case Type_T(Node::product): {
				current_str.append("product    : {");
				bool first = true;
				for (const auto elem : vdc::range(store, index)) {
					if (!std::exchange(first, false)) {
						current_str.append(", ");
					}
					current_str.append(std::to_string(elem.get_index()));
					print::append_memory_row(store, elem, content);
				}
				current_str.push_back('}');
				show_typedidx_col_nodes(index, false);
			} break;
			case Type_T(Node::generic_function): {
				const GenericFunction& generic_function = store.at(index).generic_function;
				current_str.append("function?  : {");
				bool first = true;
				for (const auto param : fn::range(store, generic_function)) {
					if (!std::exchange(first, false)) {
						current_str.append(", ");
					}
					current_str.append(std::to_string(param.get_index()));
					print::append_memory_row(store, param, content);
				}
				current_str.push_back('}');
				show_typedidx_col_nodes(generic_function.params_idx, true);
				if (generic_function.name_size == GenericFunction::NameSize::longer) {
					show_string_nodes(generic_function.long_name_idx, true);
				}
			} break;
			default: {
				assert(type.is<FnType>());
				const FnParams<TypedIdx_T>& params = store.at(index).fn_params;
				current_str.append("function   : {");
				bool first = true;
				for (const auto param : fn::range(params, type)) {
					if (!std::exchange(first, false)) {
						current_str.append(", ");
					}
					current_str.append(std::to_string(param.get_index()));
					print::append_memory_row(store, param, content);
				}
				current_str.push_back('}');
			} break;
			case Type_T(Leaf::variable): {
				current_str.append("variable   : ");
				show_string_nodes(index, false);
			} break;
			case Type_T(Leaf::complex): {
				const Complex& complex = store.at(index).complex;
				current_str.append("value      : ");
			} break;
			case Type_T(pattern::_tree_match): if constexpr (pattern) {
				current_str.append("tree_match : ");
			} break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				current_str.append("value_match: ");
			} break;
			case Type_T(pattern::_value_proxy): 
				return;
			}

			//append name of subterm to line
			current_str.append(std::max(0, 35 - (int)current_str.size()), ' ');
			append_to_string(store, ref, current_str, 0);
		} //append_memory_row

		template<typename Store_T, typename TypedIdx_T>
		std::string to_memory_layout(const Store_T& store, const TypedIdx_T head)
		{
			std::vector<std::string> elements;
			elements.reserve(store.size() + 1);
			for (std::size_t i = 0; i < store.size(); i++) {
				elements.push_back("");
				if (i < 10) { //please std::format, i need you :(
					elements[i] = " ";
				}
				elements[i].append(std::to_string(i));
				elements[i].append(" | ");
			}
			print::append_memory_row(store, head, elements);

			for (const auto i : store.free_slots()) {
				elements[i].append("-----free slot-----");
			}		

			for (auto& elem : elements) {
				elem.push_back('\n');
			}
			std::string result("   | head at index: " + std::to_string(head.get_index()) + '\n');
			result.reserve(store.size() * 15);
			for (auto& elem : elements) {
				result.append(elem);
			}
			return result;
		} //to_memory_layout
		template std::string to_memory_layout<Store, TypedIdx>(const Store& store, const TypedIdx head);
		template std::string to_memory_layout<pattern::PnStore, pattern::PnTypedIdx>(const pattern::PnStore& store, const pattern::PnTypedIdx head);

	} //namespace print

} //namespace bmath::intern
