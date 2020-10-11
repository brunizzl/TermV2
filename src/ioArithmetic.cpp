
#include <charconv>
#include <algorithm>

#include "ioArithmetic.hpp"
#include "termUtility.hpp"

namespace bmath::intern {

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////local definitions//////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	//utility for both KnownFunction and GenericFunction
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
		constexpr FnType type_of(const std::string_view name) noexcept { return search_fst(name_table, name, FnType::UNKNOWN); }

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
			static constexpr Type type_name = Type::sum;
			static constexpr char operator_char = '+';
			static constexpr char inverse_operator_char = '-';
			static constexpr Token operator_token = token::sum;
		};

		struct SumTraits : public BasicSumTraits { using Object_T = Sum; };
		struct PnSumTraits : public BasicSumTraits { using Object_T = pattern::PnSum; };



		struct BasicProductTraits
		{
			static constexpr Type type_name = Type::product;
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

	template<typename TypedIdx_T, typename TermStore_T>
	[[nodiscard]] TypedIdx_T build_value(TermStore_T& store, double re, double im = 0.0)
	{
		return TypedIdx_T(store.insert(Complex{ std::complex<double>(re, im) }), Type::complex);
	}

	namespace pattern {

		constexpr auto form_name_table = std::to_array<std::pair<Restriction, std::string_view>>({
			{ Type::sum                 , "sum"           },
			{ Type::product             , "product"       },
			{ Type::variable            , "variable"      },
			{ Type::complex             , "complex"       },
			{ Form::function     , "function"      },
			{ Form::natural      , "natural"       },
			{ Form::integer      , "integer"       },
			{ Form::real         , "real"          },
			{ Form::not_minus_one, "not_minus_one" },
			{ Form::negative     , "negative"      },
			{ Form::not_negative , "not_negative"  },
			{ Form::positive     , "positive"      },
			{ Form::not_positive , "not_positive"  },
			{ Form::any          , "any"           },
		});

		constexpr std::string_view form_name(const Restriction r) noexcept { return find_snd(form_name_table, r); }
		constexpr Restriction form_type(const std::string_view s) noexcept { return search_fst(form_name_table, s, Restriction(Form::UNKNOWN)); }

	} //namespace pattern

	namespace print {

		enum class PrintExtras { pow, COUNT };
		using PrintType = SumEnum<PrintExtras, pattern::PnSpecial, Type>;

		constexpr PrintType to_print_type(pattern::PnType t) { return PrintType(unsigned(t)); }
		static_assert(unsigned(pattern::PnType(Type::sum)) == unsigned(PrintType(Type::sum)), "to_print_type invalid");
		static_assert(unsigned(pattern::PnType(pattern::PnSpecial::match_variable)) == unsigned(PrintType(pattern::PnSpecial::match_variable)), "to_print_type invalid");

		//operator precedence (used to decide if parentheses are nessecary in out string)
		constexpr auto infixr_table = std::to_array<std::pair<PrintType, int>>({
			{ Type::known_function,               0 },
			{ Type::generic_function,             0 },
			{ Type::sum,                          2	},
			{ Type::product,                      4 },
			{ PrintExtras::pow,                   5 },
			{ Type::variable,                     6 },
			{ Type::complex,                      6 },//may be printed as sum/product itself, then (maybe) has to add parentheses on its own
			{ pattern::PnSpecial::match_variable, 6 },
			});
		constexpr int infixr(PrintType type) { return find_snd(infixr_table, type); }

	} //namespace print

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////exported in header/////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	std::size_t find_first_not_arithmetic(const TokenView view)
	{
		using namespace token;
		const char allowed_tokens[] = { character, number, open_grouping, clse_grouping, 
			unary_minus, sum, product, comma, hat, imag_unit, '\0' }; //'\0' only as end symbol for allowed_tokens, not as part of aritmetic symbols
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
		if ((op = find_first_of_skip_pars(token_view, token::hat)) != TokenView::npos) {
			return Head{ op, Head::Type::power };
		}
		if (token_view.find_first_not_of(token::number) == TokenView::npos) {
			return Head{ 0, Head::Type::value };
		}
		if ((op = token_view.find_first_not_of(token::character)) == TokenView::npos) {
			return Head{ 0, Head::Type::variable };
		}
		if (token_view.size() == 1u && token_view[0u] == token::imag_unit) {
			return Head{ 0, Head::Type::imag_unit };
		}
		throw_if<ParseFailure>(token_view[op] != token::open_grouping, op + offset, "illegal character, expected '('");
		throw_if<ParseFailure>(!token_view.ends_with(token::clse_grouping), token_view.length() + offset, "poor grouping, expected ')'");
		if (op == 0) {
			return Head{ 0, Head::Type::group };
		}
		else {
			return Head{ op, Head::Type::function };
		}
	} //find_head_type

	TypedIdx build(Store& store, ParseView input)
	{
		throw_if<ParseFailure>(input.size() == 0, input.offset, "recieved empty substring");
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
					const TypedIdx minus_1 = build_value<TypedIdx>(store, -1.0);
					return TypedIdx(store.insert(Product({ minus_1, to_invert })), Type::product);
				},
				build);
		} break;
		case Head::Type::negate: {
			input.remove_prefix(1);  //remove minus sign
			const TypedIdx to_negate = build(store, input);
			const TypedIdx minus_1 = build_value<TypedIdx>(store, -1.0);
			return TypedIdx(store.insert(Product({ to_negate, minus_1 })), Type::product);
		} break;
		case Head::Type::product: {
			return build_variadic<vdc::ProductTraits, TypedIdx>(store, input, head.where, 
				[](Store& store, TypedIdx to_invert) {
					const TypedIdx minus_1 = build_value<TypedIdx>(store, -1.0);
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
			throw_if<ParseFailure>(error == std::errc::invalid_argument, input.offset, "value syntax is illformed");
			throw_if<ParseFailure>(ptr != input.chars + input.size(), std::size_t(input.offset + ptr - input.chars + 1), "value syntax is illformed");
			return build_value<TypedIdx>(store, val);
		} break;
		case Head::Type::function: {
			return build_function<TypedIdx>(store, input, head.where, build);
		} break;
		case Head::Type::variable: {
			return TypedIdx(insert_string(store, input.to_string_view()), Type::variable);
		} break;
		case Head::Type::imag_unit: {
			return build_value<TypedIdx>(store, 0.0, 1.0);
		} break;
		default: 
			assert(false); 
			return TypedIdx();
		}
	} //build

	template<typename VariadicTraits, typename TypedIdx_T, typename TermStore_T, typename BuildInverse, typename BuildAny>
	TypedIdx_T build_variadic(TermStore_T& store, ParseView input, std::size_t op_idx, const BuildInverse build_inverse, const BuildAny build_any)
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

	template<typename TypedIdx_T, typename TermStore_T, typename BuildAny>
	[[nodiscard]] TypedIdx_T build_function(TermStore_T& store, ParseView input, const std::size_t open_par, const BuildAny build_any)
	{
		using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;

		const auto type = fn::type_of(input.to_string_view(0u, open_par));
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
				const std::size_t comma = find_first_of_skip_pars(input.tokens, token::comma);
				const auto param_view = input.steal_prefix(comma); //now input starts with comma
				const TypedIdx_T param = build_any(store, param_view);
				result.params_idx = store.insert(TypedIdxSLC_T(param));
			}
			std::size_t last_node_idx = result.params_idx;
			while (input.size()) {
				input.remove_prefix(1); //erase comma
				const std::size_t comma = find_first_of_skip_pars(input.tokens, token::comma);
				const auto param_view = input.steal_prefix(comma);
				const TypedIdx_T param = build_any(store, param_view);
				last_node_idx = TypedIdxSLC_T::insert_new(store, last_node_idx, param);
			}
			return TypedIdx_T(store.insert(result), Type::generic_function);
		}
		else { //build known function
			BasicKnownFunction<TypedIdx_T> result{ FnType(type), TypedIdx_T(), TypedIdx_T(), TypedIdx_T() };
			input.remove_suffix(1u);
			input.remove_prefix(open_par + 1u);	//only arguments are left
			std::size_t comma = find_first_of_skip_pars(input.tokens, token::comma);
			auto param_view = input.steal_prefix(comma);
			for (auto& param : fn::range(result)) {
				throw_if<ParseFailure>(param_view.size() == 0u, input.offset, "too few function parameters");
				param = build_any(store, param_view);
				comma = find_first_of_skip_pars(input.tokens, token::comma);
				param_view = input.steal_prefix(comma);
			}
			throw_if<ParseFailure>(param_view.size() > 0u, input.offset, "too many function parameters");
			return TypedIdx_T(store.insert(result), Type::known_function);
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

		std::vector<NameLookup> parse_declarations(ParseView declarations)
		{
			const auto parse_declaration = [](ParseView var_view) -> NameLookup {
				const std::size_t colon = find_first_of_skip_pars(var_view.tokens, token::colon);
				if (colon != TokenView::npos) {
					const Restriction restr = form_type(var_view.to_string_view(colon + 1u));
					throw_if<ParseFailure>(restr == Form::UNKNOWN, var_view.offset + colon + 1u, "unknown restriction");
					return { var_view.to_string_view(0, colon), restr };
				}
				else {
					return { var_view.to_string_view(), Form::any };
				}
			};

			std::vector<NameLookup> result;
			result.reserve(count_skip_pars(declarations.tokens, token::comma) + 1u);
			{
				const std::size_t comma = find_first_of_skip_pars(declarations.tokens, token::comma);
				result.push_back(parse_declaration(declarations.steal_prefix(comma)));
			}
			while (declarations.size()) {
				declarations.remove_prefix(1); //erase comma
				const std::size_t comma = find_first_of_skip_pars(declarations.tokens, token::comma);
				result.push_back(parse_declaration(declarations.steal_prefix(comma)));
			}
			return result;
		}

		PnTypedIdx PatternBuildFunction::operator()(PnStore& store, ParseView input) const
		{
			const auto match_var_name = [](std::string_view name) -> std::array<char, 4u> {
				if (name.size() >= 3u) {
					return { name[0u], name[1u], name[2u], '\0' };
				}
				if (name.size() == 2u) {
					return { name[0u], name[1u], '\0', '\0' };
				}
				if (name.size() == 1u) {
					return { name[0u], '\0', '\0', '\0' };
				}
				assert(false && "expected positive name size");
				return {};
			};

			throw_if<ParseFailure>(input.size() == 0u, input.offset, "recieved empty substring");
			Head head = find_head_type(input.tokens, input.offset);
			while (head.type == Head::Type::group) {
				input.remove_prefix(1);
				input.remove_suffix(1);
				head = find_head_type(input.tokens, input.offset);
			}
			switch (head.type) {
			case Head::Type::sum: {
				return build_variadic<vdc::PnSumTraits, PnTypedIdx>(store, input, head.where, 
					[](PnStore& store, PnTypedIdx to_invert) {
						const PnTypedIdx minus_1 = build_value<PnTypedIdx>(store, -1.0);
						return PnTypedIdx(store.insert(PnProduct({ minus_1, to_invert })), Type::product);
					},
					*this);
			} break;
			case Head::Type::negate: {
				input.remove_prefix(1);  //remove minus sign
				const PnTypedIdx to_negate = this->operator()(store, input);
				const PnTypedIdx minus_1 = build_value<PnTypedIdx>(store, -1.0);
				return PnTypedIdx(store.insert(PnProduct({ to_negate, minus_1 })), Type::product);
			} break;
			case Head::Type::product: {
				return build_variadic<vdc::PnProductTraits, PnTypedIdx>(store, input, head.where, 
					[](PnStore& store, PnTypedIdx to_invert) {
						const PnTypedIdx minus_1 = build_value<PnTypedIdx>(store, -1.0);
						return PnTypedIdx(store.insert(
							PnKnownFunction{ FnType::pow, to_invert, minus_1, PnTypedIdx() }), Type::known_function);
					},
					*this);
			} break;
			case Head::Type::power: {
				const auto base_view = input.steal_prefix(head.where);
				input.remove_prefix(1);
				const PnTypedIdx base = this->operator()(store, base_view);
				const PnTypedIdx expo = this->operator()(store, input);
				return PnTypedIdx(store.insert(PnKnownFunction{ FnType::pow, base, expo, PnTypedIdx() }), Type::known_function);
			} break;
			case Head::Type::value: {
				double val;
				const auto [ptr, error] = std::from_chars(input.chars, input.chars + input.size(), val);
				throw_if<ParseFailure>(error == std::errc::invalid_argument, input.offset, "value syntax is illformed");
				throw_if<ParseFailure>(ptr != input.chars + input.size(), std::size_t(input.offset + ptr - input.chars + 1), "value syntax is illformed");
				return build_value<PnTypedIdx>(store, val);
			} break;
			case Head::Type::function: {
				return build_function<PnTypedIdx>(store, input, head.where, *this);
			} break;
			case Head::Type::variable: {
				if (input.chars[0] == '\'') {
					throw_if<ParseFailure>(input.chars[input.size() - 1u] != '\'', input.offset + 1u, "found no matching \"'\"");
					return PnTypedIdx(insert_string(store, input.to_string_view(1u, input.size() - 1u)), Type::variable);
				}
				else {
					const auto name_it = std::find_if(this->name_map.begin(), this->name_map.end(),
						[search_name = input.to_string_view()](const auto& x) { return x.name == search_name; });
					throw_if<ParseFailure>(name_it == this->name_map.end(), input.offset, "variable was not declared");
					const auto name = match_var_name(input.to_string_view());
					const MatchVariable var = { TypedIdx(), std::uint32_t(name_it - this->name_map.begin()), name_it->restr, name };
					return PnTypedIdx(store.insert(var), PnSpecial::match_variable);
				}
			} break;
			case Head::Type::imag_unit: {
				return build_value<PnTypedIdx>(store, 0.0, 1.0);
			} break;
			default: 
				assert(false); 
				return PnTypedIdx();
			}
		}

	} //namespace pattern

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

		template<typename Store_T, typename TypedIdx_T>
		void append_to_string(const Store_T& store, const TypedIdx_T ref, std::string& str, const int parent_infixr)
		{
			using Type_T = TypedIdx_T::Enum_T;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			const auto [index, type] = ref.split();
			const int own_infixr = infixr(to_print_type(type));
			if (own_infixr <= parent_infixr) {
				str.push_back('(');
			}

			switch (type) {
			case Type_T(Type::sum): {
				bool first = true;
				for (const auto summand : vdc::range(store, index)) {
					if (!std::exchange(first, false)) {
						str.push_back('+');
					}
					append_to_string(store, summand, str, own_infixr);
				}
			} break;
			case Type_T(Type::product): {
				bool first = true;
				for (const auto factor : vdc::range(store, index)) {
					if (!std::exchange(first, false)) {
						str.push_back('*');
					}
					append_to_string(store, factor, str, own_infixr);
				}
			} break;
			case Type_T(Type::known_function): {
				const BasicKnownFunction<TypedIdx_T>& known_function = store.at(index).known_function;
				str.pop_back(); //pop '('
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
			case Type_T(Type::generic_function): {
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
			case Type_T(Type::variable): {
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
			case Type_T(Type::complex): {
				const Complex& complex = store.at(index).complex;
				append_complex(complex, str, parent_infixr);
			} break;
			case Type_T(pattern::_match_variable): if constexpr (pattern) {
				const pattern::MatchVariable& var = store.at(index).match_variable;
				str.append(var.name.data());
				if (var.restr != pattern::Form::any) {
					str.push_back(':');
					str.append(form_name(var.restr));
				}
			} break;
			default: assert(false); //if this assert hits, the switch above needs more cases.
			}

			if (own_infixr <= parent_infixr) {
				str.push_back(')');
			}
		} //append_to_string
		template void append_to_string<Store, TypedIdx>(const Store& store, const TypedIdx ref, std::string& str, const int parent_infixr);
		template void append_to_string<pattern::PnStore, pattern::PnTypedIdx>(const pattern::PnStore& store, const pattern::PnTypedIdx ref, std::string& str, const int parent_infixr);

		std::string to_pretty_string(const Store& store, const TypedIdx ref, const int parent_infixr)
		{
			const auto get_negative_real = [](const Store& store, const TypedIdx ref) ->std::optional<double> {
				const auto [index, type] = ref.split();
				if (type == Type::complex) {
					const Complex& complex = store.at(index).complex;
					if (complex.real() < 0.0 && complex.imag() == 0.0) {
						return { complex.real() };
					}
				}
				return {};
			}; //get_negative_real

			 //returns base, if ref is actually <base>^(-1)
			const auto get_pow_neg1 = [get_negative_real](const Store & store, const TypedIdx ref) -> std::optional<TypedIdx> {
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
			}; //get_pow_neg1

			struct GetNegativeProductResult { double negative_factor; std::vector<TypedIdx> other_factors; };
			const auto get_negative_product = [get_negative_real](const Store& store, const TypedIdx ref) -> std::optional<GetNegativeProductResult> {
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
			}; //get_negative_product

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
				const Variable& variable = store.at(index).string;
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

		template<typename Store_T, typename TypedIdx_T>
		void to_memory_layout(const Store_T& store, const TypedIdx_T ref, std::vector<std::string>& content)
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
			case Type_T(Type::sum): {
				current_str.append("sum       : {");
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
			case Type_T(Type::product): {
				current_str.append("product   : {");
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
			case Type_T(Type::known_function): {
				const BasicKnownFunction<TypedIdx_T>& known_function = store.at(index).known_function;
				current_str.append("function  : {");
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
			case Type_T(Type::generic_function): {
				const GenericFunction& generic_function = store.at(index).generic_function;
				current_str.append("function? : {");
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
			case Type_T(Type::variable): {
				current_str.append("variable  : ");
				show_string_nodes(index, false);
			} break;
			case Type_T(Type::complex): {
				const Complex& complex = store.at(index).complex;
				current_str.append("value     : ");
			} break;
			case Type_T(pattern::_match_variable): if constexpr (pattern) {
				current_str.append("match_var : ");
			} break;
			default: assert(false); //if this assert hits, the switch above needs more cases.
			}

			//append name of subterm to line
			current_str.append(std::max(0, 35 - (int)current_str.size()), ' ');
			append_to_string(store, ref, current_str, 0);
		} //to_memory_layout

		template<typename Store_T, typename TypedIdx_T>
		std::string show_memory_layout(const Store_T& store, const TypedIdx_T head)
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
			to_memory_layout(store, head, elements);

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
		} //show_memory_layout
		template std::string show_memory_layout<Store, TypedIdx>(const Store& store, const TypedIdx head);
		template std::string show_memory_layout<pattern::PnStore, pattern::PnTypedIdx>(const pattern::PnStore& store, const pattern::PnTypedIdx head);

	} //namespace print

} //namespace bmath::intern
