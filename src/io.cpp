#include "io.hpp"

#include <charconv>
#include <limits>
#include <concepts>

#include "utility/vector.hpp"
#include "ioArithmetic.hpp"

namespace simp {

	Head find_head_type(const bmath::intern::ParseView view)
	{
		using namespace bmath;
		using namespace bmath::intern;

		if (view.tokens.starts_with('\\')) {
			const std::size_t dot_pos = find_first_of_skip_pars(view.tokens, token::dot);
			if (dot_pos == TokenView::npos) [[unlikely]] throw ParseFailure{ view.offset + dot_pos, "found no dot in lambda" };
			return Head{ dot_pos, Head::Type::lambda };
		}
		if (const std::size_t or_pos = find_first_of_skip_pars(view.tokens, token::or_); or_pos != TokenView::npos) {
			return Head{ or_pos, Head::Type::or_ };
		}
		if (const std::size_t and_pos = find_first_of_skip_pars(view.tokens, token::and_); and_pos != TokenView::npos) {
			return Head{ and_pos, Head::Type::and_ };
		}
		if (view.tokens.starts_with(token::bang)) {
			return Head{ 0u, Head::Type::not_ };
		}
		if (const std::size_t relation_pos = find_first_of_skip_pars(view.tokens, token::relation); relation_pos != TokenView::npos) {
			const Head::Type type = [&]() {
				if (view.tokens[relation_pos + 1u] != token::relation) {
					return view.chars[relation_pos] == '>' ? Head::Type::greater : Head::Type::smaller;
				}
				switch (view.chars[relation_pos]) {
				case '=': return Head::Type::equals;
				case '!': return Head::Type::not_equals;
				case '<': return Head::Type::smaller_eq;
				case '>': return Head::Type::greater_eq;
				default:
					assert(false);
					return Head::Type::equals;
				}
			}();
			return Head{ relation_pos, type };
		}
		if (const std::size_t sum_pos = find_first_of_skip_pars(view.tokens, token::sum); sum_pos != TokenView::npos) {
			return Head{ sum_pos, view.chars[sum_pos] == '+' ? Head::Type::plus : Head::Type::minus };
		}
		if (view.tokens.starts_with(token::unary_minus)) {
			return Head{ 0u, Head::Type::negate };
		}
		if (const std::size_t product_pos = find_first_of_skip_pars(view.tokens, token::product); product_pos != TokenView::npos) {
			return Head{ product_pos, view.chars[product_pos] == '*' ? Head::Type::times : Head::Type::divided };
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
			return Head{ 0u, Head::Type::symbol };
		}
		if (view.tokens[first_not_character] != token::open_grouping) [[unlikely]] throw ParseFailure{ first_not_character + view.offset, "illegal character, expected '('" };
		if (!view.tokens.ends_with(token::clse_grouping)) [[unlikely]] throw ParseFailure{ view.size() + view.offset, "poor grouping, expected ')'" };
		if (first_not_character == 0u && find_closed_par(0, view.tokens) + 1u == view.tokens.size()) {
			return Head{ 0u, Head::Type::group };
		}
		else { //can be called immediatly -> find opening paren from back
			return Head{ find_open_par(view.size() - 1u, view.tokens), Head::Type::call };
		}
	} //find_head_type

	namespace name_lookup {

		//returns false if name represents multi match or value match
		bool mundane_name(const std::string_view name) noexcept
		{
			return name.find_first_of('.') == std::string_view::npos &&
				name.find_first_of('$') == std::string_view::npos;
		}
		
		TypedIdx to_keyword(std::string_view name)
		{
			constexpr auto keywords = std::to_array<std::pair<std::string_view, TypedIdx>>({
				{ "true", TypedIdx(1, MathType::boolean) },
				{ "false", TypedIdx(0, MathType::boolean) },
			});
			const auto iter = std::find_if(keywords.begin(), keywords.end(), [name](const auto& p) { return name == p.first; });
			return iter != keywords.end() ? iter->second : TypedIdx();
		}

		TypedIdx find_name_in_infos(const std::vector<NameInfo>& infos, const std::string_view name) 
		{
			const auto iter = std::find_if(infos.rbegin(), infos.rend(),
				[name](const NameInfo& i) { return i.name == name; });
			return iter != infos.rend() ?
				iter->value :
				TypedIdx();
		}

		TypedIdx build_symbol(Store& store, const LiteralInfos& infos, bmath::intern::ParseView view)
		{
			const std::string_view name = view.to_string_view();
			if (!mundane_name(name)) [[unlikely]] {
				throw bmath::ParseFailure{ view.offset, "ellipses or the dollar symbol are only expected when building a pattern" };
			}
			if (const TypedIdx res = to_keyword(name); res != TypedIdx()) {
				return res;
			}
			if (const TypedIdx res = find_name_in_infos(infos.lambda_params, name); res != TypedIdx()) {
				return res;
			}
			if (const fn::Buildin type = fn::type_of(name); type != fn::Buildin(fn::Buildin::COUNT)) {
				return fn::to_typed_idx(type);
			}
			return TypedIdx(Symbol::build(store, name), MathType::symbol);
		}

		TypedIdx build_symbol(Store& store, PatternInfos& infos, bmath::intern::ParseView view)
		{
			std::string_view name = view.to_string_view();
			if (const TypedIdx res = to_keyword(name); res != TypedIdx()) {
				return res;
			}
			if (const TypedIdx res = find_name_in_infos(infos.lambda_params, name); res != TypedIdx()) {
				return res;
			}
			if (const TypedIdx res = find_name_in_infos(infos.match_variables, name); res != TypedIdx()) {
				return res;
			}
			if (const fn::Buildin type = fn::type_of(name); type != fn::Buildin(fn::Buildin::COUNT)) {
				return TypedIdx(static_cast<unsigned>(type), MathType::buildin);
			}
			if (name.starts_with('\'') && name.ends_with('\'')) {
				name.remove_prefix(1u);
				name.remove_suffix(1u);
				if (!mundane_name(name)) [[unlikely]] {
					throw bmath::ParseFailure{ view.offset, "please decide: do you want a match variable or a symbol?" };
				}
				if (const TypedIdx res = to_keyword(name); res != TypedIdx()) [[unlikely]] {
					throw bmath::ParseFailure{ view.offset, "no sneaking in keywords, my dude." };
				}
				return TypedIdx(Symbol::build(store, name), MathType::symbol);
			}
			const auto add_match_variable = [&](const Type type) {
				const std::size_t id = std::count_if(infos.match_variables.begin(), infos.match_variables.end(),
					[type](const NameInfo& i) { return i.value.get_type() == type; });
				const TypedIdx result = TypedIdx(id, type);
				infos.match_variables.emplace_back(name, result);		
				return result;
			};
			if (name.starts_with('$')) {
				return add_match_variable(ValueMatch::weak);
			}	
			if (name.ends_with('.')) {				
				return add_match_variable(MultiMatch::fst);
			}
			return add_match_variable(SingleMatch::weak);
		}

		unsigned add_lambda_params(std::vector<NameInfo>& lambda_params, bmath::intern::ParseView params_view)
		{
			using namespace bmath::intern;
			if (!params_view.tokens.starts_with(token::backslash) || params_view.tokens.find_last_not_of(token::character) != 0u) [[unlikely]]
				throw bmath::ParseFailure{ params_view.offset, "this is a weird looking lambda parameter declaration" };
			unsigned param_count = 0u;
			while (params_view.size()) {
				params_view.remove_prefix(1u); //remove previous space (or '\\' for first parameter)
				const std::size_t space = params_view.to_string_view().find_first_of(' ');
				const std::string_view name = params_view.steal_prefix(space).to_string_view();
				if (!mundane_name(name)) [[unlikely]] 
					throw bmath::ParseFailure{ params_view.offset, "we name lambda parameters less exiting around here" };
				lambda_params.push_back({ name, TypedIdx(lambda_params.size(), MathType::lambda_param) });
				param_count++;
			}
			return param_count;
		}

	} //namespace name_lookup

	double parse_value(const bmath::intern::ParseView view)
	{
		double value;
		const auto [ptr, error] = std::from_chars(view.chars, view.chars + view.size(), value);
		if (error != std::errc()) [[unlikely]] throw bmath::ParseFailure{ view.offset, "value syntax is illformed or value out of bounds" };
		if (ptr != view.chars + view.size()) [[unlikely]] throw bmath::ParseFailure{ std::size_t(view.offset + ptr - view.chars + 1u), "value syntax is illformed" };
		return value;
	} //parse_value

	template<name_lookup::InfoLike Infos>
	TypedIdx build(Store& store, Infos& infos, bmath::intern::ParseView view)
	{
		using namespace bmath;
		using namespace bmath::intern;

		if (view.size() == 0u) [[unlikely]] throw ParseFailure{ view.offset, "recieved empty substring" };
		Head head = simp::find_head_type(view);
		while (head.type == Head::Type::group) {
			view.remove_prefix(1u);
			view.remove_suffix(1u);
			head = simp::find_head_type(view);
		}
		const auto to_buildin_call = [&](const std::size_t operator_length, const fn::Buildin type) {
			const TypedIdx fst = simp::build(store, infos, view.substr(0, head.where));
			const TypedIdx snd = simp::build(store, infos, view.substr(head.where + operator_length));
			const std::size_t res_index = store.allocate_one();
			store.at(res_index) = Call{ fn::to_typed_idx(type), fst, snd };
			return TypedIdx(res_index, MathType::call);
		};
		const auto to_inverse_buildin_call = [&](const fn::Buildin type, auto invert_term) {
			const TypedIdx fst = simp::build(store, infos, view.substr(0, head.where));
			const TypedIdx snd_uninverted = build(store, infos, view.substr(head.where + 1));
			const TypedIdx snd = invert_term(store, snd_uninverted);
			const std::size_t res_index = store.allocate_one();
			store.at(res_index) = Call{ fn::to_typed_idx(type), fst, snd };
			return TypedIdx(res_index, MathType::call);
		};
		switch (head.type) {
		case Head::Type::or_:        return to_buildin_call(2, fn::Comm::or_);
		case Head::Type::and_:       return to_buildin_call(2, fn::Comm::and_);
		case Head::Type::equals:     return to_buildin_call(2, fn::FixedArity::eq);
		case Head::Type::not_equals: return to_buildin_call(2, fn::FixedArity::neq);
		case Head::Type::greater:    return to_buildin_call(2, fn::FixedArity::greater);
		case Head::Type::smaller:    return to_buildin_call(2, fn::FixedArity::smaller);
		case Head::Type::greater_eq: return to_buildin_call(2, fn::FixedArity::greater_eq);
		case Head::Type::smaller_eq: return to_buildin_call(2, fn::FixedArity::smaller_eq);
		case Head::Type::plus:       return to_buildin_call(1, fn::Comm::sum);
		case Head::Type::minus:      return to_inverse_buildin_call(fn::Comm::sum, build_negated<Store>);
		case Head::Type::times:      return to_buildin_call(1, fn::Comm::product);
		case Head::Type::divided:    return to_inverse_buildin_call(fn::Comm::product, build_inverted<Store>);
		case Head::Type::power:      return to_buildin_call(1, fn::FixedArity::pow); 
		case Head::Type::not_: {
			view.remove_prefix(1u);  //remove '!'
			const TypedIdx to_negate = simp::build(store, infos, view);
			const std::size_t res_index = store.allocate_one();
			store.at(res_index) = Call{ fn::to_typed_idx(fn::FixedArity::not_), to_negate };
			return TypedIdx(res_index, MathType::call);
		} break;
		case Head::Type::negate: {
			view.remove_prefix(1u);  //remove minus sign
			const TypedIdx to_negate = simp::build(store, infos, view);
			return build_negated(store, to_negate);
		} break;
		case Head::Type::real_value: {
			return simp::build_value(store, parse_value(view));
		} break;
		case Head::Type::imag_value: {
			view.remove_suffix(1u); //remove token::imag_unit
			return simp::build_value(store, Complex(0.0, parse_value(view)));
		} break;
		case Head::Type::symbol: {
			return name_lookup::build_symbol(store, infos, view);
		} break;
		case Head::Type::call: {
			bmath::intern::StupidBufferVector<TypedIdx, 12> subterms;
			subterms.push_back(simp::build(store, infos, view.steal_prefix(head.where)));
			view.remove_prefix(1u); //remove '('
			view.remove_suffix(1u); //remove ')'
			if (view.size()) [[likely]] { //else no parameters at all
				const std::size_t comma = find_first_of_skip_pars(view.tokens, token::comma);
				const auto param_view = view.steal_prefix(comma); //now input starts with comma
				subterms.push_back(simp::build(store, infos, param_view));
			}
			while (view.size()) {
				view.remove_prefix(1u); //erase leading comma
				const std::size_t comma = find_first_of_skip_pars(view.tokens, token::comma);
				const auto param_view = view.steal_prefix(comma);
				subterms.push_back(simp::build(store, infos, param_view));
			}
			return TypedIdx(Call::build(store, subterms), MathType::call);
		} break;
		case Head::Type::lambda: {
			const unsigned param_count = name_lookup::add_lambda_params(infos.lambda_params, view.steal_prefix(head.where));
			assert(view.tokens.starts_with(token::dot));
			view.remove_prefix(1u); //remove dot
			const TypedIdx definition = simp::build(store, infos, view);
			const std::size_t res_index = store.allocate_one();
			store.at(res_index) = Lambda{ definition, param_count };
			infos.lambda_params.resize(infos.lambda_params.size() - param_count); //remove own names again
			return TypedIdx(res_index, MathType::lambda);
		} break;
		default:
			assert(false);
			return TypedIdx();
		}
	} //build
	template TypedIdx build(Store&, name_lookup::LiteralInfos&, bmath::intern::ParseView);
	template TypedIdx build(Store&, name_lookup::PatternInfos&, bmath::intern::ParseView);

	namespace print {

		constexpr int max_infixr = std::numeric_limits<int>::max();
		constexpr int default_infixr = 0;

		constexpr int infixr(const TypedIdx f) {
			if (f.get_type() != MathType::buildin) { return default_infixr; }
			using namespace fn;
			switch (from_typed_idx(f)) {
			case Buildin(FixedArity::pow):        return 3000;
			case Buildin(Comm::product):          return 2001;
			case Buildin(Comm::sum):              return 2000;
			case Buildin(FixedArity::eq):         return 1000;
			case Buildin(FixedArity::neq):        return 1000;
			case Buildin(FixedArity::greater):    return 1000;
			case Buildin(FixedArity::smaller):    return 1000;
			case Buildin(FixedArity::greater_eq): return 1000;
			case Buildin(FixedArity::smaller_eq): return 1000;
			case Buildin(Comm::and_):             return 2;
			case Buildin(Comm::or_):              return 1;
			default:                              return default_infixr;
			}
		}

		void append_to_string(const UnsaveRef ref, std::string& str, const int parent_infixr)
		{
			switch (ref.type) {
			case Type(MathType::complex):
				bmath::intern::print::append_complex(*ref, str, parent_infixr);
				break;
			case Type(MathType::boolean):
				str.append(ref.index ? "true" : "false");
				break;
			case Type(MathType::symbol):
				str.append(ref->symbol);
				break;
			case Type(MathType::call): { 
				const Call& call = *ref;
				const TypedIdx function = call.function();
				const char* replacement_seperator = nullptr;
				if (function.get_type() == MathType::buildin) {
					replacement_seperator = [function]() -> const char* {
						using namespace fn;
						switch (from_typed_idx(function)) {
						case Buildin(Comm::sum):              return " + ";
						case Buildin(Comm::product):          return " * ";
						case Buildin(Comm::and_):             return " && ";
						case Buildin(Comm::or_):              return " || ";
						case Buildin(FixedArity::pow):        return " ^ ";
						case Buildin(FixedArity::eq):         return " == ";
						case Buildin(FixedArity::neq):        return " != ";
						case Buildin(FixedArity::greater):    return " > ";
						case Buildin(FixedArity::smaller):    return " < ";
						case Buildin(FixedArity::greater_eq): return " >= ";
						case Buildin(FixedArity::smaller_eq): return " <= ";
						default:                              return nullptr;
						}
					}();
				}
				if (!replacement_seperator) {
					append_to_string(ref.new_at(function), str, max_infixr);
					replacement_seperator = ", ";
				}
				const int own_infixr = infixr(function);
				if (own_infixr <= parent_infixr) { str.push_back('('); }				
				const char* seperator = "";
				for (const TypedIdx param : call.parameters()) {
					str.append(std::exchange(seperator, replacement_seperator));
					append_to_string(ref.new_at(param), str, own_infixr);
				}
				if (own_infixr <= parent_infixr) { str.push_back(')'); }
			} break;
			case Type(MathType::buildin):
				str.append(fn::name_of(fn::Buildin(ref.index)));
				break;
			case Type(MathType::lambda): {
				const Lambda& lambda = *ref;
				str.append("(\\.");
				append_to_string(ref.new_at(lambda.definition), str, max_infixr);
				str.push_back(')');
			} break;
			case Type(MathType::lambda_param):
				str.push_back('$');
				str.append(std::to_string(ref.index));
				break;
			default:
				assert(false);
			}
		} //append_to_string

	} //namespace print

} //namespace simp

