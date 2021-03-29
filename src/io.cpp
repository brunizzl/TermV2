#include "io.hpp"

#include <charconv>

#include "utility/vector.hpp"

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
		if (const std::size_t and_pos = find_first_of_skip_pars(view.tokens, token::or_); and_pos != TokenView::npos) {
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

		TypedIdx find_name_in_infos(const std::vector<NameInfo>& infos, const std::string_view name) 
		{
			const auto iter = std::find_if(infos.rbegin(), infos.rend(),
				[name](const NameInfo& i) { return i.name == name; });
			return iter != infos.rend() ?
				iter->value :
				TypedIdx();
		}

		TypedIdx build_literal_from_name(Store& store, const std::vector<NameInfo>& lambda_infos, std::string_view name)
		{
			if (const TypedIdx res = find_name_in_infos(lambda_infos, name); res != TypedIdx()) {
				return res;
			}
			else if (const fn::Buildin type = fn::type_of(name); type != fn::Buildin(fn::Buildin::COUNT)) {
				return TypedIdx(static_cast<unsigned>(type), MathType::buildin);
			}
			else {
				return TypedIdx(Symbol::build(store, name), MathType::symbol);
			}
		}

		TypedIdx build_pattern_from_name(Store& store, const NameInfos& infos, std::string_view name)
		{
			if (const TypedIdx res = find_name_in_infos(infos.lambda_infos, name); res != TypedIdx()) {
				return res;
			}
			if (const TypedIdx res = find_name_in_infos(infos.match_infos, name); res != TypedIdx()) {
				return res;
			}
			else if (const fn::Buildin type = fn::type_of(name); type != fn::Buildin(fn::Buildin::COUNT)) {
				return TypedIdx(static_cast<unsigned>(type), MathType::buildin);
			}
			else {
				if (name.starts_with('\'') && name.ends_with('\'')) {
					name.remove_prefix(1u);
					name.remove_suffix(1u);
				}
				return TypedIdx(Symbol::build(store, name), MathType::symbol);
			}
		}

		unsigned add_lambda_params(std::vector<NameInfo>& lambda_infos, bmath::intern::ParseView params)
		{
			using namespace bmath::intern;
			assert(params.tokens.starts_with(token::backslash) && 
				(params.tokens.ends_with(token::character) || params.size() == 1u));
			unsigned param_count = 0u;
			while (params.size()) {
				params.remove_prefix(1u); //remove previous comma (or '\\' for first parameter)
				const std::size_t comma = params.tokens.find_first_of(token::comma);
				const auto param = params.steal_prefix(comma);
				assert(param.tokens.find_first_not_of(token::character) == TokenView::npos);
				lambda_infos.push_back({ param.to_string_view(), TypedIdx(lambda_infos.size(), MathType::lambda_param) });
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

	TypedIdx build_literal(Store& store, std::vector<name_lookup::NameInfo>& lambda_infos, bmath::intern::ParseView view)
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
			const TypedIdx fst = build_literal(store, lambda_infos, view.substr(0, head.where));
			const TypedIdx snd = build_literal(store, lambda_infos, view.substr(head.where + operator_length));
			const std::size_t res_index = store.allocate_one();
			store.at(res_index) = Call{ fn::to_typed_idx(type), fst, snd };
			return TypedIdx(res_index, MathType::call);
		};
		const auto to_inverse_buildin_call = [&](const fn::Buildin type, auto invert_term) {
			const TypedIdx fst = build_literal(store, lambda_infos, view.substr(0, head.where));
			const TypedIdx snd_uninverted = build_literal(store, lambda_infos, view.substr(head.where + 1));
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
			const TypedIdx to_negate = build_literal(store, lambda_infos, view);
			const std::size_t res_index = store.allocate_one();
			store.at(res_index) = Call{ fn::to_typed_idx(fn::FixedArity::not_), to_negate };
			return TypedIdx(res_index, MathType::call);
		} break;
		case Head::Type::negate: {
			view.remove_prefix(1u);  //remove minus sign
			const TypedIdx to_negate = build_literal(store, lambda_infos, view);
			return build_negated(store, to_negate);
		} break;
		case Head::Type::real_value: {
			return build_value(store, parse_value(view));
		} break;
		case Head::Type::imag_value: {
			view.remove_suffix(1u); //remove token::imag_unit
			return build_value(store, Complex(0.0, parse_value(view)));
		} break;
		case Head::Type::symbol: {
			return name_lookup::build_literal_from_name(store, lambda_infos, view.to_string_view());
		} break;
		case Head::Type::call: {
			bmath::intern::StupidBufferVector<TypedIdx, 12> subterms;
			subterms.push_back(build_literal(store, lambda_infos, view.steal_prefix(head.where)));
			view.remove_prefix(1u); //remove '('
			view.remove_suffix(1u); //remove ')'
			if (view.size()) [[likely]] { //else no parameters at all
				const std::size_t comma = find_first_of_skip_pars(view.tokens, token::comma);
				const auto param_view = view.steal_prefix(comma); //now input starts with comma
				subterms.push_back(build_literal(store, lambda_infos, param_view));
			}
			while (view.size()) {
				view.remove_prefix(1u); //erase leading comma
				const std::size_t comma = find_first_of_skip_pars(view.tokens, token::comma);
				const auto param_view = view.steal_prefix(comma);
				subterms.push_back(build_literal(store, lambda_infos, param_view));
			}
			return TypedIdx(Call::build(store, subterms), MathType::call);
		} break;
		case Head::Type::lambda: {
			const unsigned param_count = name_lookup::add_lambda_params(lambda_infos, view.steal_prefix(head.where));
			assert(view.tokens.starts_with(token::dot));
			view.remove_prefix(1u); //remove dot
			const TypedIdx definition = build_literal(store, lambda_infos, view);
			const std::size_t res_index = store.allocate_one();
			store.at(res_index) = Lambda{ definition, param_count };
			lambda_infos.resize(lambda_infos.size() - param_count); //remove own names again
			return TypedIdx(res_index, MathType::lambda);
		} break;
		default:
			assert(false);
			return TypedIdx();
		}
	} //build_literal

	namespace print {

		void append_to_string(const UnsaveRef ref, std::string& str)
		{
			switch (ref.type) {
			case Type(MathType::buildin):
				str.append(fn::name_of(fn::Buildin(ref.index)));
				break;
			case Type(MathType::symbol):
				str.append(ref->symbol);
				break;
			case Type(MathType::call): { 
				const Call& call = *ref;
				append_to_string(ref.new_at(call.function()), str);
				str.push_back('(');
				const char* seperator = "";
				for (const TypedIdx param : call.range()) {
					str.append(std::exchange(seperator, ", "));
					append_to_string(ref.new_at(param), str);
				}
				str.push_back(')');
			} break;
			case Type(MathType::lambda): {
				const Lambda& lambda = *ref;
				str.append("(\\.");
				append_to_string(ref.new_at(lambda.definition), str);
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

