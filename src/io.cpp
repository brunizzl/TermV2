#include "io.hpp"

#include <charconv>
#include <limits>
#include <concepts>
#include <bitset>
#include <array>

#include "utility/vector.hpp"
#include "utility/misc.hpp"

#include "ioArithmetic.hpp"
#include "algorithms.hpp"

namespace simp {
	namespace parse {
		Head find_head_type(const bmath::intern::ParseView view)
		{
			using namespace bmath;
			using namespace bmath::intern;
			const std::size_t operator_pos = [](TokenView token_view) {
				enum class Arity { unary, binary };
				struct Operator { Token tok; Arity arity; };
				//order by precedence (low to high)
				constexpr auto operators = std::to_array<Operator>({
					{ token::backslash,    Arity::unary  },
					{ token::or_,          Arity::binary },
					{ token::and_,         Arity::binary },
					{ token::relation,     Arity::binary },
					{ token::sum,          Arity::binary },
					{ token::product,      Arity::binary },
					{ token::sticky_space, Arity::binary }, //can also act as multiplication
					{ token::hat,          Arity::binary },
					{ token::bang,         Arity::unary  },
					{ token::unary_minus,  Arity::unary  },
					});
				for (std::size_t i = 0u; i < operators.size(); i++) {
					const std::size_t pos = find_first_of_skip_pars(token_view, operators[i].tok);
					if (pos != TokenView::npos) {
						if (operators[i].arity == Arity::binary || pos == 0u) {
							return pos;
						}
						else { //found unary operation in middle of view -> return binary operation bevore it
							token_view.remove_suffix(token_view.size() - pos);
						}
					}
				}
				return TokenView::npos;
			}(view.tokens);
			if (operator_pos != TokenView::npos) {
				const Head::Type type = [&]() {
					switch (view.chars[operator_pos]) {
					case '\\': return Head::Type::lambda;
					case '|':  return Head::Type::or_;
					case '&':  return Head::Type::and_;
					case '!':  return operator_pos == 0u ? Head::Type::not_ : Head::Type::not_equals;
					case '=':  return Head::Type::equals;
					case '<':  return view.chars[operator_pos + 1u] == '=' ? Head::Type::smaller_eq : Head::Type::smaller;
					case '>':  return view.chars[operator_pos + 1u] == '=' ? Head::Type::greater_eq : Head::Type::greater;
					case '+':  return Head::Type::plus;
					case '-':  return operator_pos == 0u ? Head::Type::negate : Head::Type::minus;
					case '*':  return Head::Type::times;
					case ' ':  return Head::Type::times;
					case '/':  return Head::Type::divided;
					case '^':  return Head::Type::power;
					default:
						assert(false);
						return Head::Type::symbol;
					}
				}();
				return Head{ operator_pos, type };
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
			//returns false if name represents multi match or value match or is otherwise invalid
			bool mundane_name(const bmath::intern::ParseView view) noexcept
			{
				const std::string_view name = view.to_string_view();
				return view.tokens.find_first_not_of(bmath::intern::token::character) == std::string_view::npos &&
					name.find_first_of('.') == std::string_view::npos &&
					name.find_first_of('$') == std::string_view::npos &&
					name.find_first_of(' ') == std::string_view::npos;
			}

			NodeIndex find_name_in_infos(const std::vector<NameInfo>& infos, const std::string_view name)
			{
				const auto iter = std::find_if(infos.rbegin(), infos.rend(),
					[name](const NameInfo& i) { return i.name == name; });
				return iter != infos.rend() ?
					iter->value :
					NodeIndex();
			}

			NodeIndex build_symbol(Store& store, const LiteralInfos& infos, bmath::intern::ParseView view)
			{
				const std::string_view name = view.to_string_view();
				if (!mundane_name(view)) [[unlikely]] {
					throw bmath::ParseFailure{ view.offset, "ellipses or the dollar symbol are only expected when building a pattern" };
				}
				if (const NodeIndex res = find_name_in_infos(infos.lambda_params, name); res != NodeIndex()) {
					return res;
				}
				if (const nv::Native type = nv::type_of(name); type != nv::Native(nv::Native::COUNT)) {
					return nv::to_typed_idx(type);
				}
				return NodeIndex(Symbol::build(store, name), Literal::symbol);
			}

			NodeIndex build_symbol(Store& store, PatternInfos& infos, bmath::intern::ParseView view)
			{
				std::string_view name = view.to_string_view();
				if (const NodeIndex res = find_name_in_infos(infos.lambda_params, name); res != NodeIndex()) {
					return res;
				}
				if (const NodeIndex res = find_name_in_infos(infos.match_variables, name); res != NodeIndex()) {
					return res;
				}
				if (const nv::Native type = nv::type_of(name); type != nv::Native(nv::Native::COUNT)) {
					return NodeIndex(static_cast<unsigned>(type), Literal::native);
				}
				if (name.starts_with('\'') && name.ends_with('\'')) {
					name.remove_prefix(1u);
					name.remove_suffix(1u);
					if (!mundane_name(view)) [[unlikely]] { //tests not name, but the '\'' make no difference
						throw bmath::ParseFailure{ view.offset, "please decide: this looks weird" };
					}
					if (nv::type_of(name) != nv::Native(nv::Native::COUNT)) [[unlikely]] {
						throw bmath::ParseFailure{ view.offset, "sneaking in keywords like that is forbidden" };
					}
					return NodeIndex(Symbol::build(store, name), Literal::symbol);
				}
				if (name.find_first_of(' ') != std::string_view::npos) [[unlikely]] {
						throw bmath::ParseFailure{ view.offset, "leave me some space, but not like that" };
				}
				const auto add_match_variable = [&](const NodeType type) {
					const std::size_t id = std::count_if(infos.match_variables.begin(), infos.match_variables.end(),
						[type](const NameInfo& i) { return i.value.get_type() == type; });
					const NodeIndex result = NodeIndex(id, type);
					infos.match_variables.emplace_back(name, result);
					return result;
				};
				if (name.starts_with('$')) {
					return add_match_variable(Match::value);
				}
				if (name.ends_with('.')) {
					return add_match_variable(Match::multi);
				}
				return add_match_variable(Match::single_weak);
			}

			unsigned add_lambda_params(std::vector<NameInfo>& lambda_params, bmath::intern::ParseView params_view)
			{
				using namespace bmath::intern;
				if (!params_view.tokens.starts_with(token::backslash) /*|| params_view.tokens.find_last_not_of(token::character) != 0u*/) [[unlikely]]
					throw bmath::ParseFailure{ params_view.offset, "this is a weird looking lambda parameter declaration" };
				unsigned param_count = 0u;
				while (params_view.size()) {
					params_view.remove_prefix(1u); //remove previous space (or '\\' for first parameter)
					const std::size_t space = params_view.to_string_view().find_first_of(' ');
					const ParseView param = params_view.steal_prefix(space);
					if (!mundane_name(param)) [[unlikely]]
						throw bmath::ParseFailure{ params_view.offset, "we name lambda parameters less exiting around here" };
					if (!param.size()) [[unlikely]]
						throw bmath::ParseFailure{ params_view.offset, "there is no reason for nullary lambdas in a purely functional language" };
					lambda_params.push_back({ param.to_string_view(), NodeIndex(lambda_params.size(), Literal::lambda_param) });
					param_count++;
				}
				return param_count;
			}
		} //namespace name_lookup

		double parse_value(const bmath::intern::ParseView view)
		{
			double value;
			const auto [ptr, error] = std::from_chars(view.chars, view.chars + view.size(), value);
			if (error != std::errc()) [[unlikely]] 
				throw bmath::ParseFailure{ view.offset, "value syntax is illformed or value out of bounds" };
			if (ptr != view.chars + view.size()) [[unlikely]] 
				throw bmath::ParseFailure{ std::size_t(view.offset + ptr - view.chars + 1u), "value syntax is illformed" };
			return value;
		} //parse_value

		template<name_lookup::InfoLike Infos>
		NodeIndex build(Store& store, Infos& infos, bmath::intern::ParseView view)
		{
			using namespace bmath;
			using namespace bmath::intern;

			if (view.size() == 0u) [[unlikely]] throw ParseFailure{ view.offset, "recieved empty substring" };
			Head head = parse::find_head_type(view);
			while (head.type == Head::Type::group) {
				view.remove_prefix(1u);
				view.remove_suffix(1u);
				head = parse::find_head_type(view);
			}
			const auto to_buildin_call = [&](const std::size_t operator_length, const nv::Native type) {
				const NodeIndex fst = parse::build(store, infos, view.substr(0, head.where));
				const NodeIndex snd = parse::build(store, infos, view.substr(head.where + operator_length));
				const std::size_t res_index = store.allocate_one();
				store.at(res_index) = Call{ nv::to_typed_idx(type), fst, snd };
				return NodeIndex(res_index, Literal::call);
			};
			const auto to_inverse_buildin_call = [&](const nv::Native type, auto invert_term) {
				const NodeIndex fst = parse::build(store, infos, view.substr(0, head.where));
				const NodeIndex snd_uninverted = build(store, infos, view.substr(head.where + 1));
				const NodeIndex snd = invert_term(store, snd_uninverted);
				const std::size_t res_index = store.allocate_one();
				store.at(res_index) = Call{ nv::to_typed_idx(type), fst, snd };
				return NodeIndex(res_index, Literal::call);
			};
			switch (head.type) {
			case Head::Type::or_:        return to_buildin_call(2, nv::Comm::or_);
			case Head::Type::and_:       return to_buildin_call(2, nv::Comm::and_);
			case Head::Type::equals:     return to_buildin_call(2, nv::ToBool::eq);
			case Head::Type::not_equals: return to_buildin_call(2, nv::ToBool::neq);
			case Head::Type::greater:    return to_buildin_call(1, nv::ToBool::greater);
			case Head::Type::smaller:    return to_buildin_call(1, nv::ToBool::smaller);
			case Head::Type::greater_eq: return to_buildin_call(2, nv::ToBool::greater_eq);
			case Head::Type::smaller_eq: return to_buildin_call(2, nv::ToBool::smaller_eq);
			case Head::Type::plus:       return to_buildin_call(1, nv::Comm::sum);
			case Head::Type::minus:      return to_inverse_buildin_call(nv::Comm::sum, build_negated<Store>);
			case Head::Type::times:      return to_buildin_call(1, nv::Comm::product);
			case Head::Type::divided:    return to_inverse_buildin_call(nv::Comm::product, build_inverted<Store>);
			case Head::Type::power:      return to_buildin_call(1, nv::CtoC::pow);
			case Head::Type::not_: {
				view.remove_prefix(1u);  //remove '!'
				const NodeIndex to_negate = parse::build(store, infos, view);
				const std::size_t res_index = store.allocate_one();
				store.at(res_index) = Call{ nv::to_typed_idx(nv::ToBool::not_), to_negate };
				return NodeIndex(res_index, Literal::call);
			} break;
			case Head::Type::negate: {
				view.remove_prefix(1u);  //remove minus sign
				const NodeIndex to_negate = parse::build(store, infos, view);
				return build_negated(store, to_negate);
			} break;
			case Head::Type::real_value: {
				return parse::build_value(store, parse_value(view));
			} break;
			case Head::Type::imag_value: {
				view.remove_suffix(1u); //remove token::imag_unit
				return view.size() ?
					parse::build_value(store, Complex(0.0, parse_value(view))):
					parse::build_value(store, Complex(0.0, 1.0));
			} break;
			case Head::Type::symbol: {
				return name_lookup::build_symbol(store, infos, view);
			} break;
			case Head::Type::call: {
				bmath::intern::StupidBufferVector<NodeIndex, 12> subterms;
				subterms.push_back(parse::build(store, infos, view.steal_prefix(head.where)));
				view.remove_prefix(1u); //remove '('
				view.remove_suffix(1u); //remove ')'
				if (view.size()) [[likely]] { //else no parameters at all
					const std::size_t comma = find_first_of_skip_pars(view.tokens, token::comma);
					const auto param_view = view.steal_prefix(comma); //now input starts with comma
					subterms.push_back(parse::build(store, infos, param_view));
				}
				while (view.size()) {
					view.remove_prefix(1u); //erase leading comma
					const std::size_t comma = find_first_of_skip_pars(view.tokens, token::comma);
					const auto param_view = view.steal_prefix(comma);
					subterms.push_back(parse::build(store, infos, param_view));
				}
				return NodeIndex(Call::build(store, subterms), Literal::call);
			} break;
			case Head::Type::lambda: {
				const bool outermost_lambda = infos.lambda_params.size() == 0u;
				const std::size_t dot_pos = view.tokens.find_first_of(token::dot);
				if (dot_pos == TokenView::npos) [[unllikely]] {
					throw ParseFailure { view.offset, "there is no valid lambda without the definition preceeded by a dot" };
				}
				const unsigned param_count = name_lookup::add_lambda_params(infos.lambda_params, view.steal_prefix(dot_pos));
				view.remove_prefix(1u); //remove dot
				const NodeIndex definition = parse::build(store, infos, view);
				const std::size_t res_index = store.allocate_one();
				store.at(res_index) = Lambda{ definition, param_count, !outermost_lambda };
				infos.lambda_params.resize(infos.lambda_params.size() - param_count); //remove own names again
				return NodeIndex(res_index, Literal::lambda);
			} break;
			default:
				assert(false);
				return NodeIndex();
			}
		} //build
		template NodeIndex build(Store&, name_lookup::LiteralInfos&, bmath::intern::ParseView);
		template NodeIndex build(Store&, name_lookup::PatternInfos&, bmath::intern::ParseView);

		std::pair<NodeIndex, NodeIndex> raw_rule(Store& store, std::string name)
		{
			using namespace bmath;
			using namespace bmath::intern;
			auto parse_str = ParseString(name);
			constexpr char allowed[] = { token::character, token::number, token::open_grouping, 
				token::clse_grouping, token::unary_minus, token::sum, token::product, token::comma, 
				token::hat, token::equals, token::bar, token::bang, token::space, token::imag_unit, 
				token::backslash, token::dot, token::relation, token::and_, token::or_, '\0' };
			if (const std::size_t pos = parse_str.tokens.find_first_not_of(allowed); pos != TokenString::npos) [[unlikely]] {
				throw ParseFailure{ pos, "unexpected character" };
			}
			parse_str.allow_implicit_product(token::sticky_space, ' ');
			parse_str.remove_space();
			auto [lhs_view, conditions_view, rhs_view] = [](ParseView view) {
				const std::size_t equals_pos = find_first_of_skip_pars(view.tokens, token::equals);
				if (equals_pos == std::string::npos) [[unlikely]]
					throw ParseFailure{ 0u, "there is no rule without a top level '='" };
				if (count_skip_pars(view.tokens, token::equals) > 1u) [[unlikely]]
					throw ParseFailure{ 0u, "to many top level '='" };

				ParseView conditions = view.steal_prefix(equals_pos); //thus far also contains lhs
				view.remove_prefix(1u); //removed equals -> view now only contains rhs

				const std::size_t bar_pos = find_first_of_skip_pars(conditions.tokens, token::bar);
				if (count_skip_pars(conditions.tokens, token::bar) > 1u) [[unlikely]]
					throw ParseFailure{ bar_pos, "expected only this '|', no further ones at top grouping level" };
				const ParseView lhs_view = conditions.steal_prefix(bar_pos);

				return std::make_tuple(lhs_view, conditions, view);
			}(parse_str);

			parse::name_lookup::PatternInfos infos;
			NodeIndex lhs_head = parse::build(store, infos, lhs_view);
			NodeIndex rhs_head = parse::build(store, infos, rhs_view);
			lhs_head = normalize::recursive(MutRef(store, lhs_head), {}, 0);
			rhs_head = normalize::recursive(MutRef(store, rhs_head), { .remove_unary_assoc = false }, 0);

			//extra condition concerning single match variables (might set multiple in relation)
			struct SingleCondition
			{
				NodeIndex head = NodeIndex();
				std::bitset<MatchData::max_single_match_count> dependencies = 0; //dependencies[i] is set iff single match i occurs in the condition
			};
			std::vector<NodeIndex> single_conditions;

			std::array<nv::ComplexSubset, MatchData::max_value_match_count> value_conditions;
			value_conditions.fill(nv::ComplexSubset::complex);

			while (conditions_view.size()) {
				assert(conditions_view.tokens.starts_with(token::bar) || conditions_view.tokens.starts_with(token::comma));
				conditions_view.remove_prefix(1u); //remove bar / comma
				const std::size_t comma = find_first_of_skip_pars(conditions_view.tokens, token::comma);
				const auto condition_view = conditions_view.steal_prefix(comma);
				const NodeIndex condition_head = normalize::recursive(
					MutRef(store, parse::build(store, infos, condition_view)), {}, 0);
				if (condition_head.get_type() != Literal::call) [[unlikely]] {
					throw ParseFailure{ conditions_view.offset, "please make this condition look a bit more conditiony." };
				}
				const auto cond_ref = MutRef(store, condition_head);
				const bool contains_single = simp::search(cond_ref, 
					[](const UnsaveRef r) { return r.type == Match::single_weak; }) != NodeIndex();
				const bool contains_value = simp::search(cond_ref,
					[](const UnsaveRef r) { return r.type == Match::value; }) != NodeIndex();
				const bool contains_multi = simp::search(cond_ref,
					[](const UnsaveRef r) { return r.type == Match::multi; }) != NodeIndex(); 
				if (contains_multi) [[unlikely]] {
					throw ParseFailure{ conditions_view.offset, "sorry, but i am too lazy to check that" };
				}
				if (contains_single && contains_value) [[unlikely]] {
					throw ParseFailure{ conditions_view.offset, "single match and value match may not depend on each other" };
				}
				if (contains_single) {
					//TODO: 
					//- find out which single variable indices are contained for SingleCondition instance
					//- add entry to single_conditions
				}
				if (contains_value) {
					//TODO:
					//condition may only be of form "type($<value match>, <complex subset>)"
					//if so: adjust value_conditions at right index
				}
			}
			{
				//TODO:
				//go through lhs in preorder, adjust single match using single_conditions
				//idea: keep track which variable has aleady been encountered. if a new single variable instance is met do the following:
				//if its index has already been encountered: done
				//if not: check of congle_conditions contains conditions depending only on already encountered indices and the current one
				//           yes: combine these conditions into an and condition, append it at a RestrictedSingleMatch node (or convert to .restriction)
				//           no: turn Instance to Match::single_unrestricted
			}
			{
				//TODO:
				//clear owner situation of value match in lhs, maybe add value restriction
			}
			{ 
				//TODO (likely own function, as otherwise value match reordering will fail):
				//change call to PatternCall where a subtree contains some pattern stuff in lhs
			}

			//the following will be done in different functions:
			//TODO (mostly in lhs):
			//verify only one occurence of each multi match
			//verify only one occurence of multi match in one Comm, any number of occurences in one NonComm, no occurences elsewhere
			//adjust muti match indices to equal corresponding call + add management field to call
			//check variadics to each not exceed maximal length and to have not more than is allowed 
			//sort Comm, flatten associative variadics (do not remove indirection via variadic in rhs!)
			//make the first single match of each index strong, give belonging conditions to each
			//bubble value match variables up as high as possible, make first occurence strong

			return std::pair{ lhs_head, rhs_head };
		} //raw_rule
	} //namespace parse

	namespace print {

		constexpr int max_infixr = std::numeric_limits<int>::max();
		constexpr int default_infixr = 0;

		constexpr int infixr(const NodeIndex f) {
			if (f.get_type() != Literal::native) { return default_infixr; }
			using namespace nv;
			switch (from_typed_idx(f)) {
			case Native(CtoC::negate):       return 4001;
			case Native(ToBool::not_):       return 4000;
			case Native(CtoC::pow):          return 3000;
			case Native(Comm::product):      return 2001;
			case Native(Comm::sum):          return 2000;
			case Native(ToBool::eq):         return 1000;
			case Native(ToBool::neq):        return 1000;
			case Native(ToBool::greater):    return 1000;
			case Native(ToBool::smaller):    return 1000;
			case Native(ToBool::greater_eq): return 1000;
			case Native(ToBool::smaller_eq): return 1000;
			case Native(Comm::and_):         return 2;
			case Native(Comm::or_):          return 1;
			default:                          return default_infixr;
			}
		}

		void append_variadic_meta_data(const VariadicMetaData& data, std::string& str)
		{
			str.append("[");
			str.append(std::to_string(data.match_data_index));
			str.append(", ");
			str.append(std::bitset<32>(data.rematchable).to_string());
			str.append(", ");
			str.append(std::bitset<32>(data.always_after_prev).to_string());
			str.append("]");
		}

		void append_to_string(const UnsaveRef ref, std::string& str, const int parent_infixr)
		{
			switch (ref.type) {
			case NodeType(Literal::complex):
				bmath::intern::print::append_complex(*ref, str, parent_infixr);
				break;
			case NodeType(Literal::symbol):
				str.append(ref->symbol);
				break;
			case NodeType(PatternCall{}): {
				append_variadic_meta_data(variadic_meta_data(ref), str);
			} [[fallthrough]];				
			case NodeType(Literal::call): { 
				const Call& call = *ref;
				const NodeIndex function = call.function();
				const char* replacement_seperator = nullptr;
				const auto [init, seperator] = [&]() -> std::pair<const char*, const char*> {
					using namespace nv;
					if (function.get_type() == Literal::native) {
						switch (from_typed_idx(function)) {
						case Native(Comm::sum):          return { "" , " + "  };
						case Native(Comm::product):      return { "" , " * "  };
						case Native(Comm::and_):         return { "" , " && " };
						case Native(Comm::or_):          return { "" , " || " };
						case Native(CtoC::pow):          return { "" , " ^ "  };
						case Native(CtoC::negate):       return { "-", ""     };
						case Native(ToBool::eq):         return { "" , " == " };
						case Native(ToBool::neq):        return { "" , " != " };
						case Native(ToBool::greater):    return { "" , " > "  };
						case Native(ToBool::smaller):    return { "" , " < "  };
						case Native(ToBool::greater_eq): return { "" , " >= " };
						case Native(ToBool::smaller_eq): return { "" , " <= " };
						case Native(ToBool::not_):       return { "!", ""     };
						}
					}
					append_to_string(ref.new_at(function), str, max_infixr);
					return { "", ", " };
				}();
				const int own_infixr = infixr(function);
				if (own_infixr <= parent_infixr) { str.push_back('('); }				
				const char* spacer = init;
				for (const NodeIndex param : call.parameters()) {
					str.append(std::exchange(spacer, seperator));
					append_to_string(ref.new_at(param), str, own_infixr);
				}
				if (own_infixr <= parent_infixr) { str.push_back(')'); }
			} break;
			case NodeType(Literal::native):
				str.append(nv::name_of(nv::Native(ref.index)));
				break;
			case NodeType(Literal::lambda): {
				const Lambda& lambda = *ref;
				str.append(lambda.transparent ? "(\\." : "{\\.");
				append_to_string(ref.new_at(lambda.definition), str, max_infixr);
				str.push_back(lambda.transparent ? ')' : '}');
			} break;
			case NodeType(Literal::lambda_param):
				str.push_back('%');
				str.append(std::to_string(ref.index));
				break;
			case NodeType(Match::single_restricted): {
				const RestrictedSingleMatch& var = *ref;
				str.append("_T");
				str.append(std::to_string(var.match_data_index));
				str.append("[");
				str.append(std::to_string(static_cast<unsigned>(var.restriction)));
				if (var.condition != NodeIndex()) {
					str.append(", ");
					append_to_string(ref.new_at(var.condition), str, default_infixr);
				}
				str.append("]");
			} break;				
			case NodeType(Match::single_unrestricted):
			case NodeType(Match::single_weak):
				str.append("_T");
				str.append(std::to_string(ref.index));
				str.append(ref.type == Match::single_weak ? "'" : "");
				break;
			case NodeType(Match::value): {
				const ValueMatch& var = *ref;
				str.append("_V");
				str.append(std::to_string(var.match_data_index));
				str.append("[");
				append_to_string(ref.new_at(var.match_index), str, default_infixr);
				str.append("]");
			} break;
			case NodeType(Match::multi):
				str.append("_MM");
				str.append(std::to_string(ref.index));
				str.append("...");
				break;
			default:
				BMATH_UNREACHABLE;
				assert(false);
			}
		} //append_to_string


		void append_memory_row(const Ref ref, std::vector<std::string>& rows)
		{
			const auto show_string_nodes = [&ref, &rows](std::uint32_t idx, bool show_first) {
				const Symbol& s = ref.store->at(idx);
				const std::size_t end = idx + s.node_count();
				if (!show_first) {
					idx++;
				}
				while (idx < end) {
					rows[idx++] += "...";
				}
			};
			if (!is_stored_node(ref.type)) {
				return;
			}

			std::string& current_str = rows[ref.index];
			switch (ref.type) {
			case NodeType(Literal::complex): {
				current_str += "value      : ";
			} break;
			case NodeType(Literal::symbol): {
				current_str += "symbol     : ";
				show_string_nodes(ref.index, false);
			} break;
			case NodeType(PatternCall{}): {
				std::string& prev_str = rows[ref.index - 1u];
				prev_str += "meta data  : ";
				append_variadic_meta_data(variadic_meta_data(ref), prev_str);
			} [[fallthrough]];
			case NodeType(Literal::call): {
				//parameters:
				current_str += "call       :    { ";
				const char* separator = "";
				const Call& vec = *ref;
				std::string* current_line = &current_str; //as we never add a new string to rows, this pointer will not dangle
				std::size_t vec_idx = 0u;
				const auto maybe_start_new_line = [&] {
					if ((vec_idx - Call::min_capacity) % Call::values_per_node == 0) {
						*(++current_line) += "  ...        ";
					}
				};
				for (; vec_idx < vec.size(); vec_idx++) {
					*current_line += std::exchange(separator, ", ");
					maybe_start_new_line();
					const std::size_t elem_idx = vec[vec_idx].get_index();
					if (elem_idx < 10) {
						*current_line += ' '; //pleeeaaasee std::format, where are you? :(
					}
					if (elem_idx < 100) {
						*current_line += ' '; //pleeeaaasee std::format, where are you? :(
					}
					*current_line += std::to_string(elem_idx);
					print::append_memory_row(ref.new_at(vec[vec_idx]), rows);
				}
				*current_line += " }";
				for (; vec_idx < vec.capacity(); vec_idx++) {
					maybe_start_new_line();
				}
			} break;
			case NodeType(Literal::lambda): {
				const Lambda& lam= *ref;
				current_str += "lambda     : ";
				print::append_memory_row(ref.new_at(lam.definition), rows);
			} break;
			default:
				current_str += "unknown...";
			}

			//append name of subterm to line
			current_str.append(std::max(0, 38 - (int)current_str.size()), ' ');
			print::append_to_string(ref, current_str, 0);
		} //append_memory_row

		std::string to_memory_layout(const Store& store, const std::initializer_list<const NodeIndex> heads)
		{
			std::vector<std::string> rows(std::max(store.size(), bmath::intern::pattern::match::MatchData::max_variadic_count), "");

			std::string result((heads.size() == 1u) ? "  head at index: " : "  heads at indices: ");
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
			{
				const bmath::intern::BitVector used_positions = store.storage_occupancy();
				for (int i = 0; i < store.size(); i++) {
					if (i < 10) { //please std::format, i need you :(
						result += "  ";
					}
					else if (i < 100) {
						result += " ";
					}
					result += std::to_string(i);
					result += " | ";
					result += rows[i];
					if (!used_positions.test(i)) {
						result += "-----free slot-----";
					}
					result += "\n";
				}
			}
			return result;
		} //to_memory_layout

	} //namespace print

} //namespace simp

