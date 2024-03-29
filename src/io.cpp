#include "io.hpp"

#include <charconv>
#include <limits>
#include <concepts>
#include <bitset>
#include <array>
#include <unordered_map>
#include <unordered_set>
#include <mutex>
#include <tuple>
#include <format>

#include "utility/vector.hpp"
#include "utility/misc.hpp"
#include "utility/sumEnum.hpp"

#include "algorithms.hpp"
#include "control.hpp"

namespace simp {

	//a symbol name is converted to a symbol id while constructing a term.
	//the data connecting symbol id and symbol name is found here
	class Names
	{
		//used during parsing: ids.at(name) gives corresponding id
		static std::unordered_map<std::string, std::uint32_t>& ids() 
		{
			static std::unordered_map<std::string, std::uint32_t> ids = [] {
				std::unordered_map<std::string, std::uint32_t> native_ids;
				for (std::uint32_t id = 0; id < (unsigned)nv::Native::COUNT; id++) {
					const std::string_view id_name = nv::name_of(nv::Native(id));
					native_ids[{id_name.data(), id_name.size()}] = id;
				}
				return native_ids;
			}();
			return ids;
		} //ids

		//used during printing: names.at(id) gives the corresponding name
		static std::vector<std::string>& names()
		{
			static std::vector<std::string> names = [] {
				std::vector<std::string> native_names;
				for (std::uint32_t id = 0; id < (unsigned)nv::Native::COUNT; id++) {
					const std::string_view id_name = nv::name_of(nv::Native(id));
					native_names.emplace_back(id_name.data(), id_name.size());
				}
				return native_names;
			}();
			return names;
		} //names

		static std::mutex access;

	public:
		static std::uint32_t id_of(const std::string name)
		{
			const auto lock = std::lock_guard<std::mutex>(access);
			const auto pos = ids().find(name);
			if (pos != ids().end()) {
				return pos->second;
			}
			else {
				const std::uint32_t id = names().size();
				ids()[name] = id;
				names().push_back(name);
				return id;
			}
		} //id_of

		static const std::string& name_of(const std::uint32_t id)
		{
			const auto lock = std::lock_guard<std::mutex>(access);
			assert(id < names().size());
			return names()[id];
		} //name_of
	}; //class Names

	std::mutex Names::access = {};

	namespace parse {
		Head find_head_type(const ParseView view)
		{
			using namespace simp;
			using namespace simp;
			const std::size_t operator_pos = [](TokenView token_view) {
				enum class SearchStrategy { front, left_right, right_left };
				struct Operator { Token tok; SearchStrategy strat; };
				//order by precedence (low to high)
				constexpr auto operators = std::to_array<Operator>({
					{ token::colon,        SearchStrategy::left_right },
					{ token::backslash,    SearchStrategy::front      },
					{ token::or_,          SearchStrategy::left_right },
					{ token::and_,         SearchStrategy::left_right },
					{ token::relation,     SearchStrategy::left_right },
					{ token::sum,          SearchStrategy::right_left },
					{ token::sticky_space, SearchStrategy::right_left }, //can also act as multiplication
					{ token::product,      SearchStrategy::right_left },
					{ token::unary_minus,  SearchStrategy::front      },
					{ token::hat,          SearchStrategy::right_left },
					{ token::bang,         SearchStrategy::front      },
				});
				
				for (std::size_t i = 0u; i < operators.size(); i++) {
					switch (operators[i].strat) {
					case SearchStrategy::front: {
						std::size_t pos = find_first_of_skip_pars(token_view, operators[i].tok);
						if (pos == 0u) {
							return pos;
						}
						else if (pos != TokenView::npos) { //found unary operation in middle of view -> return binary operation bevore it
							token_view.remove_suffix(token_view.size() - pos);
						}
					} break;
					case SearchStrategy::left_right: {
						std::size_t pos = find_first_of_skip_pars(token_view, operators[i].tok);
						if (pos != TokenView::npos) {
							return pos;
						}
					} break;
					case SearchStrategy::right_left: {
						std::size_t pos = find_last_of_skip_pars(token_view, operators[i].tok);
						if (pos != TokenView::npos) {
							return pos;
						}
					} break;
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
					case ':':  return view.chars[operator_pos + 1u] == ':' ? Head::Type::cons : Head::Type::of_type;
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
			if (view.tokens[first_not_character] != token::open_grouping) [[unlikely]] 
				throw ParseFailure{ first_not_character + view.offset, "illegal character, expected '('" };
			if (!view.tokens.ends_with(token::clse_grouping)) [[unlikely]] 
				throw ParseFailure{ view.size() + view.offset, "poor grouping, expected ')'" };
			if (first_not_character == 0u && find_closed_par(0, view.tokens) + 1u == view.tokens.size()) {
				return Head{ 0u, Head::Type::group };
			}
			else { //can be called immediatly -> find opening paren from back
				return Head{ find_open_par(view.size() - 1u, view.tokens), Head::Type::f_app };
			}
		} //find_head_type

		namespace name_lookup {
			//returns false if name represents multi match or value match or is otherwise invalid
			bool mundane_name(const ParseView view) noexcept
			{
				const std::string_view name = view.to_string_view();
				return view.tokens.find_first_not_of(token::character) == std::string_view::npos &&
					name.find_first_of(".$ ") == std::string_view::npos &&
					!name.starts_with('_');
			}

			NodeIndex find_name_in_infos(const std::vector<NameInfo>& infos, const std::string_view name)
			{
				const auto iter = std::find_if(infos.rbegin(), infos.rend(),
					[name](const NameInfo& i) { return i.name == name; });
				return iter != infos.rend() ?
					iter->value :
					invalid_index;
			}

			NodeIndex build_symbol(Store& store, const LiteralInfos& infos, ParseView view)
			{
				const std::string_view name = view.to_string_view();
				if (const NodeIndex res = find_name_in_infos(infos.lambda_params, name); res != invalid_index) {
					return res;
				}
				if (!mundane_name(view)) [[unlikely]] {
					throw ParseFailure{ view.offset, 
					"ellipses or the dollar symbol are only expected when building a pattern; only pattern variables may start with an underscore" };
				}
				return NodeIndex(Names::id_of({name.data(), name.size()}), Literal::symbol);
			}

			NodeIndex build_symbol(Store& store, PatternInfos& infos, ParseView view)
			{
				std::string_view name = view.to_string_view();
				if (const NodeIndex res = find_name_in_infos(infos.lambda_params, name); res != invalid_index) {
					return res;
				}
				if (const NodeIndex res = find_name_in_infos(infos.single_matches, name); res != invalid_index) {
					return res;
				}
				if (const NodeIndex res = find_name_in_infos(infos.multi_matches, name); res != invalid_index) {
					if (infos.parse_match) [[unlikely]] {
						throw ParseFailure{ view.offset, "multiple occurences of multi in lhs are forbidden" };
					}
					return copy_tree(Ref(store, res), store);
				}
				if (const NodeIndex res = find_name_in_infos(infos.value_matches, name); res != invalid_index) {
					return copy_tree(Ref(store, res), store);
				}
				if (!infos.parse_match && !mundane_name(view)) [[unlikely]] {
					throw ParseFailure{ view.offset, "match variables need to be introduced in lhs" };
				}
				if (name.ends_with('.')) {
					const std::size_t res_index = store.allocate_one();
					store.at(res_index) = MultiMatch{ (unsigned)infos.multi_matches.size(), 0 };
					const NodeIndex result = NodeIndex(res_index, SpecialMatch::multi);
					infos.multi_matches.emplace_back(name, result);
					return result;
				}
				if (name.starts_with('_')) {
					const NodeIndex result = NodeIndex(infos.single_matches.size(), SingleMatch::weak);
					infos.single_matches.emplace_back(name, result);
					return result;
				}
				if (name.starts_with('$')) {
					const std::size_t res_index = FApp::build(store, std::to_array({
						from_native(nv::PatternFn::value_match),				  //function type
						NodeIndex(infos.value_matches.size(), PatternUnsigned{}), //.match_state_index
						from_native(nv::ComplexSubset::complex),                  //.domain
						from_native(nv::MiscFn::id)                               //.inverse
					}));
					const NodeIndex result = NodeIndex(res_index, Literal::f_app);
					infos.value_matches.emplace_back(name, result);
					return result;
				}
				return NodeIndex(Names::id_of({ name.data(), name.size() }), Literal::symbol);
			}

			unsigned add_lambda_params(std::vector<NameInfo>& lambda_params, ParseView params_view)
			{
				using namespace simp;
				if (!params_view.tokens.starts_with(token::backslash)) [[unlikely]]
					throw ParseFailure{ params_view.offset, "this is a weird looking lambda parameter declaration" };

				const unsigned old_size = lambda_params.size();
				while (params_view.size()) {
					params_view.remove_prefix(1u); //remove previous space (or '\\' for first parameter)
					const std::size_t space = params_view.to_string_view().find_first_of(' ');
					const ParseView param = params_view.steal_prefix(space);
					if (!mundane_name(param)) 
						throw ParseFailure{ params_view.offset, "we name lambda parameters less exiting around here" };

					if (!param.size()) 
						throw ParseFailure{ params_view.offset, "there is little reason for nullary lambdas in a purely functional language" };

					const std::string_view param_name = param.to_string_view();
					//runs in O(n^2), but lambdas are expected to behave in size. 
					if (std::find_if(lambda_params.begin(), lambda_params.end(), 
						[param_name](const NameInfo& i) { return i.name == param_name; }) != lambda_params.end()) 
						throw ParseFailure{ params_view.offset, "name shadowing in lambdas is forbidden" };

					lambda_params.push_back({ param_name, NodeIndex(lambda_params.size(), Literal::lambda_param) });
				}
				return lambda_params.size() - old_size;
			}
		} //namespace name_lookup

		double parse_value(const ParseView view)
		{
			double value;
			const auto [ptr, error] = std::from_chars(view.chars, view.chars + view.size(), value);
			if (error != std::errc()) [[unlikely]] 
				throw ParseFailure{ view.offset, "value syntax is illformed or value out of bounds" };
			if (ptr != view.chars + view.size()) [[unlikely]] 
				throw ParseFailure{ std::size_t(view.offset + ptr - view.chars + 1u), "value syntax is illformed" };
			return value;
		} //parse_value

		template<name_lookup::InfoLike Infos>
		NodeIndex build(Store& store, Infos& infos, ParseView view)
		{
			using namespace simp;
			using namespace simp;

			if (view.size() == 0u) [[unlikely]] throw ParseFailure{ view.offset, "recieved empty substring" };
			Head head = parse::find_head_type(view);
			while (head.type == Head::Type::group) {
				view.remove_prefix(1u);
				view.remove_suffix(1u);
				head = parse::find_head_type(view);
			}
			const auto to_buildin_app = [&](const std::size_t operator_length, const nv::Native type) {
				const NodeIndex fst = parse::build(store, infos, view.substr(0, head.where));
				const NodeIndex snd = parse::build(store, infos, view.substr(head.where + operator_length));
				const std::size_t res_index = store.allocate_one();
				store.at(res_index) = FApp{ from_native(type), fst, snd };
				return NodeIndex(res_index, Literal::f_app);
			};
			const auto to_inverse_buildin_app = [&](const nv::Native type, auto invert_term) {
				const NodeIndex fst = parse::build(store, infos, view.substr(0, head.where));
				const NodeIndex snd_uninverted = build(store, infos, view.substr(head.where + 1));
				const NodeIndex snd = invert_term(store, snd_uninverted);
				const std::size_t res_index = store.allocate_one();
				store.at(res_index) = FApp{ from_native(type), fst, snd };
				return NodeIndex(res_index, Literal::f_app);
			};
			switch (head.type) {
			case Head::Type::or_:        return to_buildin_app(2, nv::Comm::or_);
			case Head::Type::and_:       return to_buildin_app(2, nv::Comm::and_);
			case Head::Type::equals:     return to_buildin_app(2, nv::ToBool::eq);
			case Head::Type::not_equals: return to_buildin_app(2, nv::ToBool::neq);
			case Head::Type::greater:    return to_buildin_app(1, nv::ToBool::greater);
			case Head::Type::smaller:    return to_buildin_app(1, nv::ToBool::smaller);
			case Head::Type::greater_eq: return to_buildin_app(2, nv::ToBool::greater_eq);
			case Head::Type::smaller_eq: return to_buildin_app(2, nv::ToBool::smaller_eq);
			case Head::Type::plus:       return to_buildin_app(1, nv::Comm::sum);
			case Head::Type::minus:      return to_inverse_buildin_app(nv::Comm::sum, build_negated<Store>);
			case Head::Type::times:      return to_buildin_app(1, nv::Comm::prod);
			case Head::Type::divided:    return to_inverse_buildin_app(nv::Comm::prod, build_inverted<Store>);
			case Head::Type::power:      return to_buildin_app(1, nv::CtoC::pow);
			case Head::Type::of_type:    return to_buildin_app(1, nv::PatternFn::of_type);
			case Head::Type::cons:       return to_buildin_app(2, nv::MiscFn::cons);
			case Head::Type::not_: {
				view.remove_prefix(1u);  //remove '!'
				const NodeIndex to_negate = parse::build(store, infos, view);
				const std::size_t res_index = store.allocate_one();
				store.at(res_index) = FApp{ from_native(nv::ToBool::not_), to_negate };
				return NodeIndex(res_index, Literal::f_app);
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
				if (!view.size()) [[unlikely]] {
					throw ParseFailure { view.offset, "token \"i\" is forbidden: write \"1i\" for imaginary unit" };
				}
				return parse::build_value(store, Complex(0.0, parse_value(view)));
			} break;
			case Head::Type::symbol: {
				return name_lookup::build_symbol(store, infos, view);
			} break;
			case Head::Type::f_app: {
				StupidBufferVector<NodeIndex, 12> subterms;
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
				return NodeIndex(FApp::build(store, subterms), Literal::f_app);
			} break;
			case Head::Type::lambda: {
				const bool outermost_lambda = infos.lambda_params.size() == 0u;
				const std::size_t dot_pos = view.tokens.find_first_of(token::dot);
				if (dot_pos == TokenView::npos) [[unllikely]] {
					throw ParseFailure { view.offset, "there is no valid lambda without the definition preceeded by a dot" };
				}
				const std::int32_t prev_param_count = infos.lambda_params.size();
				const std::int32_t param_count = name_lookup::add_lambda_params(infos.lambda_params, view.steal_prefix(dot_pos));
				view.remove_prefix(1u); //remove dot

				const NodeIndex definition = parse::build(store, infos, view);
				const std::size_t res_index = store.allocate_one();
				const std::int32_t owned_depth = std::max(0, owned_lambda_depth(Ref(store, definition)));
				store.at(res_index) = Lambda{ param_count, definition, owned_depth, !outermost_lambda };
				infos.lambda_params.resize(prev_param_count); //remove own names again
				return NodeIndex(res_index, Literal::lambda);
			} break;
			default:
				assert(false);
				return invalid_index;
			}
		} //build
		template NodeIndex build(Store&, name_lookup::LiteralInfos&, ParseView);
		template NodeIndex build(Store&, name_lookup::PatternInfos&, ParseView);

		std::tuple<RuleHead, name_lookup::PatternInfos> raw_rule(Store& store, std::string& name, IAmInformedThisRuleIsNotUsableYet)
		{
			using namespace simp;
			using namespace simp;
			auto parse_str = ParseString(name);
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
			RuleHead heads;
			heads.lhs = parse::build(store, infos, lhs_view);
			infos.parse_match = false;
			heads.rhs = parse::build(store, infos, rhs_view); 

			heads.lhs = normalize::recursive(MutRef(store, heads.lhs), { .eval_special = false });
			heads.rhs = normalize::recursive(MutRef(store, heads.rhs), 
				{ .remove_unary_assoc = false, .eval_special = false, .eval_lambda = false });

			//extra condition concerning single match variables (might set multiple in relation)
			struct SingleCondition
			{
				NodeIndex head = invalid_index;
				//dependencies[i] is set iff single match i occurs in the condition
				std::bitset<match::State::max_single_match_count> dependencies = 0; 
			};
			std::vector<SingleCondition> single_conditions;

			std::array<NodeIndex, match::State::max_value_match_count> value_conditions;
			value_conditions.fill(from_native(nv::ComplexSubset::complex));

			while (conditions_view.size()) {
				assert(conditions_view.tokens.starts_with(token::bar) || conditions_view.tokens.starts_with(token::comma));
				conditions_view.remove_prefix(1u); //remove bar / comma
				const std::size_t comma = find_first_of_skip_pars(conditions_view.tokens, token::comma);
				const auto condition_view = conditions_view.steal_prefix(comma);
				const NodeIndex condition_head = normalize::recursive(
					MutRef(store, parse::build(store, infos, condition_view)), { .eval_special = false });
				if (condition_head.get_type() != Literal::f_app) [[unlikely]] {
					throw ParseFailure{ conditions_view.offset, "please make this condition look a bit more conditiony." };
				}

				const bool contains_single = ctrl::search<true>(store.data(), condition_head, 
					[](const UnsaveRef r) { return r.type == SingleMatch::weak; }) != invalid_index;

				const bool contains_value = ctrl::search<true>(store.data(), condition_head,
					[](const UnsaveRef r) { return r.typed_idx() == from_native(nv::PatternFn::value_match); }) != invalid_index;

				const bool contains_multi = ctrl::search<true>(store.data(), condition_head,
					[](const UnsaveRef r) { return r.type == SpecialMatch::multi; }) != invalid_index; 

				const bool contains_lambda = ctrl::search<true>(store.data(), condition_head,
						[](const UnsaveRef r) { return r.type == Literal::lambda; }) != invalid_index; 

				if (contains_multi || contains_lambda) [[unlikely]] {
					throw ParseFailure{ conditions_view.offset, "sorry, but i am too lazy to check that" };
				}
				if (contains_single && contains_value) [[unlikely]] {
					throw ParseFailure{ conditions_view.offset, "single match and value match may not depend on each other" };
				}
				if (contains_single) {
					struct {
						using Res = decltype(SingleCondition::dependencies);
						Res operator()(const UnsaveRef ref) {
							Res res = 0u;
							if (ref.type == Literal::f_app) {
								for (const NodeIndex sub : ref->f_app) {
									res |= (*this)(ref.at(sub));
								}
							}
							else if (ref.type == Literal::lambda) {
								res |= (*this)(ref.at(ref->lambda.definition));
							}
							else if (ref.type == SingleMatch::weak) {
								res.set(ref.index);
							}
							return res;
						}
					} list_singles;
					single_conditions.emplace_back(condition_head, list_singles(Ref(store, condition_head)));
				}
				if (contains_value) {
					const NodeIndex value_app_idx = ctrl::search<true>(store.data(), condition_head,
						[](const UnsaveRef r) { 
							return r.type == Literal::f_app && 
							r->f_app.function() == from_native(nv::PatternFn::value_match); 
						});
					assert(value_app_idx != invalid_index);
					FApp& value_app = *MutRef(store, value_app_idx);
					value_conditions[value_app[1].get_index()] = condition_head;
				}
				if (!contains_single && !contains_value) {
					throw ParseFailure{ conditions_view.offset - condition_view.size(), "this constraint does not constrain anything"};
				}
			} //end while
			{ //give value conditions to value match variables
				const auto restrict_value = [&value_conditions](const MutRef r) {
					if (r.type == Literal::f_app && 
						r->f_app.function() == from_native(nv::PatternFn::value_match) && //f_app of value_match__
						//if f_app contains no match variables itself and holds only shallow parameters, 
						//  it is assumed to stem from name_lookup::build_symbol
						std::none_of(r->f_app.begin(), r->f_app.end(),
							[](auto idx) { return idx.get_type().is<MatchVariableType>() || is_stored_node(idx.get_type()); }))
					{
						const std::size_t value_index = r->f_app[1].get_index();
						r->f_app[2] = value_conditions[value_index];
					}
				};
				ctrl::transform(MutRef(store, heads.lhs), restrict_value);
			}
			{
				//go through lhs in preorder, adjust single match using single_conditions
				//idea: keep track which variable has aleady been encountered. if a new single variable instance is met do the following:
				//if its index has already been encountered: done
				//if not: check if single_conditions contains conditions depending only on already encountered indices and the current one
				//           yes: combine these conditions into an "and" condition, append it at a RestrictedSingleMatch node
				//           no: turn Instance to SingleMatch::unrestricted
				decltype(SingleCondition::dependencies) encountered = 0;
				const auto empower_singles = [&encountered, &single_conditions](const MutRef r) {
					if (r.type == SingleMatch::weak && !encountered.test(r.index)) {
						encountered.set(r.index);
						std::vector<NodeIndex> relevant_conditions = {};
						for (const SingleCondition cond : single_conditions) {
							if (cond.dependencies.test(r.index) && (cond.dependencies & ~encountered).none()) {
								relevant_conditions.push_back(cond.head);
							}
						}
						if (relevant_conditions.size() == 0u) {
							return NodeIndex(r.index, SingleMatch::unrestricted);
						}
						if (relevant_conditions.size() == 1u) {
							const std::size_t res_index = r.store->allocate_one();
							r.store->at(res_index) = RestrictedSingleMatch{ r.index, relevant_conditions.front() };
							return NodeIndex(res_index, SingleMatch::restricted);
						}
						else { //more than one condition -> "and" them
							relevant_conditions.emplace(relevant_conditions.begin(), from_native(nv::Comm::and_));
							const std::size_t and_index = FApp::build(*r.store, relevant_conditions);
							const std::size_t res_index = r.store->allocate_one();
							r.store->at(res_index) = RestrictedSingleMatch{ r.index, NodeIndex(and_index, Literal::f_app) };
							return NodeIndex(res_index, SingleMatch::restricted);
						}
					}
					return r.typed_idx();
				};
				heads.lhs = ctrl::transform(MutRef(store, heads.lhs), empower_singles);
			}
			return { heads, infos };
		} //raw_rule

	} //namespace parse

	namespace typecheck {

		std::optional<std::string_view> pattern_var_name_collision(UnsaveRef const ref, parse::name_lookup::PatternInfos const& infos)
		{
			std::unordered_set<std::string_view> contained_symbols;
			ctrl::transform(ref, [&contained_symbols](UnsaveRef const r){
				if (r.type == Literal::symbol) {
					contained_symbols.insert(Names::name_of(r.index));
				}
			});

			using namespace std::string_view_literals;
			for (auto& [vector, prefix, suffix] : std::array{
				std::tuple{ infos.single_matches, "_"sv, ""sv },
				std::tuple{ infos.value_matches, "$"sv, ""sv },
				std::tuple{ infos.multi_matches, ""sv, "..."sv } })
			{
				for (auto [name, _value] : vector) {
					assert(name.starts_with(prefix));
					assert(name.ends_with(suffix));
					name.remove_prefix(prefix.size());
					name.remove_suffix(suffix.size());
					if (contained_symbols.contains(name)) {
						return { name };
					}
				}
			}
			return std::optional<std::string_view>();
		} //pattern_var_name_collision

		//implementation of consistent_pattern_vars
		void recursive_consistent_pattern_vars(UnsaveRef const ref, MatchVariableProps& props, 
			unsigned const visible_lambda_params, bool const in_rhs)
		{
			auto const set_visible = [&](int& nr_visible) {
				//one may not find a new variable in the rhs 
				//(already guaranteed by preceeding part of building process)
				assert(!in_rhs || nr_visible != -1); 

				if (nr_visible == -1) { //-> uninitialized
					nr_visible = visible_lambda_params;
				}
				else if (nr_visible > visible_lambda_params) {
					throw TypeError{ "if pattern variable is bound inside lambda, it may only occur in lambda in rhs", ref };
				}
			};

			switch (ref.type) {
			case NodeType(Literal::lambda):
				recursive_consistent_pattern_vars(ref.at(ref->lambda.definition), props, 
					visible_lambda_params + ref->lambda.param_count, in_rhs);
				break;
			case NodeType(PatternFApp{}): assert(false); break; //not expected to be build this far
			case NodeType(Literal::f_app):
				for (NodeIndex sub : ref) {
					recursive_consistent_pattern_vars(ref.at(sub), props, visible_lambda_params, in_rhs);
				}
				break;
			case NodeType(SingleMatch::restricted): 
				set_visible(props.singles[ref->single_match.match_state_index].lambda_depth);
				break;
			case NodeType(SingleMatch::unrestricted):
			case NodeType(SingleMatch::weak):
				set_visible(props.singles[ref.index].lambda_depth);
				break;
			case NodeType(SpecialMatch::multi): 
				set_visible(props.singles[ref->multi_match.match_state_index].lambda_depth);
				break;
			}
		} //recursive_gather_lhs_props

		void consistent_pattern_vars(UnsaveRef const ref, MatchVariableProps& props, bool const in_rhs)
		{
			recursive_consistent_pattern_vars(ref, props, 0, in_rhs);
		} //consistent_pattern_vars

	} //namespace typecheck

	namespace print {

		void append_real(double val, std::string& dest)
		{
			std::stringstream buffer;
			buffer << val;
			dest.append(buffer.str());
		}

		constexpr int sum_precedence = 20;

		void append_complex(const std::complex<double> val, std::string& dest, int parent_precedence)
		{
			std::stringstream buffer;
			const bool parentheses = [&] {
				if (val.real() != 0.0 && val.imag() != 0.0) {
					buffer << val.real() << std::showpos << val.imag() << 'i';
					return parent_precedence > sum_precedence;
				}
				else if (val.real() != 0.0 && val.imag() == 0.0) {
					buffer << val.real();
					return val.real() < 0.0 && parent_precedence >= sum_precedence;	//leading '-'
				}
				else if (val.real() == 0.0 && val.imag() != 0.0) {
					buffer << val.imag() << 'i';
					return val.imag() < 0.0 && parent_precedence >= sum_precedence;	//leading '-'	
				}
				buffer << '0';
				return false;
			}();

			if (parentheses) {
				dest.push_back('(');
				dest.append(buffer.str());
				dest.push_back(')');
			}
			else {
				dest.append(buffer.str());
			}
		} //append_complex

		void append_f_app_info(const FAppInfo& info, const std::size_t param_count, std::string& str)
		{
			const auto to_string = [](const auto bitset, const std::size_t size) {
				std::string string = std::bitset<32>(bitset).to_string();
				std::reverse(string.begin(), string.end());
				string.resize(size);
				return string;
			};
			str.append("[");
			switch (info.strategy) {
			case MatchStrategy::permutation: 
				str.append("p");
				str.append(std::to_string(info.match_state_index));
				str.append(" R:");
				str.append(to_string(info.rematchable_params, param_count));
				str.append(" M:");
				str.append(to_string(info.preceeded_by_multi, 1));
				if (param_count > 0) {
					str.append(" O:");
					str.append(to_string(info.always_preceeding_next, param_count - 1));
				}
				break;
			case MatchStrategy::dilation:    
				str.append("d");
				str.append(std::to_string(info.match_state_index));
				str.append(" R:");
				str.append(to_string(info.rematchable_params, param_count));
				str.append(" M:");
				str.append(to_string(info.preceeded_by_multi, param_count + 1));
				break;
			case MatchStrategy::identic: 
				str.append("b");
				str.append(" R:");
				str.append(to_string(info.rematchable_params, param_count));
				break;
			case MatchStrategy::linear:      
				str.append("l");
				break;
			}
			str.append("]");
		} //append_f_app_info

		


		void append_to_string(const UnsaveRef ref, std::string& str, const int parent_precedence, const bool fancy)
		{
			switch (ref.type) {
			case NodeType(Literal::complex):
				append_complex(*ref, str, parent_precedence);
				break;
			case NodeType(Literal::symbol):
				str.append(Names::name_of(ref.index));
				break;
			case NodeType(PatternFApp{}): 
			case NodeType(Literal::f_app): { 
				const bool in_pattern = ref.type.is<PatternFApp>();
				const FApp& f_app = *ref;
				const NodeIndex function = f_app.function();
				const auto [init, seperator, precedence] = [&]() -> std::tuple<const char*, const char*, int> {
					if (fancy && !in_pattern && function.get_type() == Literal::symbol) {
						using namespace nv;
						switch (to_symbol(function)) {
						case Symbol(ToBool::not_):          return { "!", ""    , 40 };
						case Symbol(CtoC::pow):             return { "" , "^"   , 30 };
						case Symbol(Comm::prod):            return { "" , " "   , 21 };
						case Symbol(Comm::sum):             return { "" , " + " , 20 }; static_assert(sum_precedence == 20);
						case Symbol(ToBool::eq):            return { "" , " == ", 10 };
						case Symbol(ToBool::neq):           return { "" , " != ", 10 };
						case Symbol(ToBool::greater):       return { "" , " > " , 10 };
						case Symbol(ToBool::smaller):       return { "" , " < " , 10 };
						case Symbol(ToBool::greater_eq):    return { "" , " >= ", 10 };
						case Symbol(ToBool::smaller_eq):    return { "" , " <= ", 10 };
						case Symbol(Comm::and_):            return { "" , " && ",  3 };
						case Symbol(Comm::or_):             return { "" , " || ",  2 };
						case Symbol(PatternFn::of_type):    return { "" , " :"  ,  1 };
						case Symbol(MiscFn::cons):          return { "" , " :: ",  1 };
						}
					}
					append_to_string(ref.at(function), str, 1000, fancy);
					if (in_pattern) {
						append_f_app_info(f_app_info(ref), ref->f_app.size() - 1u, str);
					}
					return { "", ", ", 0 };
				}();
				if (precedence <= parent_precedence) { str.push_back('('); }
				const char* spacer = init;
				for (const NodeIndex param : f_app.parameters()) {
					str.append(std::exchange(spacer, seperator));
					append_to_string(ref.at(param), str, precedence, fancy);
				}
				if (precedence <= parent_precedence) { str.push_back(')'); }
			} break;
			case NodeType(Literal::lambda): {
				const Lambda& lambda = *ref;
				str += lambda.transparent ? "(\\[" : "{\\[";
				str += std::to_string(lambda.param_count);
				str += "].";
				append_to_string(ref.at(lambda.definition), str, 1000, fancy);
				str += (lambda.transparent ? ')' : '}');
			} break;
			case NodeType(Literal::lambda_param):
				str.push_back('%');
				str.append(std::to_string(ref.index));
				break;
			case NodeType(SingleMatch::restricted): {
				const RestrictedSingleMatch& var = *ref;
				str.append("_X");
				str.append(std::to_string(var.match_state_index));
				str.append("[");
				append_to_string(ref.at(var.condition), str, 0, fancy);
				str.append("]");
			} break;				
			case NodeType(SingleMatch::unrestricted):
			case NodeType(SingleMatch::weak):
				str.append("_X");
				str.append(std::to_string(ref.index));
				str.append(ref.type == SingleMatch::weak ? "'" : "");
				break;
			case NodeType(SpecialMatch::multi): {
				const MultiMatch& var = *ref;
				str.append("_M[");
				str.append(std::to_string(var.match_state_index));
				str.append(", ");
				str.append(std::to_string((int)var.index_in_params));
				str.append("]");
			} break;
			case NodeType(SpecialMatch::value): {
				const ValueMatch& var = *ref;
				str.append("_V");
				str.append(std::to_string(var.match_state_index));
				str.append(var.owner ? "" : "'");
				str.append("[");
				append_to_string(ref.at(var.inverse), str, 0, fancy);
				if (var.domain != nv::ComplexSubset::complex) {
					str.append(", ");
					str.append(nv::name_of(var.domain));
				}
				str.append("]");
			} break;
			case NodeType(PatternUnsigned{}):
				str.append(std::to_string(ref.index));
				str.append("U");
				break;
			default:
				str.append("(ERROR:");
				str.append(std::to_string((unsigned)ref.type));
				str.append(":");
				str.append(std::to_string(ref.index));
				str.append(")");
			}
		} //append_to_string

		std::string literal_to_string(const UnsaveRef head, const bool fancy)
		{
			enum class Status { fst_app, later_app, tran_lam, intr_lam };

			struct StackFrame
			{
				ctrl::UnsaveRange range = { nullptr, nullptr };
				char const* infix = ""; 
				int precedence = 0;
				Status status = {};
			};

			StupidBufferVector<StackFrame, 32> stack;
			std::string str;
			NodeIndex next = invalid_index;

			const auto add_ref = [&](const UnsaveRef ref) 
			{
				assert(stack.size());
				switch (ref.type) {
				case NodeType(Literal::complex):
					append_complex(*ref, str, stack.back().precedence);
					break;
				case NodeType(Literal::symbol):
					str.append(Names::name_of(ref.index));
					break;
				case NodeType(Literal::f_app): {
					const FApp& f_app = *ref;
					const auto [infix, precedence] = [&]() -> std::tuple<const char*, int> {
						const NodeIndex fun = f_app.function();
						if (fancy && fun.get_type() == Literal::symbol) {
							using namespace nv;
							switch (to_symbol(fun)) {
							case Symbol(CtoC::pow):             return { "^"   , 30 };
							case Symbol(Comm::prod):            return { " "   , 21 };
							case Symbol(Comm::sum):             return { " + " , 20 }; static_assert(sum_precedence == 20);
							case Symbol(ToBool::eq):            return { " == ", 10 };
							case Symbol(ToBool::neq):           return { " != ", 10 };
							case Symbol(ToBool::greater):       return { " > " , 10 };
							case Symbol(ToBool::smaller):       return { " < " , 10 };
							case Symbol(ToBool::greater_eq):    return { " >= ", 10 };
							case Symbol(ToBool::smaller_eq):    return { " <= ", 10 };
							case Symbol(Comm::and_):            return { " && ",  3 };
							case Symbol(Comm::or_):             return { " || ",  2 };
							case Symbol(PatternFn::of_type):    return { " :"  ,  1 };
							case Symbol(MiscFn::cons):          return { " :: ",  1 };
							}
						}
						next = fun; //print fun first (not needed, if output is as operator)
						return { ", ", 0 };
					}();
					const ctrl::UnsaveRange range = { f_app.begin() + 1, f_app.end() }; // +1, because begin is applied function
					stack.emplace_back(range, infix, precedence, Status::fst_app);
				} break;
				case NodeType(Literal::lambda): {
					const Lambda& lambda = *ref;
					str += lambda.transparent ? "(\\[" : "{\\[";
					str += std::to_string(lambda.param_count);
					str += "].";
					const ctrl::UnsaveRange range = { &lambda.definition, &lambda.definition + 1 };
					stack.emplace_back(range, "", 1000, lambda.transparent ? Status::tran_lam : Status::intr_lam);
				} break;
				case NodeType(Literal::lambda_param):
					str.push_back('%');
					str.append(std::to_string(ref.index));
					break;
				default:
					str.append("(ERROR:");
					str.append(std::to_string((unsigned)ref.type));
					str.append(":");
					str.append(std::to_string(ref.index));
					str.append(")");
				}
			}; //lambda add_ref

			stack.emplace_back(StackFrame{});
			add_ref(head);

			while (stack.size() >= 2) {
				while (next != invalid_index) {
					add_ref(head.at(std::exchange(next, invalid_index)));
				}
				StackFrame& frame = stack.back();
				if (frame.range.size()) {
					if (frame.status == Status::fst_app) {
						if (frame.precedence <= stack[stack.size() - 2].precedence) {
							str += "(";
						}
						frame.status = Status::later_app;
					}
					else if (frame.status == Status::later_app) {
						str += frame.infix;
					}
					next = *(frame.range.iter++);
				}
				else {
					if (frame.status == Status::intr_lam) {
						str += "}";
					}
					else if (frame.status == Status::tran_lam || 
						frame.precedence <= stack[stack.size() - 2].precedence) 
					{
						str += frame.status == Status::fst_app ? "()" : ")";
					}
					stack.pop_back();
				}
			}

			return str;
		} //literal_to_string


		void append_memory_row(const UnsaveRef ref, std::vector<std::string>& rows)
		{
			if (!is_stored_node(ref.type)) return;

			std::string& current_str = rows[ref.index];
			if (current_str.size()) return;

			switch (ref.type) {
			case NodeType(Literal::complex): {
				current_str += "value      : ";
			} break;
			case NodeType(Literal::symbol): {
				current_str += "symbol     : ";
			} break;
			case NodeType(PatternFApp{}): {
				std::string& prev_str = rows[ref.index - 1u];
				prev_str += "meta info  : ";
				append_f_app_info(f_app_info(ref), ref->f_app.size() - 1u, prev_str);
			} [[fallthrough]];
			case NodeType(Literal::f_app): {
				current_str += "application:     { ";
				const char* separator = "";
				const FApp& vec = *ref;
				std::string* current_line = &current_str; //as we never add a new string to rows, this pointer will not dangle
				std::size_t vec_idx = 0u;
				const auto maybe_start_new_line = [&] {
					if ((vec_idx - FApp::min_capacity) % FApp::values_per_node == 0) {
						*(++current_line) += "  ...         ";
					}
				};
				for (; vec_idx < vec.size(); vec_idx++) {
					*current_line += std::exchange(separator, ", ");
					maybe_start_new_line();
					*current_line += std::format("{:3}", vec[vec_idx].get_index());
					print::append_memory_row(ref.at(vec[vec_idx]), rows);
				}
				*current_line += " }";
				for (; vec_idx < vec.capacity(); vec_idx++) {
					maybe_start_new_line();
				}
			} break;
			case NodeType(Literal::lambda): {
				const Lambda& lam= *ref;
				current_str += "lambda     : ";
				print::append_memory_row(ref.at(lam.definition), rows);
			} break;
			default:
				current_str += "unknown type: " + std::to_string((unsigned)ref.type);
			}

			//append name of subterm to line
			current_str.append(std::max(0, 38 - (int)current_str.size()), ' ');
			std::string subtree_as_string;
			print::append_to_string(ref, subtree_as_string, 0, true);
			if (subtree_as_string.size() > 50u) {
				subtree_as_string.erase(40);
				subtree_as_string.append("   .  .  .");
			}
			current_str.append(subtree_as_string);
		} //append_memory_row

		template<ContainerOf<NodeIndex> C>
		std::string to_memory_layout(const Store& store, const C& heads)
		{
			std::vector<std::string> rows(std::max(store.size(), match::State::max_pattern_f_app_count), "");

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
				std::vector<std::size_t> leaks;
				const BitVector used_positions = store.storage_occupancy();
				for (std::size_t i = 0; i < store.size(); i++) {
					//result += std::format("{:4} | {:4} | ", i, store.ref_count_at(i));
					result += std::format("{:4} | ", i, store.ref_count_at(i));
					result += rows[i];
					if (!used_positions.test(i)) {
						result += "-----free slot-----";
					}
					result += "\n";

					if (!rows[i].size() && used_positions.test(i)) {
						leaks.push_back(i);
					}

					if (!rows[i].size()) { //skip ofer large sections where nothing is used
						std::size_t next_used = i + 1u;
						while (next_used < store.size() && !used_positions.test(next_used)) next_used++;
						if (next_used - i > 5u) {
							result += "...\n";
							i = next_used - 2u; //after i++ in enclosing loop, i points at last unused slot
						}
					}
				} //end for
				if (leaks.size()) {
					result += "leaks at: ";
					const char* seperator = "";
					for (const std::size_t leak : leaks) {
						result += std::exchange(seperator, ", ");
						result += std::to_string(leak);
					}
					result += "\n";
				}
			}
			return result;
		} //to_memory_layout
		template std::string to_memory_layout(const Store&, const std::initializer_list<NodeIndex>&);
		template std::string to_memory_layout(const Store&, const std::vector<NodeIndex>&);

	} //namespace print

} //namespace simp

