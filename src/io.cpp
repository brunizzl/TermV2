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
					{ token::colon,        Arity::binary },
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
					case ':':  return Head::Type::of_type;
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
					literal_nullptr;
			}

			NodeIndex build_symbol(Store& store, const LiteralInfos& infos, bmath::intern::ParseView view)
			{
				const std::string_view name = view.to_string_view();
				if (!mundane_name(view)) [[unlikely]] {
					throw bmath::ParseFailure{ view.offset, "ellipses or the dollar symbol are only expected when building a pattern" };
				}
				if (const NodeIndex res = find_name_in_infos(infos.lambda_params, name); res != literal_nullptr) {
					return res;
				}
				if (const nv::Native type = nv::type_of(name); type != nv::Native(nv::Native::COUNT)) {
					return from_native(type);
				}
				return NodeIndex(Symbol::build(store, name), Literal::symbol);
			}

			NodeIndex build_symbol(Store& store, PatternInfos& infos, bmath::intern::ParseView view)
			{
				std::string_view name = view.to_string_view();
				if (const NodeIndex res = find_name_in_infos(infos.lambda_params, name); res != literal_nullptr) {
					return res;
				}
				if (const NodeIndex res = find_name_in_infos(infos.single_matches, name); res != literal_nullptr) {
					return res;
				}
				if (const NodeIndex res = find_name_in_infos(infos.multi_matches, name); res != literal_nullptr) {
					if (infos.parse_match) [[unlikely]] {
						throw bmath::ParseFailure{ view.offset, "multiple occurences of multi in lhs are forbidden" };
					}
					return copy_tree(Ref(store, res), store);
				}
				if (const NodeIndex res = find_name_in_infos(infos.value_matches, name); res != literal_nullptr) {
					return copy_tree(Ref(store, res), store);
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
				if (!infos.parse_match) [[unlikely]] {
					throw bmath::ParseFailure{ view.offset, "match variables need to be introduced in lhs" };
				}
				if (name.ends_with('.')) {
					const std::size_t res_index = store.allocate_one();
					store.at(res_index) = MultiMatch{ (unsigned)infos.multi_matches.size(), 0 };
					const NodeIndex result = NodeIndex(res_index, SpecialMatch::multi);
					infos.multi_matches.emplace_back(name, result);
					return result;
				}
				else if (name.starts_with('$')) {
					const std::size_t res_index = Call::build(store, std::to_array({
						from_native(nv::PatternAuxFn::value_match),               //function type
						NodeIndex(infos.value_matches.size(), PatternUnsigned{}), //.match_data_index
						from_native(nv::ComplexSubset::complex),                  //.domain
						value_proxy                                               //.match_index
					}));
					const NodeIndex result = NodeIndex(res_index, Literal::call);
					infos.value_matches.emplace_back(name, result);
					return result;
				}
				else {
					const NodeIndex result = NodeIndex(infos.single_matches.size(), SingleMatch::weak);
					infos.single_matches.emplace_back(name, result);
					return result;
				}
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
				store.at(res_index) = Call{ from_native(type), fst, snd };
				return NodeIndex(res_index, Literal::call);
			};
			const auto to_inverse_buildin_call = [&](const nv::Native type, auto invert_term) {
				const NodeIndex fst = parse::build(store, infos, view.substr(0, head.where));
				const NodeIndex snd_uninverted = build(store, infos, view.substr(head.where + 1));
				const NodeIndex snd = invert_term(store, snd_uninverted);
				const std::size_t res_index = store.allocate_one();
				store.at(res_index) = Call{ from_native(type), fst, snd };
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
			case Head::Type::of_type:    return to_buildin_call(1, nv::PatternAuxFn::of_type);
			case Head::Type::not_: {
				view.remove_prefix(1u);  //remove '!'
				const NodeIndex to_negate = parse::build(store, infos, view);
				const std::size_t res_index = store.allocate_one();
				store.at(res_index) = Call{ from_native(nv::ToBool::not_), to_negate };
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
				return literal_nullptr;
			}
		} //build
		template NodeIndex build(Store&, name_lookup::LiteralInfos&, bmath::intern::ParseView);
		template NodeIndex build(Store&, name_lookup::PatternInfos&, bmath::intern::ParseView);

		RuleHead raw_rule(Store& store, std::string& name, IAmInformedThisRuleIsNotUsableYet)
		{
			using namespace bmath;
			using namespace bmath::intern;
			auto parse_str = ParseString(name);
			{
				constexpr char allowed[] = { token::character, token::number, token::open_grouping,
					token::clse_grouping, token::unary_minus, token::sum, token::product, token::comma,
					token::hat, token::equals, token::bar, token::bang, token::space, token::imag_unit,
					token::backslash, token::dot, token::relation, token::and_, token::or_, token::colon, '\0' };

				if (const std::size_t pos = parse_str.tokens.find_first_not_of(allowed); pos != TokenString::npos) [[unlikely]] {
					throw ParseFailure{ pos, "unexpected character" };
				}
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
			RuleHead heads;
			heads.lhs = parse::build(store, infos, lhs_view);
			infos.parse_match = false;
			heads.rhs = parse::build(store, infos, rhs_view); 

			heads.lhs = normalize::recursive(MutRef(store, heads.lhs), {}, 0);
			heads.lhs = normalize::recursive(MutRef(store, heads.lhs), 
				{ .eval_haskell = false, .remove_unary_assoc = false }, 0);

			//extra condition concerning single match variables (might set multiple in relation)
			struct SingleCondition
			{
				NodeIndex head = literal_nullptr;
				std::bitset<match::MatchData::max_single_match_count> dependencies = 0; //dependencies[i] is set iff single match i occurs in the condition
			};
			std::vector<SingleCondition> single_conditions;

			std::array<nv::ComplexSubset, match::MatchData::max_value_match_count> value_conditions;
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
					[](const UnsaveRef r) { return r.type == SingleMatch::weak; }) != literal_nullptr;
				const bool contains_value = simp::search(cond_ref,
					[](const UnsaveRef r) { return r.typed_idx() == from_native(nv::PatternAuxFn::value_match); }) != literal_nullptr;
				const bool contains_multi = simp::search(cond_ref,
					[](const UnsaveRef r) { return r.type == SpecialMatch::multi; }) != literal_nullptr; 
				const bool contains_lambda = simp::search(cond_ref,
						[](const UnsaveRef r) { return r.type == Literal::lambda; }) != literal_nullptr; 
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
							if (ref.type == Literal::call) {
								for (const NodeIndex sub : ref->call) {
									res |= (*this)(ref.new_at(sub));
								}
							}
							if (ref.type == SingleMatch::weak) {
								res.set(ref.index);
							}
							return res;
						}
					} list_singles;
					single_conditions.emplace_back(condition_head, list_singles(cond_ref));
				}
				if (contains_value) {
					//condition may only be of form "type($<value match>, <complex subset>)"
					//if so: adjust value_conditions at right index
					if (cond_ref.type != Literal::call ||
						cond_ref->call.function() != from_native(nv::PatternAuxFn::of_type))
					{
						throw ParseFailure{ conditions_view.offset, "value match may only have its type restricted" };
					}
					const NodeIndex subset = cond_ref->call[2];
					const NodeIndex value_match_call_idx = cond_ref->call[1];
					if (value_match_call_idx.get_type() != Literal::call || !to_native(subset).is<nv::ComplexSubset>()) {
						throw ParseFailure{ conditions_view.offset, "value match may only have its type as real, nat etc." };
					}
					Call& value_match_call = *MutRef(store, value_match_call_idx);
					if (value_match_call.function() != from_native(nv::PatternAuxFn::value_match)) {
						throw ParseFailure{ conditions_view.offset, "uh oh to funky for me" };
					}
					value_conditions[value_match_call[1].get_index()] = to_native(subset).to<nv::ComplexSubset>();
					free_tree(cond_ref);
				}
			}
			{
				struct {
					decltype(value_conditions) const& domains;
					Store& store_;

					NodeIndex operator()(const MutRef ref) {
						if (ref.type == Literal::call) {
							if (ref->call.function() == from_native(nv::PatternAuxFn::value_match) && //call to _VM
								//if call contains no match variables itself and holds only shallow parameters, 
								//  it is assumed to stem from name_lookup::build_symbol
								std::none_of(ref->call.begin(), ref->call.end(), 
									[](auto idx) { return idx.get_type().is<MatchVariableType>() || is_stored_node(idx.get_type()); }))
							{
								const std::size_t value_index = ref->call[1].get_index();
								ref->call[2] = from_native(this->domains[value_index]);
							}
							else {
								for (NodeIndex& sub : ref) {
									sub = (*this)(ref.new_at(sub));
								}
							}
						}
						return ref.typed_idx();
					}
				} build_value_matches = { value_conditions, store };
				heads.lhs = build_value_matches(MutRef(store, heads.lhs));
			}
			{
				//go through lhs in preorder, adjust single match using single_conditions
				//idea: keep track which variable has aleady been encountered. if a new single variable instance is met do the following:
				//if its index has already been encountered: done
				//if not: check if single_conditions contains conditions depending only on already encountered indices and the current one
				//           yes: combine these conditions into an "and" condition, append it at a RestrictedSingleMatch node
				//           no: turn Instance to SingleMatch::unrestricted
				decltype(SingleCondition::dependencies) encountered_singles = 0;
				struct {
					decltype(SingleCondition::dependencies)& encountered;
					const std::vector<SingleCondition>& conditions;
					Store& store_;

					NodeIndex operator()(const MutRef ref) {
						if (ref.type == Literal::call) {
							const auto stop = end(ref);
							for (auto iter = begin(ref); iter != stop; ++iter) { //important: dont skip function!
								*iter = (*this)(ref.new_at(*iter));
							}
						}
						if (ref.type == SingleMatch::weak && !this->encountered.test(ref.index)) {
							this->encountered.set(ref.index);
							std::vector<NodeIndex> relevant_conditions = {};
							for (const SingleCondition cond : this->conditions) {
								if (cond.dependencies.test(ref.index) && (cond.dependencies & ~this->encountered).none()) {
									relevant_conditions.push_back(cond.head);
								}
							}
							if (relevant_conditions.size() == 0u) {
								return NodeIndex(ref.index, SingleMatch::unrestricted);
							}
							if (relevant_conditions.size() == 1u) {
								const std::size_t res_index = this->store_.allocate_one();
								this->store_.at(res_index) = RestrictedSingleMatch{ ref.index, relevant_conditions.front() };
								return NodeIndex(res_index, SingleMatch::restricted);
							}
							else { //more than one condition -> "and" them
								relevant_conditions.emplace(relevant_conditions.begin(), from_native(nv::Comm::and_));
								const std::size_t and_index = Call::build(this->store_, relevant_conditions);
								const std::size_t res_index = this->store_.allocate_one();
								this->store_.at(res_index) = RestrictedSingleMatch{ ref.index, NodeIndex(and_index, Literal::call) };
								return NodeIndex(res_index, SingleMatch::restricted);
							}
						}
						return ref.typed_idx();
					}
				} build_single_matches = { encountered_singles, single_conditions, store };
				heads.lhs = build_single_matches(MutRef(store, heads.lhs));
			}
			return heads;
		} //raw_rule

	} //namespace parse

	namespace print {

		constexpr int max_infixr = std::numeric_limits<int>::max();
		constexpr int default_infixr = 0;

		constexpr int infixr(const NodeIndex f) {
			if (f.get_type() != Literal::native) { return default_infixr; }
			using namespace nv;
			switch (to_native(f)) {
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

		void append_pattern_meta_data(const PatternCallData& data, std::string& str)
		{
			str.append("[");
			str.append(std::to_string(data.match_data_index));
			str.append("]");
		}

		void append_to_string(const UnsaveRef ref, std::string& str, const int parent_infixr)
		{
			bool print_this_operator = true;
			switch (ref.type) {
			case NodeType(Literal::complex):
				bmath::intern::print::append_complex(*ref, str, parent_infixr);
				break;
			case NodeType(Literal::symbol):
				str.append(ref->symbol);
				break;
			case NodeType(PatternCall{}): 
				append_pattern_meta_data(pattern_call_info(ref), str);
				print_this_operator = false;
				[[fallthrough]];				
			case NodeType(Literal::call): { 
				const Call& call = *ref;
				const NodeIndex function = call.function();
				const char* replacement_seperator = nullptr;
				const auto [init, seperator] = [&]() -> std::pair<const char*, const char*> {
					using namespace nv;
					if (function.get_type() == Literal::native && print_this_operator) {
						switch (to_native(function)) {
						case Native(Comm::sum):             return { "" , " + "  };
						case Native(Comm::product):         return { "" , " * "  };
						case Native(Comm::and_):            return { "" , " && " };
						case Native(Comm::or_):             return { "" , " || " };
						case Native(CtoC::pow):             return { "" , " ^ "  };
						case Native(ToBool::eq):            return { "" , " == " };
						case Native(ToBool::neq):           return { "" , " != " };
						case Native(ToBool::greater):       return { "" , " > "  };
						case Native(ToBool::smaller):       return { "" , " < "  };
						case Native(ToBool::greater_eq):    return { "" , " >= " };
						case Native(ToBool::smaller_eq):    return { "" , " <= " };
						case Native(ToBool::not_):          return { "!", ""     };
						case Native(PatternAuxFn::of_type): return { "" , " :"   };
						}
					}
					append_to_string(ref.new_at(function), str, max_infixr);
					return { "", ", " };
				}();
				const int own_infixr = print_this_operator ? infixr(function) : default_infixr;
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
				str.append(lambda.transparent ? "(\\" : "{\\");
				append_to_string(ref.new_at(lambda.definition), str, max_infixr);
				str.push_back(lambda.transparent ? ')' : '}');
			} break;
			case NodeType(Literal::lambda_param):
				str.push_back('%');
				str.append(std::to_string(ref.index));
				break;
			case NodeType(SingleMatch::restricted): {
				const RestrictedSingleMatch& var = *ref;
				str.append("_X");
				str.append(std::to_string(var.match_data_index));
				str.append("[");
				append_to_string(ref.new_at(var.condition), str, default_infixr);
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
				str.append(std::to_string(var.match_data_index));
				str.append(", ");
				str.append(std::to_string((int)var.index_in_params));
				str.append("]");
			} break;
			case NodeType(SpecialMatch::value): {
				const ValueMatch& var = *ref;
				str.append("_V");
				str.append(std::to_string(var.match_data_index));
				str.append("[");
				append_to_string(ref.new_at(var.match_index), str, default_infixr);
				str.append("]");
			} break;
			case NodeType(PatternUnsigned{}):
				str.append(std::to_string(ref.index));
				str.append("U");
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
				append_pattern_meta_data(pattern_call_info(ref), prev_str);
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
				current_str += "unknown type: " + std::to_string((unsigned)ref.type);
			}

			//append name of subterm to line
			current_str.append(std::max(0, 38 - (int)current_str.size()), ' ');
			print::append_to_string(ref, current_str, 0);
		} //append_memory_row

		std::string to_memory_layout(const Store& store, const std::initializer_list<const NodeIndex> heads)
		{
			std::vector<std::string> rows(std::max(store.size(), match::MatchData::max_pattern_call_count), "");

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

