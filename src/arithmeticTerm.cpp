
#include <cassert>
#include <sstream>
#include <array>
#include <algorithm>
#include <cstring>
#include <cfenv>
#include <compare>

#include <iostream>

#include "termUtility.hpp"
#include "arithmeticTerm.hpp"
#include "termColony.hpp"
#include "ioArithmetic.hpp"

/*
	template<typename Union_T, typename Type_T>
	void prototype(const BasicRef<Union_T,Type_T> ref)
	{
		using TypedIdx = BasicTypedIdx<Type_T>;
		using TypedIdxSLC = TermSLC<TypedIdx>;
		constexpr bool pattern = std::is_same_v<Type_T, Type>;

		switch (ref.type) {
		case Type_T(Op::sum): 
			[[fallthrough]];
		case Type_T(Op::product): {
			for (const TypedIdx elem : vc::range(ref)) {
			}
			assert(false);
		} break;
		case Type_T(Op::named_fn): {
			for (const TypedIdx param : fn::range(ref)) {
			}
			assert(false);
		} break;
		default: {
			assert(ref.type.is<Fn>());
			for (const TypedIdx param : fn::range(ref->fn_params, ref.type)) {
			}
			assert(false);
		} break;
		case Type_T(Leaf::variable): {
			assert(false);
		} break;
		case Type_T(Leaf::complex): {
			assert(false);
		} break;
		case Type_T(PnNode::tree_match): if constexpr (pattern) {
			assert(false);
		} break;
		case Type_T(PnNode::value_match): if constexpr (pattern) {
			pattern::ValueMatchVariable& var = *ref;
			assert(false);
		} break;
		case Type_T(PnNode::value_proxy):
			assert(false);
			break;
		case Type_T(MultiPn::summands):
			assert(false);
			break;
		case Type_T(MultiPn::factors):
			assert(false);
			break;
		}
		assert(false);
		return;
	} //prototype
*/

namespace bmath::intern {

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////local definitions//////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	//if one pattern may compare equal to a term multiple ways (e.g. sum or product), it has high rematchability.
	//there are only 3 real levels of rematchability:
	//  - none    (value 1xx): pattern is not recursive 
	//  - unknown (value 2xx): pattern is recursive, but has strong operands order (e.g. all in Fn), thus can not rematch on outhermost level, but may hold sum / product as operand
	//  - likely  (value 3xx): pattern is sum or product and is rematchable, as long as two or more tree_match variables are held as operands directly
	//by sorting sum and product to the end, they are matched last in match::permutation_equals, 
	//  thus likely already having their tree_match operands assocciated with something and only permitting up to a single match.
	//this approach guarantees a possible match to succeed, if a pattern has only up to a single sum / product one level below the root and none deeper.
	//side note: as every type has a unique rematchability value, sorting by rematchability if types are different produces a strong order.
	constexpr auto unique_rematchability_table = std::to_array<std::pair<Type, int>>({
		{ Type(Leaf::complex           ), 100 }, 
		{ Type(PnNode::value_match      ), 101 }, 
		{ Type(PnNode::value_proxy), 102 }, 
		{ Type(PnNode::tree_match       ), 103 }, 
		{ Type(Leaf::variable          ), 104 },
		{ Type(Op::named_fn            ), 299 },
		{ Type(Op::sum                 ), 300 },  
		{ Type(Op::product             ), 301 }, 
		{ Type(MultiPn::summands    ), 302 }, //kinda special, as they always succeed in matching -> need to be matched last 
		{ Type(MultiPn::factors     ), 303 }, //kinda special, as they always succeed in matching -> need to be matched last 
	});
	static_assert(std::is_sorted(unique_rematchability_table.begin(), unique_rematchability_table.end(), [](auto a, auto b) { return a.second < b.second; }));
	static_assert(unsigned(Fn::COUNT) < 99u); //else named_fn's rematchability of 300 is already occupied by element of Fn

	constexpr int rematchability(Type type) noexcept 
	{ 
		if (type.is<Fn>()) {
			return 200 + static_cast<unsigned>(type.to<Fn>());
		}
		else {
			return find(unique_rematchability_table, &std::pair<Type, int>::first, type).second; 
		}
	}

	//utility for both Function and NamedFn
	namespace fn {

		OptComplex eval(Fn type, const std::array<OptComplex, 4>& param_vals)
		{
			if (param_count(type) == 1u) {
				if (param_vals[0]->imag() == 0.0) {
					const double real_val = param_vals[0]->real();
					switch (type) {
					case Fn::asinh: return std::asinh(real_val);
					case Fn::acosh: return (         real_val  >= 1.0 ? std::acosh(real_val) : std::acosh(*param_vals[0]));
					case Fn::atanh: return (std::abs(real_val) <= 1.0 ? std::atanh(real_val) : std::atanh(*param_vals[0]));
					case Fn::asin : return (std::abs(real_val) <= 1.0 ?  std::asin(real_val) :  std::asin(*param_vals[0]));
					case Fn::acos : return (std::abs(real_val) <= 1.0 ?  std::acos(real_val) :  std::acos(*param_vals[0]));
					case Fn::atan : return std::atan (real_val);
					case Fn::sinh : return std::sinh (real_val);
					case Fn::cosh : return std::cosh (real_val);
					case Fn::tanh : return std::tanh (real_val);
					case Fn::sqrt : return (         real_val  >= 0.0 ?  std::sqrt(real_val) :  std::sqrt(*param_vals[0]));
					case Fn::exp  : return std::exp  (real_val);
					case Fn::sin  : return std::sin  (real_val);
					case Fn::cos  : return std::cos  (real_val);
					case Fn::tan  : return std::tan  (real_val);
					case Fn::abs  : return std::abs  (real_val);
					case Fn::arg  : return std::arg  (real_val);
					case Fn::ln   : return std::log  (real_val);
					case Fn::re   : return real_val;
					case Fn::im   : return 0.0;
					default: assert(false);
					}
				}
				else {
					switch (type) {
					case Fn::asinh: return std::asinh(*param_vals[0]);
					case Fn::acosh: return std::acosh(*param_vals[0]);
					case Fn::atanh: return std::atanh(*param_vals[0]);
					case Fn::asin : return std::asin (*param_vals[0]);
					case Fn::acos : return std::acos (*param_vals[0]);
					case Fn::atan : return std::atan (*param_vals[0]);
					case Fn::sinh : return std::sinh (*param_vals[0]);
					case Fn::cosh : return std::cosh (*param_vals[0]);
					case Fn::tanh : return std::tanh (*param_vals[0]);
					case Fn::sqrt : return std::sqrt (*param_vals[0]);
					case Fn::exp  : return std::exp  (*param_vals[0]);
					case Fn::sin  : return std::sin  (*param_vals[0]);
					case Fn::cos  : return std::cos  (*param_vals[0]);
					case Fn::tan  : return std::tan  (*param_vals[0]);
					case Fn::abs  : return std::abs  (*param_vals[0]);
					case Fn::arg  : return std::arg  (*param_vals[0]);
					case Fn::ln   : return std::log  (*param_vals[0]);
					case Fn::re   : return std::real (*param_vals[0]);
					case Fn::im   : return std::imag (*param_vals[0]);
					default: assert(false);
					}
				}
			}
			else if (param_count(type) == 2u) {
				if (param_vals[0]->imag() == 0.0 && param_vals[1]->imag() == 0.0) {
					const double real_0 = param_vals[0]->real();
					const double real_1 = param_vals[1]->real();
					switch (type) {
					case Fn::pow  : return (real_1 == 0.5 ? std::sqrt(real_0) : std::pow(real_0, real_1));
					case Fn::log  : return std::log(real_1) / std::log(real_0);
					default: assert(false);
					}
				}
				else if (param_vals[0]->imag() == 0.0) {
					const double real_0 = param_vals[0]->real();
					switch (type) {
					case Fn::pow  : return std::pow(real_0, *param_vals[1]);
					case Fn::log  : return std::log(*param_vals[1]) / std::log(real_0); 
					default: assert(false);
					}
				}
				else if (param_vals[1]->imag() == 0.0) {
					const double real_1 = param_vals[1]->real();
					switch (type) {
					case Fn::pow  : return (real_1 == 0.5 ? std::sqrt(*param_vals[0]) : std::pow(*param_vals[0], real_1));
					case Fn::log  : return std::log(real_1) / std::log(*param_vals[0]); 
					default: assert(false);
					}
				}
				else {
					switch (type) {
					case Fn::pow  : return std::pow(*param_vals[0], *param_vals[1]);
					case Fn::log  : return std::log(*param_vals[1]) / std::log(*param_vals[0]); //https://en.wikipedia.org/wiki/Complex_logarithm#Generalizations
					default: assert(false);
					}
				}
			}
			assert(false);
			return {};
		} //eval

	} //namespace fn

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////exported in header/////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	namespace pattern {

		bool meets_restriction(const Ref ref, const Restriction restr)
		{
			switch (restr) {
			case Restriction(Restr::any):
				return true;
			case Restriction(Restr::no_val):
				return ref.type != Leaf::complex;
			case Restriction(Restr::nn1):
				return (ref.type != Leaf::complex) || (ref->complex != -1.0);
			case Restriction(Restr::function):
				return ref.type.is<Op>() || ref.type.is<Fn>();
			default:
				assert(restr.is<MathType>());
				return restr == ref.type;
			}
		} //meets_restriction

		bool has_form(const Complex& nr, const Form form)
		{
			constexpr double max_save_int = 9007199254740991; //== 2^53 - 1, largest integer explicitly stored in double

			const double re = nr.real();
			const double im = nr.imag();

			bool accept = true;
			switch (form) {
			case Form::natural:   accept &= re >  0.0;                      [[fallthrough]];
			case Form::natural_0: accept &= re >= 0.0;                      [[fallthrough]];
			case Form::integer:   accept &= re - std::int64_t(re) == 0.0; 
			                      accept &= (std::abs(re) <= max_save_int); [[fallthrough]];
			case Form::real:      accept &= im == 0.0;                      [[fallthrough]];
			case Form::complex:
				return accept;

			case Form::negative:      return re <   0.0 && im == 0.0;
			case Form::positive:      return re >   0.0 && im == 0.0;
			case Form::not_negative:  return re >=  0.0 && im == 0.0;
			case Form::not_positive:  return re <=  0.0 && im == 0.0;
			default:
				assert(false);
				return false;
			}
		} //has_form

		PnTerm::PnTerm(std::string name)
		{
			auto parse_string = ParseString(name);
			parse_string.allow_implicit_product();
			parse_string.remove_space();
			const auto parts = PatternParts(parse_string);
			auto table = NameLookupTable(parts.declarations);
			throw_if(table.tree_table.size() > MatchData::max_tree_match_count, "too many tree_match match variables declared");
			throw_if(table.value_table.size() > MatchData::max_value_match_count, "too many value match variables declared");
			throw_if(table.multi_table.size() > MatchData::max_multi_match_count, "too many multi match variables declared");
			PatternBuildFunction build_function = { table };
			decltype(PnTerm::lhs_store) lhs_temp; //exists, because actions like rearrange_value_match might produce free slots in store.
			decltype(PnTerm::rhs_store) rhs_temp; //exists, because actions like rearrange_value_match might produce free slots in store.
			this->lhs_head = build_function(lhs_temp, parts.lhs);
			table.build_lhs = false;
			this->rhs_head = build_function(rhs_temp, parts.rhs);

			for (const auto& value : table.value_table) {
				for (const auto lhs_instance : value.lhs_instances) {
					pn_tree::rearrange_value_match(lhs_temp, this->lhs_head, lhs_instance);
				}
				for (const auto rhs_instance : value.rhs_instances) {
					pn_tree::rearrange_value_match(rhs_temp, this->rhs_head, rhs_instance);
				}
			}
			//establish basic order after rearanging value match to allow constructs 
			//  like "a :real, b | (a+2)+b = ..." to take summands / factors into their value_match match part
			this->lhs_head = tree::establish_basic_order(MutRef(lhs_temp, this->lhs_head));
			this->rhs_head = tree::establish_basic_order(MutRef(rhs_temp, this->rhs_head));

			for (const auto& multi_match : table.multi_table) {
				throw_if(multi_match.lhs_count > 1u, "pattern only allows single use of each Multimatch in lhs.");
				throw_if(multi_match.rhs_count > 1u, "pattern only allows single use of each Multimatch in rhs.");
			}

			//if params occurs in variadic, it is replaced py legal and matching MultiPn version.
			//if params occurs in Fn, true is returned (as term is illegal and can not be made legal)
			const auto contains_illegal_params = [](const MutRef head) -> bool {
				const auto inspect_branches = [](const MutRef ref) -> fold::FindBool {
					if (ref.type == Op::sum || ref.type == Op::product) {
						const Type result_type = ref.type == Op::sum ? MultiPn::summands : MultiPn::factors;
						for (TypedIdx& elem : vc::range(ref)) {
							if (elem.get_type() == MultiPn::params) {
								elem = TypedIdx(elem.get_index(), result_type); //params can convert to summands / factors
							}
						}
					}
					if (ref.type.is<Fn>()) {
						for (const TypedIdx param : fn::range(ref->fn_params, ref.type)) {
							if (param.get_type() == MultiPn::params) {
								return true; //found illegal
							}
						}
					}
					return false;
				};
				return fold::simple_fold<fold::FindBool>(head, inspect_branches);
			};

			throw_if(contains_illegal_params(MutRef(lhs_temp, this->lhs_head)), "pattern variable of type params may not occur in Fn.");
			throw_if(contains_illegal_params(MutRef(rhs_temp, this->rhs_head)), "pattern variable of type params may not occur in Fn.");

			const auto contains_illegal_value_match = [](const Ref head) -> bool {
				const auto inspect_variadic = [](const Ref ref) -> fold::FindBool {
					if (ref.type == Op::sum || ref.type == Op::product) {
						std::size_t nr_value_matches = 0u;
						for (const TypedIdx elem : vc::range(ref)) {
							nr_value_matches += (elem.get_type() == PnNode::value_match);
						}
						return nr_value_matches > 1u;
					}
					return false;
				};
				return fold::simple_fold<fold::FindBool>(head, inspect_variadic);
			};

			throw_if(contains_illegal_value_match(Ref(lhs_temp, this->lhs_head)), "no two value match variables may share the same sum / product in lhs.");

			const auto contains_illegal_multi_match = [](const Ref head) -> bool {
				const auto inspect_variadic = [](const Ref ref) -> fold::FindBool {
					if (ref.type == Op::sum || ref.type == Op::product) {
						std::size_t nr_multi_matches = 0u;
						for (const TypedIdx elem : vc::range(ref)) {
							nr_multi_matches += elem.get_type().is<MultiPn>();
						}
						return nr_multi_matches > 1u;
					}
					return false;
				};
				return fold::simple_fold<fold::FindBool>(head, inspect_variadic);
			};

			throw_if(contains_illegal_multi_match(Ref(lhs_temp, this->lhs_head)), "no two multi match variables may share the same sum / product in lhs.");

			this->lhs_store.reserve(lhs_temp.nr_used_slots());
			this->rhs_store.reserve(rhs_temp.nr_used_slots());
			this->lhs_head = tree::copy(Ref(lhs_temp, this->lhs_head), this->lhs_store);
			this->rhs_head = tree::copy(Ref(rhs_temp, this->rhs_head), this->rhs_store);
		}

		std::string PnTerm::to_string() const
		{
			std::string str;
			print::append_to_string(this->lhs_ref(), str);
			str.append(" = ");
			print::append_to_string(this->rhs_ref(), str);
			return str;
		}

		std::string PnTerm::lhs_memory_layout() const
		{
			return print::to_memory_layout(this->lhs_store, { this->lhs_head });
		}

		std::string PnTerm::rhs_memory_layout() const
		{
			return print::to_memory_layout(this->rhs_store, { this->rhs_head });
		}

		std::string PnTerm::lhs_tree(const std::size_t offset) const
		{
			return print::to_tree(this->lhs_ref(), offset);
		}

		std::string PnTerm::rhs_tree(const std::size_t offset) const
		{
			return print::to_tree(this->rhs_ref(), offset);
		}

		namespace pn_tree {

			TypedIdx* find_value_match_subtree(Store& store, TypedIdx& head, const TypedIdx value)
			{
				struct MatchTraits
				{
					bool has_match = false; //true if subterm contains value_match
					bool computable = true; //more fitting name would be "computable if value_match would not present"

					constexpr void combine(const MatchTraits snd) noexcept 
					{ 
						this->has_match |= snd.has_match; 
						this->computable &= snd.computable; 
					}
				}; //struct MatchTraits	

				//Yaaa in kow. Big O hates this implementation. I tried it in efficient and it looked so mutch worse. this is better. trust me.
				const auto classify_subterm = [&store, value](const TypedIdx head) -> MatchTraits {
					struct OpAccumulator
					{
						MatchTraits acc;

						constexpr OpAccumulator(const Ref ref, const TypedIdx value, const TypedIdx value_proxy) 
							:acc({ .has_match = false, .computable = true })
						{
							switch (ref.type) {
							case Type(Op::sum):     break;
							case Type(Op::product): break;
							case Type(Fn::pow):     break;// for now only allow these Fn to be computed in value
							case Type(Fn::sqrt):    break;// for now only allow these Fn to be computed in value  
							default:
								assert(ref.type.is<Fn>()); 
								[[fallthrough]];
							case Type(Op::named_fn):
								this->acc = MatchTraits{ .has_match = false, .computable = false };
								break;
							case Type(PnNode::value_match): {
								const bool is_right_match = TypedIdx(ref.index, ref.type) == value;
								this->acc = MatchTraits{ .has_match = is_right_match, .computable = is_right_match };
								break;
							}
							}
						} //ctor

						constexpr void consume(const MatchTraits elem_res) { this->acc.combine(elem_res); }
						constexpr MatchTraits result() const noexcept { return this->acc; }
					}; //struct OpAccumulator

					const auto leaf_apply = [](const Ref ref) -> MatchTraits {
						return MatchTraits{ false, is_one_of<Leaf::complex, PnNode::value_proxy>(ref.type) };
					};

					const ValueMatchVariable& var = store.at(value.get_index()).value_match;
					assert(var.copy_idx == var.mtch_idx);

					return fold::tree_fold<MatchTraits, OpAccumulator>(Ref(store, head), leaf_apply, value, var.copy_idx);
				}; //classify_subterm

				{
					const MatchTraits this_traits = classify_subterm(head);
					if (this_traits.computable && this_traits.has_match) {
						return &head;
					}
					else if (!this_traits.has_match) {
						return nullptr;
					}
				}

				const auto [index, type] = head.split();
				switch (type) {
				case Type(Op::sum): 
					[[fallthrough]];
				case Type(Op::product): {
					for (TypedIdx& elem : vc::range(MutRef(store, head))) {
						if (TypedIdx* const elem_res = find_value_match_subtree(store, elem, value)) {
							return elem_res;
						}
					}
				} break;
				case Type(Op::named_fn): {
					for (TypedIdx& param : fn::range(MutRef(store, head))) {
						if (TypedIdx* const elem_res = find_value_match_subtree(store, param, value)) {
							return elem_res;
						}
					}
				} break;
				default: {
					assert(type.is<Fn>()); //if this assert hits, the switch above needs more cases.
					FnParams& params = store.at(index).fn_params;
					for (TypedIdx& param : fn::range(params, type)) {
						if (TypedIdx* const elem_res = find_value_match_subtree(store, param, value)) {
							return elem_res;
						}
					}
				} break;
				case Type(Leaf::variable): 
					break;
				case Type(Leaf::complex): 
					break;
				case Type(PnNode::tree_match): 
					break;
				case Type(PnNode::value_match): 
					break;
				case Type(PnNode::value_proxy):
					break;
				case Type(MultiPn::summands):
					break;
				case Type(MultiPn::factors):
					break;
				case Type(MultiPn::params):
					break;
				}
				return nullptr;
			} //find_value_match_subtree

			void rearrange_value_match(Store& store, TypedIdx& head, const TypedIdx value_match)
			{
				using VarRef = BasicNodeRef<TypesUnion, ValueMatchVariable, Const::no>;

				TypedIdx* const value_match_subtree = find_value_match_subtree(store, head, value_match);
				TypedIdx* const value_match_storage = tree::find_subtree_owner(store, head, value_match);

				if (value_match_storage != value_match_subtree) { //else value owns just itself, no nodes upstream
					const VarRef var = VarRef(store, value_match.get_index());
					const TypedIdx proxy_value = var->copy_idx;
					assert(var->copy_idx == var->mtch_idx);

					var->copy_idx = *value_match_subtree; //keep the nodes now owned by value in current arrangement as tree to copy
					*value_match_storage = proxy_value; //value_match_storage is now owned by value. it would break the tree structure to leave the reference to itself there
					*value_match_subtree = value_match; //previous owner of subtree now belonging to value becomes owner of value itself (value now bubbled up)

					const TypedIdx match_data = tree::copy(Ref(store, var->copy_idx), store); //this and the following step might invalidate pointers into store data
					const auto [new_match_data, new_match_idx] = stupid_solve_for(store, { match_data, proxy_value }, proxy_value); //rhs starts with just proxy_value
					assert(new_match_data == proxy_value); //all terms around old position of value have been reversed around new_match_idx -> lhs should only have proxy left
					var->mtch_idx = new_match_idx;

					var->mtch_idx = tree::establish_basic_order(MutRef(store, var->mtch_idx));
				}
			} //rearrange_value_match

			Equation stupid_solve_for(Store& store, Equation eq, const TypedIdx to_isolate)
			{
				assert(tree::contains(Ref(store, eq.lhs_head), to_isolate));

				while (eq.lhs_head != to_isolate) {

					const auto [lhs_index, lhs_type] = eq.lhs_head.split();
					switch (lhs_type) {
					case Type(Op::sum): 
						[[fallthrough]];
					case Type(Op::product): {
						std::uint32_t last_node_idx = store.insert(TypedIdxSLC{ eq.rhs_head });
						eq.rhs_head = TypedIdx(last_node_idx, lhs_type); //new eq.rhs_head is product (sum) of old eq.rhs_head divided by (minus) eq.lhs_head factors (summands).
						for (const TypedIdx elem : vc::range(Ref(store, eq.lhs_head))) {
							if (tree::contains(Ref(store, elem), to_isolate)) {
								eq.lhs_head = elem; 
							}
							else {
								const TypedIdx new_rhs_elem = (lhs_type == Op::sum ? 
									build_negated (store, elem) : 
									build_inverted(store, elem));
								last_node_idx = TypedIdxSLC::insert_new(store, last_node_idx, new_rhs_elem);
							}
						}
						//all factors (summands) have been shifted to rhs -> delete SLC (but not recursively, elems have new owner!)
						free_slc(TypedIdxSLC::SLCMutRef<TypesUnion>(store, lhs_index)); 
					} break;
					case Type(Op::named_fn): 
						assert(false); break;
					case Type(Leaf::variable): 
						assert(false); break;
					case Type(Leaf::complex): 
						assert(false); break;
					case Type(PnNode::tree_match): 
						assert(false); break;
					case Type(PnNode::value_match): 
						assert(false); break;
					case Type(PnNode::value_proxy):
						assert(false); break;
					case Type(MultiPn::summands):
						assert(false); break;
					case Type(MultiPn::factors):
						assert(false); break;
					case Type(MultiPn::params):
						assert(false); break;
					case Type(Fn::pow): {
						FnParams* params = &store.at(lhs_index).fn_params;
						if (tree::contains(Ref(store, (*params)[0u]), to_isolate)) { //case <contains var>^<computable>
							if ((*params)[1u].get_type() == Leaf::complex && MutRef(store, (*params)[1u])->complex == 2.0) { //special case <contains var>^2 -> use sqrt, not <...>^0.5
								tree::free(MutRef(store, (*params)[1u]));
								(*params)[1u] = TypedIdx();
								eq.lhs_head = (*params)[0u];
								eq.rhs_head = TypedIdx(lhs_index, Type(Fn::sqrt));
							}
							else {
								eq.lhs_head = (*params)[0u];
								(*params)[0u] = eq.rhs_head;	
								const TypedIdx inverse_expo = build_inverted(store, (*params)[1u]);
								params = &store.at(lhs_index).fn_params;
								(*params)[1u] = inverse_expo;
								eq.rhs_head = TypedIdx(lhs_index, Type(Fn::pow));
							}
						}
						else { //case <computable>^<contains var>
							eq.lhs_head = (*params)[1u];
							(*params)[0u] = eq.rhs_head;
							eq.rhs_head = TypedIdx(lhs_index, Type(Fn::log));
						}
					} break;
					case Type(Fn::sqrt): { //repurpose params from lhs as pow in "<prev. rhs> ^ 2"
						FnParams* params = &store.at(lhs_index).fn_params;
						eq.lhs_head = (*params)[0u];
						(*params)[0u] = eq.rhs_head;
						const TypedIdx square = build_value(store, 2.0);
						params = &store.at(lhs_index).fn_params;
						(*params)[1u] = square;	
						eq.rhs_head = TypedIdx(lhs_index, Type(Fn::pow));
					} break;
					default:
						assert(false);
					}
				}
				return eq;
			} //stupid_solve_for

			OptComplex eval_value_match(const Ref ref, const Complex& start_val)
			{
				const auto get_divisor = [](const Ref ref) -> std::optional<TypedIdx> {
					if (ref.type == Fn::pow) {
						const FnParams& params = *ref;
						if (params[1].get_type() == Leaf::complex) {
							if (ref.new_at(params[1])->complex == -1.0) {
								return { params[0] }; //return just base
							}
						}
					}
					return {}; //return nothing
				};

				const auto compute_exact = [](auto operate) -> OptComplex {
					std::feclearexcept(FE_ALL_EXCEPT);
					const OptComplex result = operate();
					return (!std::fetestexcept(FE_ALL_EXCEPT)) ? result : OptComplex();
				};

				switch (ref.type) {
				case Type(Op::sum): {
					OptComplex result_val = 0.0;
					for (auto& summand : vc::range(ref)) {
						if (const OptComplex summand_val = eval_value_match(ref.new_at(summand), start_val)) {
							if (const OptComplex res = compute_exact([&] {return result_val + summand_val; })) {
								result_val = res;
								continue;
							}
						}
						return {};
					}
					return result_val;
				} break;
				case Type(Op::product): {
					OptComplex result_factor = 1.0;
					OptComplex result_divisor = 1.0;
					for (auto& factor : vc::range(ref)) {
						if (const std::optional<TypedIdx> divisor = get_divisor(ref.new_at(factor))) {
							if (const OptComplex divisor_val = eval_value_match(ref.new_at(*divisor), start_val)) {
								if (const OptComplex res = compute_exact([&] { return result_divisor * divisor_val; })) {
									result_divisor = res;
									continue;
								}
							}
						}
						if (const OptComplex factor_val = eval_value_match(ref.new_at(factor), start_val)) {
							if (const OptComplex res = compute_exact([&] {return result_factor * factor_val; })) {
								result_factor = res;
								continue;
							}
						}
						return {};
					}
					return compute_exact([&] { return result_factor / result_divisor; });
				} break;
				default: {
					assert(ref.type.is<Fn>()); 
					if (const std::optional<TypedIdx> divisor = get_divisor(ref)) {
						if (const OptComplex divisor_val = eval_value_match(ref.new_at(*divisor), start_val)) {
							return compute_exact([&] { return 1.0 / *divisor_val; });
						}
						else {
							return {};
						}
					}
					const FnParams& params = *ref;
					std::array<OptComplex, 4> res_vals;
					for (std::size_t i = 0; i < fn::param_count(ref.type); i++) {
						res_vals[i] = eval_value_match(ref.new_at(params[i]), start_val);
						if (!res_vals[i]) {
							return {};
						}
					}
					return compute_exact([&] { return fn::eval(ref.type.to<Fn>(), res_vals); });
				} break;
				case Type(Leaf::complex): 
					return ref->complex;
				case Type(PnNode::value_proxy): 
					return start_val;
				}
			} //eval_value_match

		} //namespace pn_tree
		
	} //namespace pattern

	namespace tree {

		void free(const MutRef ref)
		{
			switch (ref.type) {
			case Type(Op::sum): 
				[[fallthrough]];
			case Type(Op::product): {
				for (const TypedIdx elem : vc::range(ref)) {
					tree::free(ref.new_at(elem));
				}
				free_slc(ref.cast<TypedIdxSLC>());
			} break;
			case Type(Op::named_fn): {
				for (const TypedIdx param : fn::range(ref)) {
					tree::free(ref.new_at(param));
				}
				const NamedFn& named_fn = *ref;
				free_slc(ref.new_as<TypedIdxSLC>(named_fn.params_idx));
				ref.store->free(ref.index);
			} break;
			default: {
				assert(ref.type.is<Fn>());
				for (const TypedIdx param : fn::range(ref->fn_params, ref.type)) {
					tree::free(ref.new_at(param));
				}
				ref.store->free(ref.index);
			} break;
			case Type(Leaf::variable): {
				free_slc(ref.cast<StringSLC>());
			} break;
			case Type(Leaf::complex): {
				ref.store->free(ref.index);
			} break;
			case Type(PnNode::tree_match): {
				ref.store->free(ref.index);
			} break;
			case Type(PnNode::value_match): {
				pattern::ValueMatchVariable& var = *ref;
				tree::free(ref.new_at(var.mtch_idx));
				tree::free(ref.new_at(var.copy_idx));
				ref.store->free(ref.index);
			} break;
			case Type(PnNode::value_proxy):
				break;
			case Type(MultiPn::summands):
				break;
			case Type(MultiPn::factors):
				break;
			case Type(MultiPn::params):
				break;
			}
		} //free

		TypedIdx combine(const MutRef ref, const bool exact)
		{
			const auto compute = [exact](auto operate) -> OptComplex {
				if (exact) {
					std::feclearexcept(FE_ALL_EXCEPT);
				}
				const OptComplex result = operate();
				return (!exact || std::fetestexcept(FE_ALL_EXCEPT)) ? OptComplex() : result;
			};

			switch (ref.type) {
			case Type(Op::sum): {
				OptComplex value_acc = 0.0; //stores sum of values encountered as summands
				std::uint32_t current_append_node = ref.index; //advances if summand layers are combined

				//if summand_count == 0u only combinable values where encountered -> return only value_acc (put in store), not sum
				//if summand_count == 1u sum is redundant -> return just the single summand
				std::uint32_t summand_count = 0u;

				for (TypedIdx& summand : vc::range(ref)) { //reference allowed, as no new elements are pushed in store -> never reallocates
					summand = tree::combine(ref.new_at(summand), exact);
					switch (summand.get_type()) {
					case Type(Leaf::complex):						
						if (const OptComplex res = compute([&] { return value_acc + ref.store->at(summand.get_index()).complex; })) {
							value_acc = res;
							tree::free(ref.new_at(summand));
							summand = TypedIdxSLC::null_value;
							continue;
						}
						break;
					case Type(Op::sum):
						//idea for future: push new summands to front, as they already recieved treatment
						current_append_node = TypedIdxSLC::append(*ref.store, current_append_node, summand.get_index());
						summand = TypedIdxSLC::null_value;
						break;
					}
					summand_count++; 
				}

				if (summand_count == 0u) { //catches also (hopefully impossible?) case of zero summands overall
					free_slc(ref.cast<TypedIdxSLC>()); //might as well call tree::free, but there are no remaining summands anyway.
					const auto new_summand = build_value(*ref.store, *value_acc);
					return new_summand;
				}
				else if (*value_acc != 0.0) {
					const auto new_summand = build_value(*ref.store, *value_acc);
					//note: new_summand is never counted in summand_count, but summand_count will never be read again if this line is executed anyway.
					TypedIdxSLC::insert_new(*ref.store, ref.index, new_summand); 
				}
				else if (summand_count == 1u) {
					const TypedIdx sole_summand = *begin(vc::range(ref));
					free_slc(ref.cast<TypedIdxSLC>()); //dont call tree::free, as that would also free sole_summand
					return sole_summand;
				}
			} break;
			case Type(Op::product): {
				//unlike with summands, where an inversion is just flipping the sign bit, not every multiplicativly inverse of a floating point number 
				//  can be stored as floating point number without rounding -> we need two value accumulators. sigh.
				OptComplex factor_acc = 1.0;
				OptComplex divisor_acc = 1.0; 

				//if factor_count == 0u only combinable values where encountered
				//if factor_count == 1u product is redundant -> return only that factor and free product
				std::uint32_t factor_count = 0u;

				std::uint32_t current_append_node = ref.index; //advances if summand layers are combined

				for (TypedIdx& factor : vc::range(ref)) {
					factor = tree::combine(ref.new_at(factor), exact);
					switch (factor.get_type()) {
					case Type(Fn::pow): {
						FnParams& power = *ref.new_at(factor);
						if (power[0].get_type() == Leaf::complex && power[1].get_type() == Leaf::complex) {
							std::array<OptComplex, 4> power_params = {
								ref.store->at(power[0].get_index()).complex,
								ref.store->at(power[1].get_index()).complex,
							};
							if (power_params[1]->imag() == 0.0 && power_params[1]->real() < 0.0) {
								power_params[1] *= -1.0;
								if (const OptComplex res = compute([&] { return divisor_acc * fn::eval(Fn::pow, power_params); })) {
									divisor_acc = res;
									tree::free(ref.new_at(factor)); //free whole power
									factor = TypedIdxSLC::null_value;
									continue;
								}
							}
						}
					} break;
					case Type(Leaf::complex):
						if (const OptComplex res = compute([&] { return factor_acc * ref.store->at(factor.get_index()).complex; })) {
							factor_acc = res;
							tree::free(ref.new_at(factor));
							factor = TypedIdxSLC::null_value;
							continue;
						}
						break;
					case Type(Op::product):
						//idea for future: push new factors to front, as they already recieved treatment
						current_append_node = TypedIdxSLC::append(*ref.store, current_append_node, factor.get_index());
						factor = TypedIdxSLC::null_value;
						break;
					}
					factor_count++;
				}
				{
					//reinserting the computed value(s)
					const OptComplex result_val = compute([&] { return factor_acc / divisor_acc; });
					if (result_val && *result_val != 1.0) {
						const auto new_val = build_value(*ref.store, *result_val);
						TypedIdxSLC::insert_new(*ref.store, ref.index, new_val);
						factor_count++; //single factor case is tested below -> no need to check (and perhaps return without reinserting) here
					}
					else {
						if (*factor_acc != 1.0) {
							const auto new_factor = build_value(*ref.store, *factor_acc);
							TypedIdxSLC::insert_new(*ref.store, ref.index, new_factor);
							factor_count++;
						}
						if (*divisor_acc != 1.0) {
							const auto new_divisor = build_value(*ref.store, *divisor_acc);
							const auto new_pow = build_inverted<Store>(*ref.store, new_divisor);
							TypedIdxSLC::insert_new(*ref.store, ref.index, new_pow);
							factor_count++; //single factor case is tested below -> no need to check (and perhaps return without reinserting) here
						}
					}
				}
				if (factor_count == 1u) {
					const TypedIdx sole_factor = *begin(vc::range(ref));
					free_slc(ref.cast<TypedIdxSLC>()); //dont call tree::free, as that would also free sole_factor
					return sole_factor;
				}
				else if (factor_count == 0u) { 
					//only neccesairy if empty products are possible (hopefully not?), as in that case the product makes space for the value.
					free_slc(ref.cast<TypedIdxSLC>()); //might as well call tree::free, but there are no factors anyway.
					return build_value(*ref.store, 1.0);
					//note: it is not sufficient to make ref a complex number with value 1.0, as even an empty product may hold more than one element in store.
				}
			} break;
			case Type(Op::named_fn): {
				NamedFn& function = *ref;
				for (TypedIdx& elem : fn::range(ref)) {
					elem = tree::combine(ref.new_at(elem), exact);
				}
			} break;
			case Type(Fn::force): {
				TypedIdx& param = ref->fn_params[0];
				param = tree::combine(ref.new_at(param), false);
				if (param.get_type() == Leaf::complex) {
					const TypedIdx result_val = param;
					ref.store->free(ref.index);
					return result_val;
				}
			} break;
			default: {
				assert(ref.type.is<Fn>()); 
				FnParams& params = *ref;
				std::array<OptComplex, 4> res_vals = { OptComplex{}, {}, {}, {} }; //default initialized to NaN
				for (std::size_t i = 0; i < fn::param_count(ref.type); i++) {
					params[i] = tree::combine(ref.new_at(params[i]), exact);
					if (params[i].get_type() == Leaf::complex) {
						res_vals[i] = ref.store->at(params[i].get_index()).complex;
					}
				}
				if (const OptComplex res = compute([&] { return fn::eval(ref.type.to<Fn>(), res_vals); })) {
					tree::free(ref);
					return build_value(*ref.store, *res);
				}
			} break;
			case Type(Leaf::variable): 
				break;
			case Type(Leaf::complex): 
				break;
			case Type(PnNode::tree_match): 
				break;
			case Type(PnNode::value_match): {
				pattern::ValueMatchVariable& var = *ref;
				var.mtch_idx = tree::combine(ref.new_at(var.mtch_idx), exact);
				var.copy_idx = tree::combine(ref.new_at(var.copy_idx), exact);
				assert(var.mtch_idx.get_type() != Leaf::complex); //subtree always contains value_proxy, thus should never be evaluatable    
				assert(var.copy_idx.get_type() != Leaf::complex);  //subtree always contains value_proxy, thus should never be evaluatable    
			} break;
			case Type(PnNode::value_proxy): 
				break;
			case Type(MultiPn::summands):
				break;
			case Type(MultiPn::factors):
				break;
			case Type(MultiPn::params):
				break;
			}
			return ref.typed_idx();
		} //combine

		[[nodiscard]] std::strong_ordering compare(const Ref ref_1, const Ref ref_2)
		{
			if (ref_1.type != ref_2.type) [[likely]] {
				static_assert((rematchability(Type(Op::sum)) <=> rematchability(Type(Op::sum))) == std::strong_ordering::equal); //dont wanna mix with std::strong_ordering::equivalent
				return rematchability(ref_1.type) <=> rematchability(ref_2.type);
			}

			switch (ref_1.type) {
			case Type(Op::sum):
				[[fallthrough]];
			case Type(Op::product): {
				auto range_1 = vc::range(ref_1);
				auto range_2 = vc::range(ref_2);
				auto iter_1 = begin(range_1);
				auto iter_2 = begin(range_2);
				const auto end_1 = end(range_1);
				const auto end_2 = end(range_2);
				for (; iter_1 != end_1 && iter_2 != end_2; ++iter_1, ++iter_2) {
					const auto iter_compare = tree::compare(ref_1.new_at(*iter_1), ref_2.new_at(*iter_2));
					if (iter_compare != std::strong_ordering::equal) [[likely]] {
						return iter_compare;
					}
				}
				if (iter_1 == end_1 && iter_2 == end_2) {
					return std::strong_ordering::equal;
				}
				else {
					return iter_1 == end_1 ?
						std::strong_ordering::less :
						std::strong_ordering::greater;
				}
			} break;
			case Type(Op::named_fn): {
				const auto name_cmp = compare_arrays(ref_1->named_fn.name, ref_2->named_fn.name, NamedFn::max_name_size);
				if (name_cmp != std::strong_ordering::equal) {
					return name_cmp;
				}
				auto range_1 = fn::range(ref_1);
				auto range_2 = fn::range(ref_2);
				auto iter_1 = begin(range_1);
				auto iter_2 = begin(range_2);
				const auto end_1 = end(range_1);
				const auto end_2 = end(range_2);
				for (; iter_1 != end_1 && iter_2 != end_2; ++iter_1, ++iter_2) {
					const auto iter_compare = tree::compare(ref_1.new_at(*iter_1), ref_2.new_at(*iter_2));
					if (iter_compare != std::strong_ordering::equal) [[likely]] {
						return iter_compare;
					}
				}
				if (iter_1 == end_1 && iter_2 == end_2) {
					return std::strong_ordering::equal;
				}
				else {
					return iter_1 == end_1 ?
						std::strong_ordering::less :
						std::strong_ordering::greater;
				}
			} break;
			default: {
				assert(ref_1.type.is<Fn>() && ref_2.type.is<Fn>());
				auto range_1 = fn::range(ref_1->fn_params, ref_1.type);
				auto range_2 = fn::range(ref_2->fn_params, ref_2.type);
				auto iter_1 = range_1.begin();
				auto iter_2 = range_2.begin();
				for (; iter_1 != range_1.end(); ++iter_1, ++iter_2) { //iter_1 and iter_2 both go over same number of params -> only test iter_1 for end
					const auto iter_compare = tree::compare(ref_1.new_at(*iter_1), ref_2.new_at(*iter_2));
					if (iter_compare != std::strong_ordering::equal) [[likely]] {
						return iter_compare;
					}
				}
				return std::strong_ordering::equal;
			} break;
			case Type(Leaf::variable): {
				return str_slc::compare(ref_1.cast<StringSLC>(), ref_2.cast<StringSLC>());
			} break;
			case Type(Leaf::complex): {
				const Complex& complex_1 = *ref_1;
				const Complex& complex_2 = *ref_2;
				return compare_complex(complex_2, complex_1);
			} break;
			case Type(PnNode::tree_match): {
				const pattern::TreeMatchVariable& var_1 = *ref_1;
				const pattern::TreeMatchVariable& var_2 = *ref_2;
				return var_1.match_data_idx <=> var_2.match_data_idx;
			} break;
			case Type(PnNode::value_match): {
				const pattern::ValueMatchVariable& var_1 = *ref_1;
				const pattern::ValueMatchVariable& var_2 = *ref_2;
				if (var_1.form != var_2.form) {
					return var_1.form <=> var_2.form;
				}
				if (var_1.match_data_idx != var_2.match_data_idx) {
					return var_1.match_data_idx <=> var_2.match_data_idx;
				}
				if (const auto cmp = tree::compare(ref_1.new_at(var_1.mtch_idx), ref_2.new_at(var_2.mtch_idx)); cmp != std::strong_ordering::equal) {
					return cmp;
				}
				return tree::compare(ref_1.new_at(var_1.copy_idx), ref_2.new_at(var_2.copy_idx));
			} break;
			case Type(PnNode::value_proxy): 
				[[fallthrough]];
			case Type(MultiPn::summands): 
				[[fallthrough]];
			case Type(MultiPn::factors): 
				[[fallthrough]];
			case Type(MultiPn::params): {
				return ref_1.index <=> ref_2.index;
			} break;
			}
			assert(false); 
			return std::strong_ordering::equal;
		} //compare

		void sort(const MutRef ref)
		{
			const auto sort_variadic = [&](MutRef ref) {
				const auto compare_function = [&](const TypedIdx lhs, const TypedIdx rhs) {
					return tree::compare(Ref(*ref.store, lhs), Ref(*ref.store, rhs)) == std::strong_ordering::less;
				};

				if (ref.type == Op::sum || ref.type == Op::product) {
					sort_slc(ref.cast<TypedIdxSLC>(), compare_function);						
				}
				return fold::Void{};
			};

			fold::simple_fold<fold::Void>(ref, sort_variadic);
		} //sort

		std::size_t count(const Ref ref)
		{
			struct OpAccumulator
			{
				std::size_t acc;

				constexpr OpAccumulator(const Ref) noexcept :acc(1u) {}
				void consume(const std::size_t child_size) noexcept { this->acc += child_size; }
				auto result() noexcept { return this->acc; }
			};

			return fold::tree_fold<std::size_t, OpAccumulator>(ref, [](auto) { return 1u; });
		} //count

		[[nodiscard]] TypedIdx copy(const Ref src_ref, Store& dst_store)
		{
			switch (src_ref.type) {
			case Type(Op::sum): 
				[[fallthrough]];
			case Type(Op::product): {
				const std::uint32_t dst_index = dst_store.insert(TypedIdxSLC());
				std::uint32_t last_node_idx = dst_index;
				for (const auto src_elem : vc::range(src_ref)) {
					const TypedIdx dst_elem = tree::copy(src_ref.new_at(src_elem), dst_store);
					last_node_idx = TypedIdxSLC::insert_new(dst_store, last_node_idx, dst_elem);
				}
				return TypedIdx(dst_index, src_ref.type);
			} break;
			case Type(Op::named_fn): {
				const NamedFn src_function = *src_ref; //no reference, as src and dst could be same store -> may reallocate
				NamedFn dst_function;
				std::uint32_t last_node_idx = dst_store.insert(TypedIdxSLC());
				dst_function.params_idx = last_node_idx;
				for (const auto src_param : fn::range(src_ref)) {
					const TypedIdx dst_param = tree::copy(src_ref.new_at(src_param), dst_store);
					last_node_idx = TypedIdxSLC::insert_new(dst_store, last_node_idx, dst_param);
				}

				std::copy(src_function.name, src_function.name + NamedFn::max_name_size, dst_function.name);
				return TypedIdx(dst_store.insert(dst_function), src_ref.type);
			} break;
			default: {
				assert(src_ref.type.is<Fn>());
				const FnParams src_params = *src_ref; //no reference, as src and dst could be same store -> may reallocate
				const std::size_t dst_index = dst_store.allocate(); //allocate early to have better placement order in store
				auto dst_params = FnParams();
				for (std::size_t i = 0u; i < fn::param_count(src_ref.type); i++) {
					const TypedIdx param_i = tree::copy(src_ref.new_at(src_params[i]), dst_store);
					dst_params[i] = param_i;
				}
				dst_store.at(dst_index) = dst_params;
				return TypedIdx(dst_index, src_ref.type);
			} break;
			case Type(Leaf::variable): {
				std::string src_name; //in most cases the small string optimisation works, else dont care
				str_slc::read(src_ref.cast<StringSLC>(), src_name);
				const std::size_t dst_index = str_slc::insert(dst_store, src_name);
				return TypedIdx(dst_index, src_ref.type);
			} break;
			case Type(Leaf::complex): {
				const std::size_t dst_index = dst_store.insert(*src_ref);
				return TypedIdx(dst_index, src_ref.type);
			} break;
			case Type(PnNode::tree_match): {
				const std::size_t dst_index = dst_store.insert(*src_ref); //shallow copy of var
				return TypedIdx(dst_index, src_ref.type);
			} break;
			case Type(PnNode::value_match): {
				const pattern::ValueMatchVariable src_var = *src_ref;
				const std::size_t dst_index = dst_store.allocate(); //allocate early to have better placement order in store
				auto dst_var = pattern::ValueMatchVariable(src_var.match_data_idx, src_var.form);
				dst_var.mtch_idx = tree::copy(src_ref.new_at(src_var.mtch_idx), dst_store);
				dst_var.copy_idx = tree::copy(src_ref.new_at(src_var.copy_idx), dst_store);
				dst_store.at(dst_index) = dst_var;
				return TypedIdx(dst_index, src_ref.type);
			} break;
			case Type(PnNode::value_proxy): //return same ref, as proxy does not own any nodes in src_store anyway (index has different meaning)
				[[fallthrough]];
			case Type(MultiPn::summands):
				[[fallthrough]];
			case Type(MultiPn::factors):	//return same ref, as multi_match does not own any nodes in src_store anyway (index has different meaning)
				[[fallthrough]];
			case Type(MultiPn::params):
				return src_ref.typed_idx();
			}
			assert(false); 
			return TypedIdx();
		} //copy


		bool contains(const Ref ref, const TypedIdx to_contain)
		{
			return fold::simple_fold<fold::FindBool>(ref, 
				[to_contain](const Ref ref) -> fold::FindBool 
				{ return TypedIdx(ref.index, ref.type) == to_contain; }
			);
		} //contains

		TypedIdx establish_basic_order(MutRef ref)
		{
			const TypedIdx combine_result = tree::combine(ref, true);
			tree::sort(ref.new_at(combine_result));
			return combine_result;
		} //establish_basic_order

		TypedIdx* find_subtree_owner(Store& store, TypedIdx& head, const TypedIdx subtree)
		{
			if (head == subtree) {
				return &head;
			}
			else {
				const auto [index, type] = head.split();
				switch (type) {
				case Type(Op::sum): 
					[[fallthrough]];
				case Type(Op::product): {
					for (TypedIdx& elem : vc::range(MutRef(store, head))) {
						if (TypedIdx* const elem_res = tree::find_subtree_owner(store, elem, subtree)) {
							return elem_res;
						}
					}
				} break;
				case Type(Op::named_fn): {
					for (TypedIdx& param : fn::range(MutRef(store, head))) {
						if (TypedIdx* const param_res = tree::find_subtree_owner(store, param, subtree)) {
							return param_res;
						}
					}
				} break;
				default: {
					assert(type.is<Fn>()); //if this assert hits, the switch above needs more cases.
					FnParams& params = store.at(index).fn_params;
					for (TypedIdx& param : fn::range(params, type)) {
						if (TypedIdx* const param_res = tree::find_subtree_owner(store, param, subtree)) {
							return param_res;
						}
					}
				} break;
				case Type(Leaf::variable): 
					break;
				case Type(Leaf::complex): 
					break;
				case Type(PnNode::tree_match): 
					break;
				case Type(PnNode::value_match): {
					pattern::ValueMatchVariable& var = store.at(index).value_match;
					if (TypedIdx* const copy_res = tree::find_subtree_owner(store, var.copy_idx, subtree)) {
						return copy_res;
					}
					if (TypedIdx* const match_res = tree::find_subtree_owner(store, var.mtch_idx, subtree)) {
						return match_res;
					}
				} break;
				case Type(PnNode::value_proxy):
					break;
				case Type(MultiPn::summands):
					break;
				case Type(MultiPn::factors):
					break;
				case Type(MultiPn::params):
					break;
				}
				return nullptr;
			}

		} //find_subtree_owner

		bool contains_variables(const Ref ref)
		{
			using namespace pattern;
			const auto test_for_variables = [](Ref ref) -> fold::FindBool {
				return ref.type == Leaf::variable || ref.type.is<MatchType>();
			};
			return fold::simple_fold<fold::FindBool>(ref, test_for_variables);
		} //contains_variables

		TypedIdx search_variable(const Ref ref, const std::string_view name)
		{
			const auto test_for_name = [name](Ref ref) -> fold::Find<TypedIdx> {
				return (ref.type == Leaf::variable && str_slc::compare(ref.cast<StringSLC>(), name) == std::strong_ordering::equal) ?
					fold::done(TypedIdx(ref.index, ref.type)) : //name was found -> cut tree evaluation here
					fold::more(TypedIdx());
			};
			return *fold::simple_fold<fold::Find<TypedIdx>>(ref, test_for_name);
		} //search_variable	

	} //namespace tree

	namespace match {

		bool equals(const Ref pn_ref, const Ref ref, pattern::MatchData& match_data)
		{
			using namespace pattern;

			if (!pn_ref.type.is<MatchType>()) {
				if (pn_ref.type != ref.type) [[likely]] {
					return false;
				}
				else {
					switch (pn_ref.type) {
					case Type(Op::sum): 
						[[fallthrough]];
					case Type(Op::product): {
						const auto [matched, not_matched] = match::permutation_equals(pn_ref, ref, match_data);
						return matched.size() > 0u && not_matched.size() == 0u;
					} break;
					case Type(Op::named_fn): {
						if (compare_arrays(ref->named_fn.name, pn_ref->named_fn.name, NamedFn::max_name_size) != std::strong_ordering::equal) {
							return false;
						}
						auto pn_range = fn::range(pn_ref);
						auto pn_iter = begin(pn_range);
						const auto pn_stop = end(pn_range);
						auto range = fn::range(ref);
						auto iter = begin(range);
						const auto stop = end(range);
						for (; pn_iter != pn_stop && iter != stop; ++pn_iter, ++iter) {
							const auto pn_iter_ref = pn_ref.new_at(*pn_iter);
							const auto iter_ref = ref.new_at(*iter);
							if (pn_iter_ref.type == MultiPn::params) {
								SharedMultiDatum& info = match_data.multi_info(pn_iter_ref.index);
								assert(info.match_indices.size() == 0u);
								while (iter != stop) {
									info.match_indices.push_back(*iter);
									++iter;
								}
								return true;
							}
							if (!match::equals(pn_iter_ref, iter_ref, match_data)) {
								return false;
							}
						}

						if (iter == stop) {
							return pn_iter == pn_stop || pn_iter->get_type() == MultiPn::params;
						}
						return false;
					} break;
					default: {
						assert(pn_ref.type.is<Fn>());
						auto pn_range = fn::range(pn_ref->fn_params, pn_ref.type);
						auto pn_iter = pn_range.begin();
						const auto pn_stop = end(pn_range);
						auto range = fn::range(ref->fn_params, ref.type);
						auto iter = range.begin();
						for (; pn_iter != pn_stop; ++pn_iter, ++iter) { //iter and pn_iter both go over same number of params
							if (!match::equals(pn_ref.new_at(*pn_iter), ref.new_at(*iter), match_data)) {
								return false;
							}
						}
						return true;
					} break;
					case Type(Leaf::variable):
						return str_slc::compare(pn_ref.cast<StringSLC>(), ref.cast<StringSLC>()) == std::strong_ordering::equal;
					case Type(Leaf::complex): {
						const Complex& complex = *ref;
						const Complex& pn_complex = *pn_ref;
						return compare_complex(complex, pn_complex) == std::strong_ordering::equal;
					} break;
					}
				}
			}
			else {

				switch (pn_ref.type) {
				case Type(PnNode::tree_match): {
					const TreeMatchVariable& var = *pn_ref;
					if (!meets_restriction(ref, var.restr)) {
						return false;
					}
					auto& match_info = match_data.info(var);
					if (match_info.is_set()) {
						return tree::compare(ref, ref.new_at(match_info.match_idx)) == std::strong_ordering::equal;
					}
					else {
						match_info.match_idx = ref.typed_idx();
						match_info.responsible = pn_ref.typed_idx();
						return true;
					}
				} break;
				case Type(PnNode::value_match): {
					if (ref.type != Leaf::complex) { //only this test allows us to pass *ref to evaluate this_value
						return false;
					}
					const ValueMatchVariable& var = *pn_ref;
					auto& match_info = match_data.info(var);
					const OptComplex this_value = pn_tree::eval_value_match(pn_ref.new_at(var.mtch_idx), *ref); 
					if (!this_value || !has_form(*this_value, var.form)) {
						return false;
					}
					else if (match_info.is_set()) {
						return this_value.val == match_info.value;
					}
					else {
						match_info.value = *this_value;
						match_info.mtch_idx = ref.typed_idx();
						match_info.responsible = pn_ref.typed_idx();
						return true;
					}
				} break;
				case Type(PnNode::value_proxy): //may only be encountered in pn_tree::eval_value_match (as value_match does no equals call)
					assert(false);
					return false;
				case Type(MultiPn::summands):
					if (ref.type == Op::sum) {
						SharedMultiDatum& info = match_data.multi_info(pn_ref.index);
						assert(info.match_indices.size() == 0u);
						for (const TypedIdx elem : vc::range(ref)) {
							info.match_indices.push_back(elem);
						}
						return true;
					}
					return false;
				case Type(MultiPn::factors):
					if (ref.type == Op::product) {
						SharedMultiDatum& info = match_data.multi_info(pn_ref.index);
						assert(info.match_indices.size() == 0u);
						for (const TypedIdx elem : vc::range(ref)) {
							info.match_indices.push_back(elem);
						}
						return true;
					}
					return false;
				case Type(MultiPn::params): //assumed to be handeled only as param in named_fn
					[[fallthrough]];
				default:
					assert(false);
					return false;
				}
			}

		} //equals

		//this function currently has complexity O(m^n) where m is count of ref's elements and n is count of pn_ref's elements.
		//in principle it could be turend to O(m*n), but i have not yet turned this into a working algorithm.
		//the tricky part is to ensure, that one will never 
		PermutationEqualsRes permutation_equals(const Ref pn_ref, const Ref ref, pattern::MatchData& match_data)
		{
			using namespace pattern;
			assert(pn_ref.type == ref.type && (is_one_of<Op::sum, Op::product>(ref.type)));

			const auto reset_own_matches = [&match_data](const Ref pn_ref) {
				const auto reset_single = [&match_data](const Ref ref) -> fold::Void {
					switch (ref.type) {
					case Type(PnNode::tree_match): {
						SharedTreeDatum& info = match_data.info(ref->tree_match);
						if (info.responsible == ref.typed_idx()) {
							info = SharedTreeDatum();
						}
					} break;
					case Type(PnNode::value_match): {
						SharedValueDatum& info = match_data.info(ref->value_match);
						if (info.responsible == ref.typed_idx()) {
							info = SharedValueDatum();
						}
					} break;
					case Type(MultiPn::summands):
						[[fallthrough]];
					case Type(MultiPn::factors): {
						SharedMultiDatum& info = match_data.multi_info(ref.index);
						info.match_indices.clear();
					} break;
					}
					return fold::Void{};
				};
				fold::simple_fold<fold::Void>(pn_ref, reset_single);
			};

			PermutationEqualsRes result;
			auto& [matched, not_matched] = result;
			for (const TypedIdx elem : vc::range(ref)) {
				not_matched.push_back(elem);
			}

			struct PnElemData
			{
				TypedIdx elem;
				std::uint32_t result_idx = -1u; //index in not_matched where current match used to reside (if current match exists, it is stored in matched)
			};
			StupidBufferVector<PnElemData, 8> pn_elements;
			for (const TypedIdx elem : vc::range(pn_ref)) {
				pn_elements.emplace_back(elem, -1u);
			}

			constexpr auto null_value = TypedIdx();

			std::uint32_t pn_i = 0u; //index in pn_elements
			std::uint32_t start_k = 0u; //all elements in not_matched bevore index start_k are ignored
			while (pn_i < pn_elements.size()) {
				const PnElemData pn_data_i = pn_elements[pn_i];
				if (ref.type == Op::sum     && pn_data_i.elem.get_type() == MultiPn::summands ||
					ref.type == Op::product && pn_data_i.elem.get_type() == MultiPn::factors ) [[unlikely]]
				{
					not_matched.shorten_to(std::remove(not_matched.begin(), not_matched.end(), null_value)); //remove null_value's from not_matched
					for (const TypedIdx elem : not_matched) {
						matched.emplace_back(elem);
					}
					SharedMultiDatum& info = match_data.multi_info(pn_data_i.elem.get_index());
					info.match_indices = std::move(not_matched);
					return result;
				}

				const Ref pn_elem_i_ref = pn_ref.new_at(pn_data_i.elem);

				for (std::uint32_t k = start_k; k < not_matched.size(); k++) {
					const TypedIdx elem_k = not_matched[k];
					if (elem_k == null_value) {
						continue; //elem_k is currently matched by some other pattern variable -> ignore it for now
					}
					else if (match::equals(pn_elem_i_ref, ref.new_at(elem_k), match_data)) {
						matched.push_back(elem_k);
						pn_elements[pn_i].result_idx = k;
						not_matched[k] = null_value;
						goto found_match; //this is considered harmful >:) hehehehe
					}
					else {
						reset_own_matches(pn_elem_i_ref);
					}
				}

				//if we get here, no match for pn_elem_i_ref was found in all of not_matched. 
				//now we need to rematch previous pn_elem's and test if this frees some spot for our current pn_elem_i_ref
				if (pn_i > 0u) {
					pn_i--;
					const std::uint32_t matched_at_idx = std::exchange(pn_elements[pn_i].result_idx, -1u);
					not_matched[matched_at_idx] = matched.pop_back();
					start_k = matched_at_idx + 1u;

					const Ref pn_elem_i_ref = pn_ref.new_at(pn_elements[pn_i].elem);
					reset_own_matches(pn_elem_i_ref); //here could a call to equals_another_way be happening
					continue;
				}
				else {
					matched.clear();
					not_matched.clear();
					return result;
				}

			found_match:
				pn_i++;
			}

			not_matched.shorten_to(std::remove(not_matched.begin(), not_matched.end(), null_value)); //remove null_value's from not_matched
			return result;
		} //permutation_equals

		TypedIdx copy(const Ref pn_ref, const pattern::MatchData& match_data, const Store& src_store, Store& dst_store)
		{
			using namespace pattern;

			switch (pn_ref.type) {
			case Type(Op::sum): 
				[[fallthrough]];
			case Type(Op::product): {
				const std::uint32_t res_idx = dst_store.insert(TypedIdxSLC());
				std::uint32_t last_node_idx = res_idx;
				for (const TypedIdx elem : vc::range(pn_ref)) {
					const TypedIdx dst_elem = match::copy(pn_ref.new_at(elem), match_data, src_store, dst_store);
					last_node_idx = TypedIdxSLC::insert_new(dst_store, last_node_idx, dst_elem);
				}
				return TypedIdx(res_idx, pn_ref.type);
			} break;
			case Type(Op::named_fn): {
				const NamedFn& src_function = *pn_ref;
				NamedFn dst_function;
				std::uint32_t last_node_idx = dst_store.insert(TypedIdxSLC());
				dst_function.params_idx = last_node_idx;
				for (const TypedIdx param : fn::range(pn_ref)) {
					const auto param_ref = pn_ref.new_at(param);
					if (param_ref.type == MultiPn::params) {
						for (const TypedIdx matched_param : match_data.multi_info(param_ref.index).match_indices) {
							const TypedIdx dst_param = tree::copy(Ref(src_store, matched_param), dst_store); //call normal copy!
							last_node_idx = TypedIdxSLC::insert_new(dst_store, last_node_idx, dst_param);
						}
					}
					else {
						const TypedIdx dst_param = match::copy(param_ref, match_data, src_store, dst_store);
						last_node_idx = TypedIdxSLC::insert_new(dst_store, last_node_idx, dst_param);
					}
				}

				std::copy_n(src_function.name, NamedFn::max_name_size, dst_function.name);
				return TypedIdx(dst_store.insert(dst_function), pn_ref.type);
			} break;
			default: {
				assert(pn_ref.type.is<Fn>());
				const FnParams& pn_params = *pn_ref;
				auto dst_params = FnParams();
				for (std::size_t i = 0u; i < fn::param_count(pn_ref.type); i++) {
					dst_params[i] = match::copy(pn_ref.new_at(pn_params[i]), match_data, src_store, dst_store);
				}
				return TypedIdx(dst_store.insert(dst_params), pn_ref.type);
			} break;
			case Type(Leaf::variable): {
				std::string src_name; //in most cases the small string optimisation works, else dont care
				str_slc::read(pn_ref.cast<StringSLC>(), src_name);
				const std::size_t dst_index = str_slc::insert(dst_store, src_name);
				return TypedIdx(dst_index, pn_ref.type);
			} break;
			case Type(Leaf::complex): 
				return TypedIdx(dst_store.insert(pn_ref->complex), pn_ref.type);
			case Type(PnNode::tree_match): {
				const SharedTreeDatum& info = match_data.info(pn_ref->tree_match);
				return tree::copy(Ref(src_store, info.match_idx), dst_store); //call to different copy!
			} break;
			case Type(PnNode::value_match): {
				const ValueMatchVariable& var = *pn_ref;
				return match::copy(pn_ref.new_at(var.copy_idx), match_data, src_store, dst_store);				
			} break;
			case Type(PnNode::value_proxy): {
				const Complex& val = match_data.value_match_data[pn_ref.index].value;
				return TypedIdx(dst_store.insert(val), Type(Leaf::complex));
			} break;
			case Type(MultiPn::summands):
				[[fallthrough]];
			case Type(MultiPn::factors): {
				const std::uint32_t res_idx = dst_store.insert(TypedIdxSLC());
				std::uint32_t last_node_idx = res_idx;
				const SharedMultiDatum& info = match_data.multi_info(pn_ref.index);
				for (const TypedIdx elem : info.match_indices) {
					const TypedIdx dst_elem = tree::copy(Ref(src_store, elem), dst_store); //call to different copy!
					last_node_idx = TypedIdxSLC::insert_new(dst_store, last_node_idx, dst_elem);
				}
				return TypedIdx(res_idx, pn_ref.type == MultiPn::summands ? Type(Op::sum) : Type(Op::product));			
			} break;
			case Type(MultiPn::params):  //already handeled in named_fn
				assert(false);
				return TypedIdx();
			}
		} //copy

		std::optional<TypedIdx> match_and_replace(const Ref from, const Ref to, const MutRef ref)
		{		

			if ((from.type == Op::sum || from.type == Op::product) && (from.type == ref.type)) {
				pattern::MatchData match_data;
				const auto [matched_elems, remaining_elems] = match::permutation_equals(from, ref, match_data);
				if (matched_elems.size() > 0u) {
					Store copy_buffer;
					copy_buffer.reserve(32u);
					free_slc(ref.cast<TypedIdxSLC>()); //shallow deletion only of initial sum / product itself, not of its operands
					const TypedIdx buffer_head = match::copy(to, match_data, *ref.store, copy_buffer);
					for (const TypedIdx elem : matched_elems) { //delete each summand / factor occuring in match
						tree::free(ref.new_at(elem));
					}
					const TypedIdx pattern_copy_head = tree::copy(Ref(copy_buffer, buffer_head), *ref.store);
					const std::uint32_t res_idx = ref.store->insert(TypedIdxSLC({ pattern_copy_head }));
					std::uint32_t last_node_idx = res_idx;
					for (const TypedIdx elem : remaining_elems) { //copy back all summands / factors not part of match
						last_node_idx = TypedIdxSLC::insert_new(*ref.store, last_node_idx, elem);
					}
					return { TypedIdx(res_idx, ref.type) };
				}
			}
			else {
				pattern::MatchData match_data;
				if (match::equals(from, ref, match_data)) {
					Store copy_buffer;
					copy_buffer.reserve(32u);
					const TypedIdx buffer_head = match::copy(to, match_data, *ref.store, copy_buffer);
					tree::free(ref);
					const TypedIdx result_head = tree::copy(Ref(copy_buffer, buffer_head), *ref.store);
					return { result_head };
				}
			}
			return {};
		} //match_and_replace

		std::pair<std::optional<TypedIdx>, bool> recursive_match_and_replace(const Ref in, const Ref out, const MutRef ref)
		{
			switch (ref.type) {
			case Type(Op::sum): 
				[[fallthrough]];
			case Type(Op::product): {
				auto range = vc::range(ref);
				auto iter = begin(range);
				const auto stop = end(range);
				for (; iter != stop; ++iter) {
					const auto [new_elem, matched_deeper] = recursive_match_and_replace(in, out, ref.new_at(*iter));
					if (new_elem) {
						*iter = *new_elem;
						return std::make_pair(std::nullopt, true);
					}
					else if (matched_deeper) {
						return std::make_pair(std::nullopt, true);
					}
				}
			} break;
			case Type(Op::named_fn): {
				auto range = fn::range(ref);
				auto iter = begin(range);
				const auto stop = end(range);
				for (; iter != stop; ++iter) {
					const auto [new_param, matched_deeper] = recursive_match_and_replace(in, out, ref.new_at(*iter));
					if (new_param) {
						*iter = *new_param;
						return std::make_pair(std::nullopt, true);
					}
					else if (matched_deeper) {
						return std::make_pair(std::nullopt, true);
					}
				}
			} break;
			default: {
				assert(ref.type.is<Fn>());
				auto range = fn::range(ref->fn_params, ref.type);
				auto iter = begin(range);
				const auto stop = end(range);
				for (; iter != stop; ++iter) {
					const auto [new_param, matched_deeper] = recursive_match_and_replace(in, out, ref.new_at(*iter));
					if (new_param) {
						*iter = *new_param;
						return std::make_pair(std::nullopt, true);
					}
					else if (matched_deeper) {
						return std::make_pair(std::nullopt, true);
					}
				}
			} break;
			case Type(Leaf::variable): 
				break;
			case Type(Leaf::complex): 
				break;
			}
			return std::make_pair(match_and_replace(in, out, ref), false);
		} //recursive_match_and_replace

	} //namespace match

	namespace fold {

		template<typename Res_T, typename Union_T, typename Type_T, Const is_const, typename Apply>
		Res_T simple_fold(const BasicRef<Union_T, Type_T, is_const> ref, Apply apply)
		{
			constexpr bool pattern = std::is_same_v<Type_T, Type>;
			constexpr bool return_early_possible = ReturnEarlyPossible<Res_T>::value;

			switch (ref.type) {
			case Type_T(Op::sum): 
				[[fallthrough]];
			case Type_T(Op::product): {
				for (const auto elem : vc::range(ref)) {
					const Res_T elem_res = fold::simple_fold<Res_T>(ref.new_at(elem), apply);
					if constexpr (return_early_possible) { if (elem_res.return_early()) { return elem_res; } }
				}
			} break;
			case Type_T(Op::named_fn): {
				for (const auto param : fn::range(ref)) {
					const Res_T param_res = fold::simple_fold<Res_T>(ref.new_at(param), apply);
					if constexpr (return_early_possible) { if (param_res.return_early()) { return param_res; } }
				}
			} break;
			default: {
				assert(ref.type.is<Fn>());
				for (const auto param : fn::range(ref->fn_params, ref.type)) {
					const Res_T param_res = fold::simple_fold<Res_T>(ref.new_at(param), apply);
					if constexpr (return_early_possible) { if (param_res.return_early()) { return param_res; } }
				}
			} break;
			case Type_T(Leaf::variable): 
				break;
			case Type_T(Leaf::complex): 
				break;
			case Type_T(PnNode::tree_match): 
				break;
			case Type_T(PnNode::value_match): if constexpr (pattern) {
				const pattern::ValueMatchVariable var = *ref;
				const Res_T elem_res_1 = fold::simple_fold<Res_T>(ref.new_at(var.mtch_idx), apply);
				if constexpr (return_early_possible) { if (elem_res_1.return_early()) { return elem_res_1; } }
				const Res_T elem_res_2 = fold::simple_fold<Res_T>(ref.new_at(var.copy_idx), apply);
				if constexpr (return_early_possible) { if (elem_res_2.return_early()) { return elem_res_2; } }
			} break;
			case Type_T(PnNode::value_proxy):
				break;
			case Type_T(MultiPn::summands):
				break;
			case Type_T(MultiPn::factors):
				break;
			case Type_T(MultiPn::params):
				break;
			}
			return apply(ref); 
		} //simple_fold

		template<typename Res_T, typename OpAccumulator, typename Union_T, typename Type_T, Const is_const, typename LeafApply, typename... AccInit>
		Res_T tree_fold(const BasicRef<Union_T, Type_T, is_const> ref, LeafApply leaf_apply, const AccInit... init)
		{
			constexpr bool pattern = std::is_same_v<Type_T, Type>;

			switch (ref.type) {
			case Type_T(Op::sum): 
				[[fallthrough]];
			case Type_T(Op::product): {
				OpAccumulator acc(ref, init...);
				for (const auto elem : vc::range(ref)) {
					acc.consume(fold::tree_fold<Res_T, OpAccumulator>(ref.new_at(elem), leaf_apply, init...));
				}
				return acc.result();
			} 
			case Type_T(Op::named_fn): {
				OpAccumulator acc(ref, init...);
				for (const auto param : fn::range(ref)) {
					acc.consume(fold::tree_fold<Res_T, OpAccumulator>(ref.new_at(param), leaf_apply, init...));
				}
				return acc.result();
			} 
			default: {
				assert(ref.type.is<Fn>());
				OpAccumulator acc(ref, init...);
				for (const auto param : fn::range(ref->fn_params, ref.type)) {
					acc.consume(fold::tree_fold<Res_T, OpAccumulator>(ref.new_at(param), leaf_apply, init...));
				}
				return acc.result();		
			}
			case Type_T(PnNode::value_match): if constexpr (pattern) {
				OpAccumulator acc(ref, init...);
				const pattern::ValueMatchVariable var = *ref;
				acc.consume(fold::tree_fold<Res_T, OpAccumulator>(ref.new_at(var.mtch_idx), leaf_apply, init...));
				acc.consume(fold::tree_fold<Res_T, OpAccumulator>(ref.new_at(var.copy_idx), leaf_apply, init...));
				return acc.result();
			} [[fallthrough]];
			case Type_T(Leaf::variable): 
				[[fallthrough]];
			case Type_T(Leaf::complex):
				[[fallthrough]];
			case Type_T(PnNode::tree_match): 
				[[fallthrough]];
			case Type_T(PnNode::value_proxy):
				[[fallthrough]];
			case Type_T(MultiPn::summands):
				[[fallthrough]];
			case Type_T(MultiPn::factors):
				[[fallthrough]];
			case Type_T(MultiPn::params):
				return leaf_apply(ref);
			}
		} //tree_fold

	} //namespace fold

} //namespace bmath::intern


namespace bmath {
	using namespace intern;

	Term::Term(std::string& name)
	{
		this->store.reserve(name.size() / 2);
		auto parse_string = ParseString(name);
		parse_string.allow_implicit_product();
		parse_string.remove_space();
		{
			const std::size_t error_pos = find_first_not_arithmetic(parse_string.tokens);
			if (error_pos != TokenView::npos) [[unlikely]] throw ParseFailure{ error_pos, "illegal character" };
		}
		this->head = build(this->store, parse_string);
	} //Term

	Term::Term(const std::string_view simple_name)
	{
		this->store.reserve(simple_name.size() / 2);
		const auto tokens = TokenString(simple_name);
		{
			const std::size_t char_pos = find_first_not_arithmetic(tokens);
			if (char_pos != TokenView::npos) [[unlikely]] throw ParseFailure{ char_pos, "illegal character" };
			const std::size_t space_pos = simple_name.find_first_of(' ');
			if (space_pos != TokenView::npos) [[unlikely]] throw ParseFailure{ space_pos, "space forbidden in simple constructor" };
		}
		this->head = build(this->store, ParseView(tokens, simple_name.data(), 0u));
	} //Term

	void Term::establish_order() noexcept
	{
		this->head = tree::establish_basic_order(this->mut_ref());
	}

	std::string bmath::Term::to_memory_layout() const noexcept
	{
		return print::to_memory_layout(this->store, { this->head });
	}

	std::string Term::to_string() const noexcept
	{
		std::string result;
		result.reserve(this->store.size() * 2);
		print::append_to_string(this->ref(), result);
		return result;
	}

	std::string Term::to_pretty_string() noexcept
	{
		this->establish_order();
		return print::to_pretty_string(this->ref());
	}

	std::string Term::to_pretty_string() const noexcept
	{
		return print::to_pretty_string(this->ref());
	}

	std::string Term::to_tree() const noexcept
	{
		return print::to_tree(this->ref());
	}

	MutRef Term::mut_ref() noexcept
	{
		return MutRef(this->store, this->head);
	}

	Ref Term::ref() const noexcept
	{
		return Ref(this->store, this->head);
	}

	bool Term::match_and_replace(const intern::pattern::PnTerm& p) noexcept
	{
		const auto [head_match, deeper_match] = match::recursive_match_and_replace(p.lhs_ref(), p.rhs_ref(), this->mut_ref());
		if (head_match) {
			this->head = *head_match;
		}
		return head_match || deeper_match;
	}

} //namespace bmath