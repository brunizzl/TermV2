
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
		using TypedIdx_T = BasicTypedIdx<Type_T>;
		using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
		constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

		switch (ref.type) {
		case Type_T(Op::sum): 
			[[fallthrough]];
		case Type_T(Op::product): {
			for (const TypedIdx_T elem : vc::range(ref)) {
			}
			assert(false);
		} break;
		case Type_T(Op::generic_function): {
			for (const TypedIdx_T param : fn::range(ref)) {
			}
			assert(false);
		} break;
		default: {
			assert(ref.type.is<Fn>());
			for (const TypedIdx_T param : fn::range(ref->fn_params, ref.type)) {
			}
			assert(false);
		} break;
		case Type_T(Leaf::variable): {
			assert(false);
		} break;
		case Type_T(Leaf::complex): {
			assert(false);
		} break;
		case Type_T(pattern::_tree_match): if constexpr (pattern) {
			assert(false);
		} break;
		case Type_T(pattern::_value_match): if constexpr (pattern) {
			pattern::ValueMatchVariable& var = *ref;
			assert(false);
		} break;
		case Type_T(pattern::_value_proxy):
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

	//more unique (meaning harder to match) is smaller
	constexpr auto uniqueness_table = std::to_array<std::pair<pattern::PnType, int>>({
		{ Type(Fn::asinh           )  ,  0 }, //order of parameters is given -> most unique
		{ Type(Fn::acosh           )  ,  2 }, //order of parameters is given -> most unique
		{ Type(Fn::atanh           )  ,  4 }, //order of parameters is given -> most unique
		{ Type(Fn::asin            )  ,  6 }, //order of parameters is given -> most unique
		{ Type(Fn::acos            )  ,  8 }, //order of parameters is given -> most unique
		{ Type(Fn::atan            )  , 10 }, //order of parameters is given -> most unique
		{ Type(Fn::sinh            )  , 12 }, //order of parameters is given -> most unique
		{ Type(Fn::cosh            )  , 14 }, //order of parameters is given -> most unique
		{ Type(Fn::tanh            )  , 16 }, //order of parameters is given -> most unique
		{ Type(Fn::sqrt            )  , 18 }, //order of parameters is given -> most unique
		{ Type(Fn::pow             )  , 20 }, //order of parameters is given -> most unique
		{ Type(Fn::log             )  , 22 }, //order of parameters is given -> most unique
		{ Type(Fn::exp             )  , 24 }, //order of parameters is given -> most unique
		{ Type(Fn::sin             )  , 26 }, //order of parameters is given -> most unique
		{ Type(Fn::cos             )  , 28 }, //order of parameters is given -> most unique
		{ Type(Fn::tan             )  , 30 }, //order of parameters is given -> most unique
		{ Type(Fn::abs             )  , 32 }, //order of parameters is given -> most unique
		{ Type(Fn::arg             )  , 34 }, //order of parameters is given -> most unique
		{ Type(Fn::ln              )  , 36 }, //order of parameters is given -> most unique
		{ Type(Fn::re              )  , 38 }, //order of parameters is given -> most unique
		{ Type(Fn::im              )  , 40 }, //order of parameters is given -> most unique
		{ Type(Op::generic_function)  , 50 }, //order of parameters is given -> most unique
		{ Type(Op::product         )  , 55 }, //order of operands my vary -> second most unique
		{ Type(Op::sum             )  , 60 }, //order of operands my vary -> second most unique
		{ pattern::PnVariable::value_match, 65 }, //a bit more unique than complex
		{ pattern::PnVariable::value_proxy, 70 }, //a bit more unique than complex
		{ Type(Leaf::variable        )  , 75 }, //quite not unique
		{ Type(Leaf::complex         )  , 80 }, //quite not unique
		{ pattern::PnVariable::tree_match , 85 }, //can match anything (in princible) -> least unique
	});
	constexpr int uniqueness(pattern::PnType type) noexcept { return find_snd(uniqueness_table, type); }

	//utility for both Function and GenericFunction
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
					case Fn::pow  : return std::pow(real_0, real_1);
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
					case Fn::pow  : return std::pow(*param_vals[0], real_1);
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
		
		template<typename TypedIdx_T>
		bool meets_restriction(const TypedIdx_T head, const Restriction restr)
		{
			if (restr == Restr::any) {
				return true;
			}
			else if (restr.is<Type>()) {
				const Type type = head.get_type();
				return restr == type;
			}
			else if (restr == Restr::function) {
				const Type type = head.get_type();
				return type == Type::generic_function || type.is<Fn>();
			}
			else {
				assert(false);
				return false;
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
			case Form::integer:   accept &= re - std::uint64_t(re) == 0.0; 
			                      accept &= (std::abs(re) <= max_save_int); [[fallthrough]];
			case Form::real:      accept &= im == 0.0;
				return accept;
			case Form::negative:      return re <   0.0 && im == 0.0;
			case Form::positive:      return re >   0.0 && im == 0.0;
			case Form::not_negative:  return re >=  0.0 && im == 0.0;
			case Form::not_positive:  return re <=  0.0 && im == 0.0;
			case Form::not_minus_one: return re != -1.0 || im != 0.0;
			default:
				assert(false);
				return false;
			}
		} //has_form

		PnTerm::PnTerm(std::string& name) 
		{
			auto parse_string = ParseString(name);
			parse_string.allow_implicit_product();
			parse_string.remove_space();
			const auto parts = split(parse_string);
			NameLookupTable table = parse_declarations(parts.declarations);
			throw_if(table.tree_table.size() > MatchData::max_tree_match_count, "too many tree match variables declared");
			throw_if(table.value_table.size() > MatchData::max_value_match_count, "too many value match variables declared");
			PatternBuildFunction build_function = { table };
			this->lhs_head = build_function(this->lhs_store, parts.lhs);
			table.build_lhs = false;
			this->rhs_head = build_function(this->rhs_store, parts.rhs);

			this->lhs_head = tree::establish_basic_order(this->lhs_ref());
			this->rhs_head = tree::establish_basic_order(this->rhs_ref());

			for (const auto& value_match : table.value_table) {
				for (const auto lhs_instance : value_match.lhs_instances) {
					pn_tree::rearrange_value_match(this->lhs_store, this->lhs_head, lhs_instance);
				}
				for (const auto rhs_instance : value_match.rhs_instances) {
					pn_tree::rearrange_value_match(this->rhs_store, this->rhs_head, rhs_instance);
				}
			}
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

		namespace pn_tree {

			PnTypedIdx* find_value_match_subtree(PnStore& store, PnTypedIdx& head, const PnTypedIdx value_match)
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
				const auto classify_subterm = [&store, value_match](const PnTypedIdx head) -> MatchTraits {
					struct OpAccumulator
					{
						MatchTraits acc;

						constexpr OpAccumulator(PnRef ref, const PnTypedIdx value_match, const PnTypedIdx value_proxy) 
							:acc({ .has_match = false, .computable = true })
						{
							switch (ref.type) {
							case PnType(Op::sum):     break;
							case PnType(Op::product): break;
							case PnType(Fn::pow):     break;// for now only allow these Fn to be computed in value_match
							case PnType(Fn::sqrt):    break;// for now only allow these Fn to be computed in value_match  
							default:
								assert(ref.type.is<Fn>()); 
								[[fallthrough]];
							case PnType(Op::generic_function):
								this->acc = MatchTraits{ .has_match = false, .computable = false };
								break;
							case PnType(PnVariable::value_match): {
								const bool is_right_match = PnTypedIdx(ref.index, ref.type) == value_match;
								this->acc = MatchTraits{ .has_match = is_right_match, .computable = is_right_match };
								break;
							}
							}
						} //ctor

						constexpr void consume(const MatchTraits elem_res) { this->acc.combine(elem_res); }
						constexpr MatchTraits result() const noexcept { return this->acc; }
					}; //struct OpAccumulator

					const auto leaf_apply = [](PnRef ref) -> MatchTraits {
						return MatchTraits{ false, is_one_of<Leaf::complex, PnVariable::value_proxy>(ref.type) };
					};

					const ValueMatchVariable& var = store.at(value_match.get_index()).value_match;
					assert(var.copy_idx == var.match_idx);

					return fold::tree_fold<MatchTraits, OpAccumulator>(PnRef(store, head), leaf_apply, value_match, var.copy_idx);
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
				case PnType(Op::sum): 
					[[fallthrough]];
				case PnType(Op::product): {
					for (PnTypedIdx& elem : vc::range(PnMutRef(store, head))) {
						if (PnTypedIdx* const elem_res = find_value_match_subtree(store, elem, value_match)) {
							return elem_res;
						}
					}
				} break;
				case PnType(Op::generic_function): {
					for (PnTypedIdx& param : fn::range(PnMutRef(store, head))) {
						if (PnTypedIdx* const elem_res = find_value_match_subtree(store, param, value_match)) {
							return elem_res;
						}
					}
				} break;
				default: {
					assert(type.is<Fn>()); //if this assert hits, the switch above needs more cases.
					FnParams<PnTypedIdx>& params = store.at(index).fn_params;
					for (PnTypedIdx& param : fn::range(params, type)) {
						if (PnTypedIdx* const elem_res = find_value_match_subtree(store, param, value_match)) {
							return elem_res;
						}
					}
				} break;
				case PnType(pattern::_value_match): 
					break;
				case PnType(Leaf::variable): 
					break;
				case PnType(Leaf::complex): 
					break;
				case PnType(pattern::_tree_match): 
					break;
				case PnType(pattern::_value_proxy):
					break;
				}
				return nullptr;
			} //find_value_match_subtree

			void rearrange_value_match(PnStore& store, PnTypedIdx& head, const PnTypedIdx value_match)
			{
				PnTypedIdx* const value_match_subtree = find_value_match_subtree(store, head, value_match);
				PnTypedIdx* const value_match_storage = tree::find_subtree_owner(store, &head, value_match);
				ValueMatchVariable* var = &store.at(value_match.get_index()).value_match;
				const PnTypedIdx proxy_value = var->copy_idx;
				assert(var->copy_idx == var->match_idx);

				if (value_match_storage != value_match_subtree) { //else value_match owns just itself, no nodes upstream
					var->copy_idx = *value_match_subtree; //keep the nodes now owned by value_match in current arrangement as tree to copy
					*value_match_storage = proxy_value;
					*value_match_subtree = value_match;

					PnTypedIdx match_data = tree::copy(PnRef(store, var->copy_idx), store); //invalidates var
					const auto [new_match_data, new_match_idx] = stupid_solve_for(store, { match_data, proxy_value }, proxy_value);
					assert(new_match_data == proxy_value);
					var = &store.at(value_match.get_index()).value_match;
					var->match_idx = new_match_idx;

					var->match_idx = tree::establish_basic_order(PnMutRef(store, var->match_idx));
				}

			} //rearrange_value_match

			Equation stupid_solve_for(PnStore& store, Equation eq, const PnTypedIdx to_isolate)
			{
				assert(tree::contains(PnRef(store, eq.lhs_head), to_isolate));

				while (eq.lhs_head != to_isolate) {

					const auto [lhs_index, lhs_type] = eq.lhs_head.split();
					switch (lhs_type) {
					case PnType(Op::sum): 
						[[fallthrough]];
					case PnType(Op::product): {
						std::uint32_t last_node_idx = store.insert(PnTypedIdxSLC{ eq.rhs_head });
						eq.rhs_head = PnTypedIdx(last_node_idx, lhs_type); //new eq.rhs_head is product (sum) of old eq.rhs_head divided by (minus) eq.lhs_head factors (summands).
						for (const PnTypedIdx elem : vc::range(PnRef(store, eq.lhs_head))) {
							if (tree::contains(PnRef(store, elem), to_isolate)) {
								eq.lhs_head = elem; 
							}
							else {
								const PnTypedIdx new_rhs_elem = (lhs_type == Op::sum ? 
									build_negated <PnStore, PnTypedIdx>(store, elem) : 
									build_inverted<PnStore, PnTypedIdx>(store, elem));
								last_node_idx = PnTypedIdxSLC::insert_new(store, last_node_idx, new_rhs_elem);
							}
						}
						PnTypedIdxSLC::free_slc(store, lhs_index); //all factors (summands) have been shifted to rhs -> delete SLC (but not recursively, elems have new owner!)
					} break;
					case PnType(Op::generic_function): 
						assert(false); break;
					case PnType(Leaf::variable): 
						assert(false); break;
					case PnType(Leaf::complex): 
						assert(false); break;
					case PnType(pattern::_tree_match): 
						assert(false); break;
					case PnType(pattern::_value_match): 
						assert(false); break;
					case PnType(pattern::_value_proxy):
						assert(false); break;
					case PnType(Fn::pow): {
						FnParams<PnTypedIdx>* params = &store.at(lhs_index).fn_params;
						if (tree::contains(PnRef(store, (*params)[0]), to_isolate)) { //case <contains var>^<computable>
							eq.lhs_head = (*params)[0u];
							(*params)[0u] = eq.rhs_head;	
							const PnTypedIdx inverse_expo = build_inverted<PnStore, PnTypedIdx>(store, (*params)[1u]);
							params = &store.at(lhs_index).fn_params;
							(*params)[1u] = inverse_expo;
							eq.rhs_head = PnTypedIdx(lhs_index, PnType(Fn::pow));
						}
						else { //case <cumputable>^<contains var>
							eq.lhs_head = (*params)[1u];
							(*params)[0u] = eq.rhs_head;	
							eq.rhs_head = PnTypedIdx(lhs_index, PnType(Fn::log));
						}
					} break;
					case PnType(Fn::sqrt): { //repurpose params from lhs as pow in "<prev. rhs> ^ 2"
						FnParams<PnTypedIdx>* params = &store.at(lhs_index).fn_params;
						eq.lhs_head = (*params)[0u];
						(*params)[0u] = eq.rhs_head;
						const PnTypedIdx square = build_value<PnTypedIdx>(store, 2.0);
						params = &store.at(lhs_index).fn_params;
						(*params)[1u] = square;	
						eq.rhs_head = PnTypedIdx(lhs_index, PnType(Fn::pow));
					} break;
					default:
						assert(false);
					}
				}
				return eq;
			} //stupid_solve_for

		} //namespace pn_tree
		
	} //namespace pattern

	namespace fn {

		template<typename Union_T1, typename Type_T1, typename Union_T2, typename Type_T2>
		std::strong_ordering compare_name(const BasicRef<Union_T1, Type_T1> ref_1, const BasicRef<Union_T2, Type_T2> ref_2)
		{
			assert(ref_1.type == Op::generic_function && ref_2.type == Op::generic_function);
			const GenericFunction& fn_1 = *ref_1;
			const GenericFunction& fn_2 = *ref_2;
			if (fn_1.name_size == GenericFunction::NameSize::small &&
				fn_2.name_size == GenericFunction::NameSize::small) 
			{//change to comparison of std::string_view when they support <=>
				return compare_arrays(fn_1.short_name, fn_2.short_name, GenericFunction::short_name_max);				
			}
			else if (fn_1.name_size == GenericFunction::NameSize::longer &&
			         fn_2.name_size == GenericFunction::NameSize::longer) 			
			{
				return string_compare(ref_1.unsave_at(fn_1.long_name_idx), ref_2.unsave_at(fn_2.long_name_idx));
			}
			else {
				return fn_1.name_size == GenericFunction::NameSize::small ?
					std::strong_ordering::less : 
					std::strong_ordering::greater;
			}
		}

	} //namespace fn

	namespace tree {

		template<typename Union_T, typename Type_T>
		void free(const BasicMutRef<Union_T,Type_T> ref)
		{
			using TypedIdx_T = BasicTypedIdx<Type_T>;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			switch (ref.type) {
			case Type_T(Op::sum): 
				[[fallthrough]];
			case Type_T(Op::product): {
				for (const TypedIdx_T elem : vc::range(ref)) {
					tree::free(ref.new_at(elem));
				}
				TypedIdxSLC_T::free_slc(*ref.store, ref.index);
			} break;
			case Type_T(Op::generic_function): {
				for (const TypedIdx_T param : fn::range(ref)) {
					tree::free(ref.new_at(param));
				}
				const GenericFunction& generic_function = *ref;
				TypedIdxSLC_T::free_slc(*ref.store, generic_function.params_idx);
				if (generic_function.name_size == GenericFunction::NameSize::longer) {
					TermString128::free_slc(*ref.store, generic_function.long_name_idx);
				}
				ref.free();
			} break;
			default: {
				assert(ref.type.is<Fn>());
				for (const TypedIdx_T param : fn::range(ref->fn_params, ref.type)) {
					tree::free(ref.new_at(param));
				}
				ref.free();
			} break;
			case Type_T(Leaf::variable): {
				TermString128::free_slc(*ref.store, ref.index);
			} break;
			case Type_T(Leaf::complex): {
				ref.free();
			} break;
			case Type_T(pattern::_tree_match): if constexpr (pattern) {
				ref.free();
			} break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				pattern::ValueMatchVariable& var = *ref;
				tree::free(ref.new_at(var.match_idx));
				tree::free(ref.new_at(var.copy_idx));
				ref.free();
			} break;
			case Type_T(pattern::_value_proxy):
				break;
			}
		} //free

		template<typename Union_T, typename Type_T>
		void combine_layers(BasicMutRef<Union_T,Type_T> ref)
		{
			using TypedIdx_T = BasicTypedIdx<Type_T>;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			switch (ref.type) {
			case Type_T(Op::sum):
				[[fallthrough]];
			case Type_T(Op::product): {
				std::uint32_t current_append_node = ref.index;
				for (auto& elem : vc::range(ref)) {
					const auto [elem_idx, elem_type] = elem.split();
					if (elem_type == ref.type) {
						elem = TypedIdxSLC_T::null_value;
						current_append_node = TypedIdxSLC_T::append(*ref.store, current_append_node, elem_idx);
					}
					else {
						tree::combine_layers(ref.new_at(elem));
					}
				}
			} break;
			case Type_T(Op::generic_function): {
				const GenericFunction& generic_function = *ref;
				for (const auto param : fn::range(ref)) {
					tree::combine_layers(ref.new_at(param));
				}
			} break;
			default: {
				assert(ref.type.is<Fn>());
				const FnParams<TypedIdx_T>& params = *ref;
				for (const auto param : fn::range(params, ref.type)) {
					tree::combine_layers(ref.new_at(param));
				}
			} break;
			case Type_T(Leaf::variable):
				break;
			case Type_T(Leaf::complex):
				break;
			case Type_T(pattern::_tree_match):
				break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				pattern::ValueMatchVariable& var = *ref;
				tree::combine_layers(ref.new_at(var.match_idx));
				tree::combine_layers(ref.new_at(var.copy_idx));
			} break;
			case Type_T(pattern::_value_proxy):
				break;
			}
		} //combine_layers

		template<typename Union_T, typename Type_T>
		OptComplex combine_values_inexact(const BasicMutRef<Union_T,Type_T> ref)
		{
			using TypedIdx_T = BasicTypedIdx<Type_T>;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			switch (ref.type) {
			case Type_T(Op::sum): {
				OptComplex result_val = 0.0;
				bool only_values = true;
				for (auto& summand : vc::range(ref)) {
					if (const OptComplex summand_val = tree::combine_values_inexact(ref.new_at(summand))) {
						result_val += summand_val;
						tree::free(ref.new_at(summand));
						summand = TypedIdxSLC_T::null_value;
					}
					else {
						only_values = false;
					}
				}
				if (only_values) {
					return  result_val;
				}
				else if (*result_val != 0.0) {
					const auto new_summand = TypedIdx_T(ref.store->insert(*result_val), Type(Leaf::complex));
					TypedIdxSLC_T::insert_new(*ref.store, ref.index, new_summand);
				}
			} break;
			case Type_T(Op::product): {
				OptComplex result_val = 1.0;
				bool only_values = true;
				for (auto& factor : vc::range(ref)) {
					if (const OptComplex factor_val = tree::combine_values_inexact(ref.new_at(factor))) {
						result_val *= factor_val;
						tree::free(ref.new_at(factor));
						factor = TypedIdxSLC_T::null_value;
					}
					else {
						only_values = false;
					}
				}
				if (only_values) {
					return result_val;
				}
				else if (*result_val != 1.0) {
					const auto new_factor = TypedIdx_T(ref.store->insert(*result_val), Type(Leaf::complex));
					TypedIdxSLC_T::insert_new(*ref.store, ref.index, new_factor);
				}
			} break;
			case Type_T(Op::generic_function): {
				GenericFunction& function = *ref;
				for (auto& elem : fn::range(ref)) {
					if (const OptComplex param_res = tree::combine_values_inexact(ref.new_at(elem))) {
						tree::free(ref.new_at(elem));
						elem = TypedIdx_T(ref.store->insert(*param_res), Type(Leaf::complex));
					}
				}
			} break;
			default: {
				assert(ref.type.is<Fn>()); //if this assert hits, the switch above needs more cases.
				FnParams<TypedIdx_T>& params = *ref;
				std::array<OptComplex, 4> results_values;
				bool all_computable = true;
				for (std::size_t i = 0; i < fn::param_count(ref.type); i++) {
					results_values[i] = tree::combine_values_inexact(ref.new_at(params[i]));
					all_computable &= (bool) results_values[i];
				}
				if (all_computable) {
					return fn::eval(ref.type.to<Fn>(), results_values);
				}
				for (std::size_t i = 0; i < fn::param_count(ref.type); i++) {
					if (results_values[i]) {
						tree::free(ref.new_at(params[i]));
						params[i] = TypedIdx_T(ref.store->insert(*results_values[i]), Type(Leaf::complex));
					}
				}
			} break; 
			case Type_T(Leaf::variable): 
				break;
			case Type_T(Leaf::complex): 
				return ref->complex;
			case Type_T(pattern::_tree_match): 
				break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				pattern::ValueMatchVariable& var = *ref;
				const Complex match_res = tree::combine_values_inexact(store);
				const Complex copy_res = tree::combine_values_inexact(store);
				assert(!is_valid(match_res)); //pattern variable can not decay to value
				assert(!is_valid(copy_res));  //pattern variable can not decay to value
			} break; 
			case Type_T(pattern::_value_proxy):
				break;
			}
			return {};
		} //combine_values_inexact

		template<typename Union_T, typename Type_T>
		OptComplex combine_values_exact(const BasicMutRef<Union_T,Type_T> ref)
		{
			using TypedIdx_T = BasicTypedIdx<Type_T>;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3u>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			const auto get_divisor = [](BasicMutRef<Union_T, Type_T> ref) -> std::optional<TypedIdx_T> {
				if (ref.type == Fn::pow) {
					const FnParams<TypedIdx_T>& params = *ref;
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
			case Type_T(Op::sum): {
				OptComplex result_val = 0.0;
				bool only_exact = true;
				for (auto& summand : vc::range(ref)) {
					if (const OptComplex summand_val = tree::combine_values_exact(ref.new_at(summand))) {
						if (const OptComplex res = compute_exact([&] {return result_val + summand_val; })) {
							result_val = res;
							tree::free(ref.new_at(summand));
							summand = TypedIdxSLC_T::null_value;
							continue;
						}
					}
					only_exact = false;
				}

				if (only_exact) {
					return result_val;
				}
				else if (*result_val != 0.0) {
					const auto new_summand = TypedIdx_T(ref.store->insert(*result_val), Type(Leaf::complex));
					TypedIdxSLC_T::insert_new(*ref.store, ref.index, new_summand);
				}
			} break;
			case Type_T(Op::product): {
				OptComplex result_factor = 1.0;
				OptComplex result_divisor = 1.0;
				bool only_exact = true;
				for (auto& factor : vc::range(ref)) {
					if (const std::optional<TypedIdx_T> divisor = get_divisor(ref.new_at(factor))) {
						if (const OptComplex divisor_val = tree::combine_values_exact(ref.new_at(*divisor))) {
							if (const OptComplex res = compute_exact([&] { return result_divisor * divisor_val; })) {
								result_divisor = res;
								tree::free(ref.new_at(factor)); //free whole factor including pow() and -1
								factor = TypedIdxSLC_T::null_value;
								continue;
							}
						}
					}
					if (const OptComplex factor_val = tree::combine_values_exact(ref.new_at(factor))) {
						if (const OptComplex res = compute_exact([&] {return result_factor * factor_val; })) {
							result_factor = res;
							tree::free(ref.new_at(factor));
							factor = TypedIdxSLC_T::null_value;
							continue;
						}
					}
					only_exact = false;
				}

				const OptComplex result_val = compute_exact([&] { return result_factor / result_divisor; });
				if (only_exact && result_val) {
					return result_val;
				}
				else if (result_val && *result_val != 1.0) {
					const auto new_val = TypedIdx_T(ref.store->insert(*result_val), Type(Leaf::complex));
					TypedIdxSLC_T::insert_new(*ref.store, ref.index, new_val);
				}
				else {
					if (*result_factor != 1.0) {
						const auto new_factor = TypedIdx_T(ref.store->insert(*result_factor), Type(Leaf::complex));
						TypedIdxSLC_T::insert_new(*ref.store, ref.index, new_factor);
					}
					if (*result_divisor != 1.0) {
						const auto new_divisor = TypedIdx_T(ref.store->insert(*result_divisor), Type(Leaf::complex));
						const auto new_pow = build_inverted<BasicStore<Union_T>, TypedIdx_T>(*ref.store, new_divisor);
						TypedIdxSLC_T::insert_new(*ref.store, ref.index, new_pow);
					}
				}
			} break;
			case Type_T(Op::generic_function): {
				GenericFunction& function = *ref;
				for (auto& elem : fn::range(ref)) {
					const OptComplex param_res = tree::combine_values_exact(ref.new_at(elem));
					if (param_res) {
						tree::free(ref.new_at(elem));
						elem = TypedIdx_T(ref.store->insert(*param_res), Type(Leaf::complex));
					}
				}
			} break;
			default: {
				assert(ref.type.is<Fn>()); //if this assert hits, the switch above needs more cases.
				FnParams<TypedIdx_T>& params = *ref;
				std::array<OptComplex, 4> res_vals;
				bool only_exact = true;
				for (std::size_t i = 0; i < fn::param_count(ref.type); i++) {
					res_vals[i] = tree::combine_values_exact(ref.new_at(params[i]));
					only_exact &= (bool) res_vals[i];
				}
				if (only_exact) {
					if (const auto res = compute_exact([&] { return fn::eval(ref.type.to<Fn>(), res_vals); })) {
						return res;
					}
				}
				for (std::size_t i = 0; i < fn::param_count(ref.type); i++) {
					if (res_vals[i]) {
						tree::free(ref.new_at(params[i]));
						params[i] = TypedIdx_T(ref.store->insert(*res_vals[i]), Type(Leaf::complex));
					}
				}
			} break;
			case Type_T(Leaf::variable): 
				break;
			case Type_T(Leaf::complex): 
				return ref->complex;
			case Type_T(pattern::_tree_match): 
				break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				pattern::ValueMatchVariable& var = *ref;
				const OptComplex match_res = tree::combine_values_exact(ref.new_at(var.match_idx));
				const OptComplex copy_res = tree::combine_values_exact(ref.new_at(var.copy_idx));
				assert(!match_res); //pattern variable can not decay to value
				assert(!copy_res);  //pattern variable can not decay to value
			} break;
			case Type_T(pattern::_value_proxy):
				break;
			}
			return {};
		} //combine_values_exact

		template<typename Union_T1, typename Type_T1, typename Union_T2, typename Type_T2>
		[[nodiscard]] std::strong_ordering compare(const BasicRef<Union_T1,Type_T1> ref_1, const BasicRef<Union_T2,Type_T2> ref_2)
		{
			using TypedIdx_T = BasicTypedIdx<Type_T1>;
			constexpr bool pattern = std::is_same_v<Type_T1, pattern::PnType>;

			if (ref_1.type != ref_2.type) [[likely]] {
				static_assert((uniqueness(Type(Op::sum)) <=> uniqueness(Type(Op::sum))) == std::strong_ordering::equal); //dont wanna mix with std::strong_ordering::equivalent
				return uniqueness(ref_1.type) <=> uniqueness(ref_2.type);
			}

			switch (ref_1.type) {
			case Type_T1(Op::sum):
				[[fallthrough]];
			case Type_T1(Op::product): {
				auto range_1 = vc::range(ref_1);
				auto range_2 = vc::range(ref_2);
				auto iter_1 = range_1.begin();
				auto iter_2 = range_2.begin();
				for (; iter_1 != range_1.end() && iter_2 != range_2.end(); ++iter_1, ++iter_2) {
					const auto iter_compare = tree::compare(ref_1.new_at(*iter_1), ref_2.new_at(*iter_2));
					if (iter_compare != std::strong_ordering::equal) [[likely]] {
						return iter_compare;
					}
				}
				if (iter_1 == range_1.end() && iter_2 == range_2.end()) {
					return std::strong_ordering::equal;
				}
				else {
					return iter_1 == range_1.end() ?
						std::strong_ordering::less :
						std::strong_ordering::greater;
				}
			} break;
			case Type_T1(Op::generic_function): {
				const auto name_cmp = fn::compare_name(ref_2, ref_1); //reverse order, as to_pretty_string reverses again
				if (name_cmp != std::strong_ordering::equal) {
					return name_cmp;
				}
				auto range_1 = fn::range(ref_1);
				auto range_2 = fn::range(ref_2);
				auto iter_1 = range_1.begin();
				auto iter_2 = range_2.begin();
				for (; iter_1 != range_1.end() && iter_2 != range_2.end(); ++iter_1, ++iter_2) {
					const auto iter_compare = tree::compare(ref_1.new_at(*iter_1), ref_2.new_at(*iter_2));
					if (iter_compare != std::strong_ordering::equal) [[likely]] {
						return iter_compare;
					}
				}
				if (iter_1 == range_1.end() && iter_2 == range_2.end()) {
					return std::strong_ordering::equal;
				}
				else {
					return iter_1 == range_1.end() ?
						std::strong_ordering::less :
						std::strong_ordering::greater;
				}
			} break;
			default: {
				assert(ref_1.type.is<Fn>() && ref_2.type.is<Fn>()); //if this assert hits, the switch above needs more cases.
				auto range_1 = fn::range(ref_1->fn_params, ref_1.type);
				auto range_2 = fn::range(ref_2->fn_params, ref_2.type);
				auto iter_1 = range_1.begin();
				auto iter_2 = range_2.begin();
				for (; iter_1 != range_1.end(); ++iter_1, ++iter_2) { //iter_1 and iter_2 both go over same number of params
					const auto iter_compare = tree::compare(ref_1.new_at(*iter_1), ref_2.new_at(*iter_2));
					if (iter_compare != std::strong_ordering::equal) [[likely]] {
						return iter_compare;
					}
				}
				return std::strong_ordering::equal;
			} break;
			case Type_T1(Leaf::variable): {
				return string_compare(ref_2, ref_1); //reverse order, as to_pretty_string reverses again
			} break;
			case Type_T1(Leaf::complex): {
				const Complex& complex_1 = *ref_1;
				const Complex& complex_2 = *ref_2;
				return compare_complex(complex_2, complex_1); //reverse order, as to_pretty_string reverses again
			} break;
			case Type_T1(pattern::_tree_match): if constexpr (pattern) {
				const pattern::TreeMatchVariable& var_1 = *ref_1;
				const pattern::TreeMatchVariable& var_2 = *ref_2;
				return var_1.match_data_idx <=> var_2.match_data_idx; //reverse to make pretty_string prettier
			} break;
			case Type_T1(pattern::_value_match): if constexpr (pattern) {
				const pattern::ValueMatchVariable& var_1 = *ref_1;
				const pattern::ValueMatchVariable& var_2 = *ref_2;
				if (var_1.form != var_2.form) {
					return var_1.form <=> var_2.form;
				}
				if (var_1.match_data_idx != var_2.match_data_idx) {
					return var_1.match_data_idx <=> var_2.match_data_idx;
				}
				if (const auto cmp = tree::compare(ref_1.new_at(var_1.match_idx), ref_2.new_at(var_2.match_idx)); cmp != std::strong_ordering::equal) {
					return cmp;
				}
				return tree::compare(ref_1.new_at(var_1.copy_idx), ref_2.new_at(var_2.copy_idx));
			} break;
			case Type_T1(pattern::_value_proxy): if constexpr (pattern) {
				return ref_1.index <=> ref_2.index;
			} break;
			}
			assert(false); 
			return std::strong_ordering::equal;
		} //compare

		template<typename Union_T, typename Type_T>
		void sort(const BasicMutRef<Union_T,Type_T> ref)
		{
			using TypedIdx_T = BasicTypedIdx<Type_T>;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3u>;

			const auto sort_variadic = [&](BasicMutRef<Union_T, Type_T> ref) {
				const auto compare_function = [&](const TypedIdx_T lhs, const TypedIdx_T rhs) {
					using Ref_T = BasicRef<Union_T, Type_T>;
					return tree::compare(Ref_T(*ref.store, lhs), Ref_T(*ref.store, rhs)) == std::strong_ordering::less;
				};

				if (ref.type == Op::sum || ref.type == Op::product) {
					TypedIdxSLC_T::sort(ref, compare_function);						
				}
				return fold::Void{};
			};

			fold::simple_fold<fold::Void>(ref, sort_variadic);
		} //sort

		template<typename Union_T, typename Type_T>
		std::size_t count(const BasicRef<Union_T,Type_T> ref)
		{
			struct OpAccumulator
			{
				std::size_t acc;

				constexpr OpAccumulator(const BasicRef<Union_T, Type_T>) noexcept :acc(1u) {}
				void consume(const std::size_t child_size) noexcept { this->acc += child_size; }
				auto result() noexcept { return this->acc; }
			};

			return fold::tree_fold<std::size_t, OpAccumulator>(ref, [](auto) { return 1u; });
		} //count
		template std::size_t count<TypesUnion, Type>(Ref ref);

		template<typename Union_T, typename Type_T>
		[[nodiscard]] BasicTypedIdx<Type_T> copy(const BasicRef<Union_T,Type_T> src_ref, BasicStore<Union_T>& dst_store)
		{
			using TypedIdx_T = BasicTypedIdx<Type_T>;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool src_pattern = std::is_same_v<Type_T, pattern::PnType>;

			switch (src_ref.type) {
			case Type_T(Op::sum): 
				[[fallthrough]];
			case Type_T(Op::product): {
				const std::uint32_t dst_index = dst_store.insert(TypedIdxSLC_T());
				std::uint32_t last_node_idx = dst_index;
				for (const auto src_elem : vc::range(src_ref)) {
					const TypedIdx_T dst_elem = tree::copy(src_ref.new_at(src_elem), dst_store);
					last_node_idx = TypedIdxSLC_T::insert_new(dst_store, last_node_idx, dst_elem);
				}
				return TypedIdx_T(dst_index, src_ref.type);
			} break;
			case Type_T(Op::generic_function): {
				const GenericFunction src_function = *src_ref; //no reference, as src and dst could be same store -> may reallocate
				GenericFunction dst_function;
				std::uint32_t last_node_idx = dst_store.insert(TypedIdxSLC_T());
				dst_function.params_idx = last_node_idx;
				for (const auto src_param : fn::range(src_ref)) {
					const TypedIdx_T dst_param = tree::copy(src_ref.new_at(src_param), dst_store);
					last_node_idx = TypedIdxSLC_T::insert_new(dst_store, last_node_idx, dst_param);
				}
				std::string src_name; //in most cases the small string optimisation works, else dont care
				read(src_ref, src_name);
				if (src_name.size() <= GenericFunction::short_name_max) [[likely]] {
					std::copy(src_name.begin(), src_name.end(), dst_function.short_name);
					dst_function.name_size = GenericFunction::NameSize::small;
				}
				else {
					dst_function.long_name_idx = insert_string(dst_store, src_name);
					dst_function.name_size = GenericFunction::NameSize::longer;
				}
				return TypedIdx_T(dst_store.insert(dst_function), src_ref.type);
			} break;
			default: {
				assert(src_ref.type.is<Fn>());
				const FnParams<TypedIdx_T> src_params = *src_ref; //no reference, as src and dst could be same store -> may reallocate
				auto dst_params = FnParams<TypedIdx_T>();
				for (std::size_t i = 0u; i < fn::param_count(src_ref.type); i++) {
					dst_params[i] = tree::copy(src_ref.new_at(src_params[i]), dst_store);
				}
				return TypedIdx_T(dst_store.insert(dst_params), src_ref.type);
			} break;
			case Type_T(Leaf::variable): {
				std::string src_name; //in most cases the small string optimisation works, else dont care
				read(src_ref, src_name);
				const std::size_t dst_index = insert_string(dst_store, src_name);
				return TypedIdx_T(dst_index, src_ref.type);
			} break;
			case Type_T(Leaf::complex): {
				const std::size_t dst_index = dst_store.insert(*src_ref);
				return TypedIdx_T(dst_index, src_ref.type);
			} break;
			case Type_T(pattern::_tree_match): if constexpr (src_pattern) {
				const std::size_t dst_index = dst_store.insert(*src_ref); //shallow copy of var
				return TypedIdx_T(dst_index, src_ref.type);
			} break;
			case Type_T(pattern::_value_match): if constexpr (src_pattern) {
				const pattern::ValueMatchVariable src_var = *src_ref;
				auto dst_var = pattern::ValueMatchVariable(src_var.match_data_idx, src_var.form);
				dst_var.match_idx = tree::copy(src_ref.new_at(src_var.match_idx), dst_store);
				dst_var.copy_idx = tree::copy(src_ref.new_at(src_var.copy_idx), dst_store);
				const std::size_t dst_index = dst_store.insert(dst_var);
				return TypedIdx_T(dst_index, src_ref.type);
			} break;
			case Type_T(pattern::_value_proxy):
				return src_ref.typed_idx(); //return same ref, as proxy does not own any nodes in src_store anyway (index has different meaning)
			}
			assert(false); 
			return TypedIdx_T();
		} //copy
		template TypedIdx copy<TypesUnion, Type>(Ref src_ref, Store& dst_store);
		template pattern::PnTypedIdx copy<pattern::PnTypesUnion, pattern::PnType> (pattern::PnRef ref, pattern::PnStore& dst_store);


		template<typename Union_T, typename Type_T>
		bool contains(const BasicRef<Union_T,Type_T> ref, const BasicTypedIdx<Type_T> to_contain)
		{
			return fold::simple_fold<fold::FindBool>(ref, 
				[to_contain](const BasicRef<Union_T, Type_T> ref) -> fold::FindBool 
				{ return BasicTypedIdx<Type_T>(ref.index, ref.type) == to_contain; }
			);
		} //contains

		template<typename Union_T, typename Type_T>
		BasicTypedIdx<Type_T> establish_basic_order(const BasicMutRef<Union_T,Type_T> ref)
		{
			using TypedIdx_T = BasicTypedIdx<Type_T>;

			BasicTypedIdx<Type_T> new_head = ref.typed_idx();
			if (ref.type != Leaf::complex) {
				tree::combine_layers(ref);
				if (const OptComplex val = tree::combine_values_exact(ref)) {
					tree::free(ref);
					new_head = TypedIdx_T(ref.store->insert(*val), Type(Leaf::complex));
				}
				tree::sort(ref);
			}
			return new_head;
		} //establish_basic_order

		template<typename Union_T, typename TypedIdx_T>
		TypedIdx_T* find_subtree_owner(BasicStore<Union_T>& store, TypedIdx_T* const head, const TypedIdx_T subtree)
		{
			using Type_T = TypedIdx_T::Enum_T;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			if (*head == subtree) {
				return head;
			}
			else {
				const auto [index, type] = head->split();
				switch (type) {
				case Type_T(Op::sum): 
					[[fallthrough]];
				case Type_T(Op::product): {
					for (TypedIdx_T& elem : vc::range(BasicMutRef<Union_T, Type_T>(store, *head))) {
						if (TypedIdx_T* const elem_res = tree::find_subtree_owner(store, &elem, subtree)) {
							return elem_res;
						}
					}
				} break;
				case Type_T(Op::generic_function): {
					for (TypedIdx_T& param : fn::range(BasicMutRef<Union_T, Type_T>(store, *head))) {
						if (TypedIdx_T* const param_res = tree::find_subtree_owner(store, &param, subtree)) {
							return param_res;
						}
					}
				} break;
				default: {
					assert(type.is<Fn>()); //if this assert hits, the switch above needs more cases.
					FnParams<TypedIdx_T>& params = store.at(index).fn_params;
					for (TypedIdx_T& param : fn::range(params, type)) {
						if (TypedIdx_T* const param_res = tree::find_subtree_owner(store, &param, subtree)) {
							return param_res;
						}
					}
				} break;
				case Type_T(Leaf::variable): 
					break;
				case Type_T(Leaf::complex): 
					break;
				case Type_T(pattern::_tree_match): 
					break;
				case Type_T(pattern::_value_match): if constexpr (pattern) {
					pattern::ValueMatchVariable& var = store.at(index).value_match;
					if (TypedIdx_T* const copy_res = tree::find_subtree_owner(store, &var.copy_idx, subtree)) {
						return copy_res;
					}
					if (TypedIdx_T* const match_res = tree::find_subtree_owner(store, &var.match_idx, subtree)) {
						return match_res;
					}
				} break;
				case Type_T(pattern::_value_proxy):
					break;
				}
				return nullptr;
			}

		} //find_subtree_owner

		bool contains_variables(const Ref ref)
		{
			return fold::simple_fold<fold::FindBool>(ref, [](Ref ref) -> fold::FindBool { return ref.type == Leaf::variable; });
		} //contains_variables

		bool contains_variables(const pattern::PnRef ref)
		{
			using namespace pattern;
			const auto test_for_variables = [](pattern::PnRef ref) -> fold::FindBool {
				return ref.type == Leaf::variable || ref.type.is<PnVariable>();
			};
			return fold::simple_fold<fold::FindBool>(ref, test_for_variables);
		} //contains_variables

		TypedIdx search_variable(const Ref ref, std::string_view name)
		{
			const auto test_for_name = [name](Ref ref) -> fold::Find<TypedIdx> {
				return (ref.type == Leaf::variable && string_compare(ref, name) == std::strong_ordering::equal) ?
					fold::done(TypedIdx(ref.index, ref.type)) : //name was found -> cut tree evaluation here
					fold::more(TypedIdx());
			};
			return *fold::simple_fold<fold::Find<TypedIdx>>(ref, test_for_name);
		} //search_variable

		bool match(const pattern::PnRef pn_ref, const Ref ref, pattern::MatchData& match_data)
		{
			if (pn_ref.type.is<Type>()) {
				if (pn_ref.type != ref.type) [[likely]] {
					return false;
				}
				else {
					switch (ref.type) {
					case Type(Op::sum): {
						assert(false);
					} break;
					case Type(Op::product): {
						assert(false);
					} break;
					case Type(Op::generic_function): {
						assert(false);
					} break;
					default: {
						assert(ref.type.is<Fn>()); //if this assert hits, the switch above needs more cases.
						auto range = fn::range(ref->fn_params, ref.type);
						auto pn_range = fn::range(pn_ref->fn_params, pn_ref.type);
						auto iter = range.begin();
						auto pn_iter = pn_range.begin();
						for (; iter != range.end(); ++iter, ++pn_iter) { //iter and pn_iter both go over same number of params
							if (!tree::match(pn_ref.new_at(*pn_iter), ref.new_at(*iter), match_data)) {
								return false;
							}
						}
						return true;
					} break;
					case Type(Leaf::variable):
						return string_compare(pn_ref, ref) == std::strong_ordering::equal;
					case Type(Leaf::complex): {
						const Complex& complex = *ref;
						const Complex& pn_complex = *pn_ref;
						return compare_complex(complex, pn_complex) == std::strong_ordering::equal;
					} break;
					}
				}
			}
			return false;
			
		} //match

	} //namespace tree

	namespace fold {

		template<typename Res_T, typename Union_T, typename Type_T, bool Const, typename Apply>
		Res_T simple_fold(const BasicRef<Union_T, Type_T, Const> ref, Apply apply)
		{
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;
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
			case Type_T(Op::generic_function): {
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
			case Type_T(pattern::_tree_match): 
				break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				const pattern::ValueMatchVariable var = *ref;
				const Res_T elem_res_1 = fold::simple_fold<Res_T>(ref.new_at(var.match_idx), apply);
				if constexpr (return_early_possible) { if (elem_res_1.return_early()) { return elem_res_1; } }
				const Res_T elem_res_2 = fold::simple_fold<Res_T>(ref.new_at(var.copy_idx), apply);
				if constexpr (return_early_possible) { if (elem_res_2.return_early()) { return elem_res_2; } }
			} break;
			case Type_T(pattern::_value_proxy):
				break;
			}
			return apply(ref); 
		} //simple_fold

		template<typename Res_T, typename OpAccumulator, typename Union_T, typename Type_T, bool Const, typename LeafApply, typename... AccInit>
		Res_T tree_fold(const BasicRef<Union_T, Type_T, Const> ref, LeafApply leaf_apply, const AccInit... init)
		{
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

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
			case Type_T(Op::generic_function): {
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
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				OpAccumulator acc(ref, init...);
				const pattern::ValueMatchVariable var = *ref;
				acc.consume(fold::tree_fold<Res_T, OpAccumulator>(ref.new_at(var.match_idx), leaf_apply, init...));
				acc.consume(fold::tree_fold<Res_T, OpAccumulator>(ref.new_at(var.copy_idx), leaf_apply, init...));
				return acc.result();
			} [[fallthrough]];
			case Type_T(Leaf::variable): 
				[[fallthrough]];
			case Type_T(Leaf::complex):
				[[fallthrough]];
			case Type_T(pattern::_tree_match): 
				[[fallthrough]];
			case Type_T(pattern::_value_proxy):
				return leaf_apply(ref);
			}
		} //tree_fold

	} //namespace fold

} //namespace bmath::intern


namespace bmath {
	using namespace intern;

	Term::Term(std::string& name)
		:store(name.size() / 2)
	{
		auto parse_string = ParseString(name);
		parse_string.allow_implicit_product();
		parse_string.remove_space();
		const std::size_t error_pos = find_first_not_arithmetic(TokenView(parse_string.tokens));
		intern::throw_if<ParseFailure>(error_pos != TokenView::npos, error_pos, "illegal character");
		this->head = build(this->store, parse_string);
		name = std::move(parse_string.name); //give content of name back to name
	} //Term

	void Term::combine_layers() noexcept
	{
		tree::combine_layers(MutRef(this->store, this->head));
	}

	void Term::combine_values_inexact() noexcept
	{
		if (const OptComplex val = tree::combine_values_inexact(this->ref())) {
			tree::free(this->ref());
			this->head = TypedIdx(this->store.insert(*val), Leaf::complex);
		}	
	}

	void Term::combine_values_exact() noexcept
	{
		if (const OptComplex val = tree::combine_values_exact(this->ref())) {
			tree::free(MutRef(this->store, this->head));
			this->head = TypedIdx(this->store.insert(*val), Leaf::complex);
		}
	}

	void Term::sort() noexcept
	{
		tree::sort(this->ref());
	}

	std::string bmath::Term::to_memory_layout() const
	{
		return print::to_memory_layout(this->store, { this->head });
	} //to_memory_layout

	std::string Term::to_string() const
	{
		std::string result;
		result.reserve(this->store.size() * 2);
		print::append_to_string(static_cast<const Term*>(this)->ref(), result);
		return result;
	}

	std::string Term::to_pretty_string()
	{
		this->combine_layers();
		this->combine_values_exact();
		this->sort();
		return print::to_pretty_string(static_cast<const Term*>(this)->ref());
	}

	std::string Term::to_pretty_string() const
	{
		return print::to_pretty_string(static_cast<const Term*>(this)->ref());
	}

	MutRef Term::ref() noexcept
	{
		return MutRef(this->store, this->head);
	}

	Ref Term::ref() const noexcept
	{
		return Ref(this->store, this->head);
	}

} //namespace bmath