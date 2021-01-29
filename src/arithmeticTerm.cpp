
#include <cassert>
#include <sstream>
#include <array>
#include <algorithm>
#include <cstring>
#include <cfenv>
#include <compare>
#include <numeric>

#include <iostream>

#include "arithmeticTerm.hpp"
#include "ioArithmetic.hpp"
#include "pattern.hpp"
#include "fold.hpp"

namespace bmath::intern {

	namespace fn {

		OptComplex eval(Fn type, const std::array<OptComplex, 4>& param_vals)
		{
			if (arity(type) == 1u) {
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
					default: return {};
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
					default: return {};
					}
				}
			}
			else if (arity(type) == 2u) {
				switch (type) {
				case Fn::pow: 
					if (*param_vals[1] == 0.5) {
						return (param_vals[0]->imag() == 0.0 && param_vals[0]->real() >= 0.0) ?
							std::sqrt(param_vals[0]->real()) :
							std::sqrt(*param_vals[0]);
					}
					else {
						return std::pow(*param_vals[0], *param_vals[1]);
					}
				case Fn::log: 
					//https://en.wikipedia.org/wiki/Complex_logarithm#Generalizations
					if (param_vals[0]->imag() == 0.0 && param_vals[1]->imag() == 0.0) {
						return std::log(param_vals[1]->real()) / std::log(param_vals[0]->real());
					}
					else {
						const OptComplex num = (param_vals[1]->imag() == 0.0) ?
							std::log(param_vals[1]->real()) :
							std::log(*param_vals[1]);
						const OptComplex denom = (param_vals[0]->imag() == 0.0) ?
							std::log(param_vals[0]->real()) :
							std::log(*param_vals[0]);
						return num / denom;
					}
				case Fn::diff:
					[[fallthrough]];
				default:
					return {};
				}
			}
			//parameter count of three or higher
			return {};
		} //eval

	} //namespace fn

	namespace tree {

		void free(const MutRef ref)
		{
			switch (ref.type) {
			default: 
				assert(ref.type.is<Function>());
				for (const TypedIdx elem : fn::unsave_range(ref)) {
					tree::free(ref.new_at(elem));
				}
				if (ref.type.is<NamedFn>()) { //also free name
					const std::uint32_t node_count = ref->parameters.node_count() + fn::named_fn_name(ref).node_count();
					ref.store->free_n(ref.index, node_count);
				}
				else {
					IndexVector::free(*ref.store, ref.index);
				}
				break;
			case Type(Literal::variable): 
				CharVector::free(*ref.store, ref.index);
				break;
			case Type(Literal::complex):
				ref.store->free_one(ref.index);
				break;
			case Type(PnNode::tree_match):
				ref.store->free_one(ref.index);
				break;
			case Type(PnNode::value_match): {
				pattern::ValueMatchVariable& var = *ref;
				tree::free(ref.new_at(var.mtch_idx));
				tree::free(ref.new_at(var.copy_idx));
				ref.store->free_one(ref.index);
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
				return (!exact || !std::fetestexcept(FE_ALL_EXCEPT)) ? result : OptComplex();
			};

			switch (ref.type) {
			case Type(Comm::sum): {
				OptComplex value_acc = 0.0; //stores sum of values encountered as summands
				StupidBufferVector<TypedIdx, 16> new_sum;

				for (TypedIdx& summand : fn::unsave_range(ref)) {
					summand = tree::combine(ref.new_at(summand), exact);
					switch (summand.get_type()) {
					case Type(Comm::sum):
						for (const TypedIdx nested_summand : fn::unsave_range(ref.new_at(summand))) {
							new_sum.push_back(nested_summand);
						}						
						IndexVector::free(*ref.store, summand.get_index()); //free nested sum, but not nested summands
						break;
					case Type(Literal::complex):						
						if (const OptComplex res = compute([&] { return value_acc + ref.store->at(summand.get_index()).complex; })) {
							value_acc = res;
							tree::free(ref.new_at(summand));
							break;
						}
						[[fallthrough]];
					default:
						new_sum.push_back(summand);
						break;
					}
				}

				if (*value_acc != 0.0 || new_sum.size() == 0u) { //== 0u also catches (hopefully impossible?) zero summands case
					new_sum.push_back(build_value(*ref.store, *value_acc));
				}
				
				if (new_sum.size() == 1u) {
					IndexVector::free(*ref.store, ref.index); //free old sum (but not summands)
					return new_sum.front();
				}
				else if (const auto old_capacity = ref->parameters.capacity(); new_sum.size() <= old_capacity) [[likely]] {
					IndexVector::emplace(*ref, new_sum, old_capacity);
				}
				else {
					IndexVector::free(*ref.store, ref.index); //free old sum (but not old summands)
					return TypedIdx(IndexVector::build(*ref.store, new_sum), Type(Comm::sum));
				}
			} break;
			case Type(Comm::product): {
				//unlike with summands, where an inversion is just flipping the sign bit, not every multiplicativly inverse of a floating point number 
				//  can be stored as floating point number without rounding -> we need two value accumulators. sigh.
				OptComplex factor_acc = 1.0;
				OptComplex divisor_acc = 1.0; 
				StupidBufferVector<TypedIdx, 16> new_product;

				for (TypedIdx& factor : fn::unsave_range(ref)) {
					factor = tree::combine(ref.new_at(factor), exact);
					switch (factor.get_type()) {
					case Type(Fn::pow): {
						IndexVector& power = *ref.new_at(factor);
						if (power[0].get_type() == Literal::complex && power[1].get_type() == Literal::complex) {
							std::array<OptComplex, 4> power_params = {
								ref.store->at(power[0].get_index()).complex,
								ref.store->at(power[1].get_index()).complex,
							};
							if (power_params[1]->imag() == 0.0 && power_params[1]->real() < 0.0) {
								power_params[1] *= -1.0;
								if (const OptComplex res = compute([&] { return divisor_acc * fn::eval(Fn::pow, power_params); })) {
									divisor_acc = res;
									tree::free(ref.new_at(factor)); //free whole power
									break;
								}
							}
						}
						new_product.push_back(factor);
					} break;
					case Type(Comm::product):
						for (const TypedIdx nested_factor : fn::unsave_range(ref.new_at(factor))) {
							new_product.push_back(nested_factor);
						}
						IndexVector::free(*ref.store, factor.get_index()); //free nested product, but not nested factors
						break;
					case Type(Literal::complex):
						if (const OptComplex res = compute([&] { return factor_acc * ref.store->at(factor.get_index()).complex; })) {
							factor_acc = res;
							tree::free(ref.new_at(factor));
							break;
						}
						[[fallthrough]];
					default:
						new_product.push_back(factor);
						break;
					}
				}

				//reinserting the computed value(s)					
				if (const OptComplex result_val = compute([&] { return factor_acc / divisor_acc; }); 
					result_val && *result_val != 1.0) 
				{
					new_product.push_back(build_value(*ref.store, *result_val));
				}
				else if (*factor_acc != *divisor_acc) {
					if (*factor_acc != 1.0) {
						new_product.push_back(build_value(*ref.store, *factor_acc));
					}
					if (*divisor_acc != 1.0) {
						const std::uint32_t divisor_idx = ref.store->allocate_one();
						new (&ref.store->at(divisor_idx)) MathUnion(*divisor_acc);
						new_product.push_back(build_inverted(*ref.store, TypedIdx(divisor_idx, Literal::complex)));
					}
				}

				if (new_product.size() == 1u) {
					IndexVector::free(*ref.store, ref.index); //free old product (but not old factors)
					return new_product.front();
				}
				else if (new_product.size() == 0u) { 
					//note: it is not sufficient to make ref a complex number with value 1.0, as even an empty product may hold more than one element in store.
					IndexVector::free(*ref.store, ref.index); //might as well call tree::free, but there are no factors anyway.
					return build_value(*ref.store, 1.0);
				}
				else if (const auto old_capacity = ref->parameters.capacity(); new_product.size() <= old_capacity) [[likely]] {
					IndexVector::emplace(*ref, new_product, old_capacity);
				}
				else {
					IndexVector::free(*ref.store, ref.index); //free old product (but not old factors)
					return TypedIdx(IndexVector::build(*ref.store, new_product), Type(Comm::product));
				}
			} break;
			case Type(Fn::force): {
				TypedIdx& param = ref->parameters[0];
				param = tree::combine(ref.new_at(param), false);
				if (param.get_type() == Literal::complex) {
					const TypedIdx result_val = param;
					ref.store->free_one(ref.index);
					return result_val;
				}
			} break;
			default: 
				if (ref.type.is<Fn>()) {
					IndexVector& params = *ref;
					std::array<OptComplex, 4> res_vals = { OptComplex{}, {}, {}, {} }; //default initialized to NaN
					assert(fn::arity(ref.type) <= 4); //else res_vals size to small
					for (std::size_t i = 0; i < fn::arity(ref.type); i++) {
						params[i] = tree::combine(ref.new_at(params[i]), exact);
						if (params[i].get_type() == Literal::complex) {
							res_vals[i] = ref.store->at(params[i].get_index()).complex;
						}
					}
					if (const OptComplex res = compute([&] { return fn::eval(ref.type.to<Fn>(), res_vals); })) {
						tree::free(ref);
						return build_value(*ref.store, *res);
					}
				} 
				else if (ref.type.is<Variadic>() && fn::is_associative(ref.type)) { //-> flatten nested instances allowed
					StupidBufferVector<TypedIdx, 16> new_parameters;
					for (const TypedIdx param : fn::unsave_range(ref)) {
						const TypedIdx new_param = tree::combine(ref.new_at(param), exact);
						if (new_param.get_type() == ref.type) {
							for (const TypedIdx nested_param : fn::unsave_range(ref.new_at(new_param))) {
								new_parameters.push_back(nested_param);
							}
							IndexVector::free(*ref.store, new_param.get_index()); //free nested variadic itself, but not nested params
						}
						else {
							new_parameters.push_back(new_param);
						}
					}
					if (const auto old_capacity = ref->parameters.capacity(); new_parameters.size() <= old_capacity) [[likely]] {
						IndexVector::emplace(*ref, new_parameters, old_capacity);
					}
					else {
						IndexVector::free(*ref.store, ref.index); //free old variadic itself, but not its params
						return TypedIdx(IndexVector::build(*ref.store, new_parameters), ref.type);
					}
				}
				else {
					assert(ref.type.is<Variadic>() || ref.type.is<NamedFn>()); 
					for (TypedIdx& param : fn::unsave_range(ref)) {
						param = tree::combine(ref.new_at(param), exact);

					}
				}
				break;
			case Type(Literal::variable): 
				break;
			case Type(Literal::complex): 
				break;
			case Type(PnNode::tree_match): 
				break;
			case Type(PnNode::value_match): {
				pattern::ValueMatchVariable& var = *ref;
				var.mtch_idx = tree::combine(ref.new_at(var.mtch_idx), exact);
				var.copy_idx = tree::combine(ref.new_at(var.copy_idx), exact);
				assert(var.mtch_idx.get_type() != Literal::complex); //subtree always contains value_proxy, thus should never be evaluatable    
				assert(var.copy_idx.get_type() != Literal::complex);  //subtree always contains value_proxy, thus should never be evaluatable    
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

		[[nodiscard]] std::strong_ordering compare(const UnsaveRef ref_1, const UnsaveRef ref_2)
		{
			const auto compare_char_vecs = [](const CharVector& fst, const CharVector& snd) -> std::strong_ordering {
				const std::size_t size = std::min(fst.size(), snd.size());
				if (const std::strong_ordering cmp = compare_arrays(fst.data(), snd.data(), size); 
					cmp != std::strong_ordering::equal) 
				{
					return cmp;
				}
				return fst.size() <=> snd.size();
			};


			if (ref_1.type != ref_2.type) [[likely]] {
				return generality(ref_1.type) <=> generality(ref_2.type);
			}

			switch (ref_1.type) {
			case Type(NamedFn{}): {
				const CharVector& name_1 = fn::named_fn_name(ref_1);
				const CharVector& name_2 = fn::named_fn_name(ref_2);
				if (const auto cmp = compare_char_vecs(name_1, name_2); cmp != std::strong_ordering::equal) {
					return cmp;
				}
			} [[fallthrough]];
			default: {
				assert(ref_1.type.is<Function>());
				const IndexVector& vector_1 = *ref_1;
				const IndexVector& vector_2 = *ref_2;
				if (const auto cmp = vector_1.size() <=> vector_2.size(); cmp != std::strong_ordering::equal) {
					return cmp;
				}
				const auto stop = vector_1.end(); //both are of equal length
				for (auto iter_1 = vector_1.begin(),
				          iter_2 = vector_2.begin();
					iter_1 != stop; 
					++iter_1,
					++iter_2) 
				{
					const auto iter_compare = tree::compare(ref_1.new_at(*iter_1), ref_2.new_at(*iter_2));
					if (iter_compare != std::strong_ordering::equal) [[likely]] {
						return iter_compare;
					}
				}
				return std::strong_ordering::equal;
			} break;			
			case Type(Literal::variable): {
				return compare_char_vecs(*ref_1, *ref_2);
			} break;
			case Type(Literal::complex): {
				return compare_complex(*ref_1, *ref_2);
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
			const auto sort_variadic = [](const MutRef ref) {
				const auto compare_function = [&](const TypedIdx lhs, const TypedIdx rhs) {
					return tree::compare(Ref(*ref.store, lhs), Ref(*ref.store, rhs)) == std::strong_ordering::less;
				};

				if (ref.type.is<Comm>()) {
					IndexVector& operation = *ref;
					std::sort(operation.begin(), operation.end(), compare_function);
				}
				return fold::Void{};
			};

			fold::simple_fold<fold::Void>(ref, sort_variadic);
		} //sort

		std::size_t count(const UnsaveRef ref)
		{
			struct Acc
			{
				std::size_t acc;

				constexpr Acc(const UnsaveRef) noexcept :acc(1u) {}
				void consume(const std::size_t child_size) noexcept { this->acc += child_size; }
				auto result() noexcept { return this->acc; }
			};

			return fold::tree_fold<std::size_t, Acc>(ref, [](auto) { return 1u; });
		} //count

		[[nodiscard]] TypedIdx copy(const Ref src_ref, MathStore& dst_store)
		{
			switch (src_ref.type) {			
			default: {
				assert(src_ref.type.is<Function>());
				StupidBufferVector<TypedIdx, 12> dst_parameters;
				for (const TypedIdx src_param : fn::save_range(src_ref)) {
					const TypedIdx dst_param = tree::copy(src_ref.new_at(src_param), dst_store);
					dst_parameters.push_back(dst_param);
				}
				if (src_ref.type.is<NamedFn>()) {
					const CharVector& name_ref = fn::named_fn_name(src_ref);
					std::string name = std::string(name_ref.begin(), name_ref.end());
					return fn::build_named_fn(dst_store, std::move(name), dst_parameters);
				}
				else {
					return TypedIdx(IndexVector::build(dst_store, dst_parameters), src_ref.type);
				}
			} break;
			case Type(Literal::variable): {
				const CharVector& src_var = *src_ref;
				const auto src_name = std::string(src_var.data(), src_var.size());
				const std::size_t dst_index = CharVector::build(dst_store, src_name);
				return TypedIdx(dst_index, src_ref.type);
			} break;
			case Type(Literal::complex): 
				[[fallthrough]];
			case Type(PnNode::tree_match): {
				const std::size_t dst_index = dst_store.allocate_one();
				dst_store.at(dst_index) = *src_ref; //bitwise copy of src
				return TypedIdx(dst_index, src_ref.type);
			} break;
			case Type(PnNode::value_match): {
				const pattern::ValueMatchVariable src_var = *src_ref;
				auto dst_var = pattern::ValueMatchVariable(src_var.match_data_idx, src_var.form);
				dst_var.mtch_idx = tree::copy(src_ref.new_at(src_var.mtch_idx), dst_store);
				dst_var.copy_idx = tree::copy(src_ref.new_at(src_var.copy_idx), dst_store);
				const std::size_t dst_index = dst_store.allocate_one();
				dst_store.at(dst_index) = dst_var;
				return TypedIdx(dst_index, src_ref.type);
			} break;
			case Type(PnNode::value_proxy): //return same ref, as proxy does not own any nodes in src_store anyway (index has different meaning)
				[[fallthrough]];
			case Type(MultiPn::summands):
				[[fallthrough]];
			case Type(MultiPn::factors):
				[[fallthrough]];
			case Type(MultiPn::params):
				return src_ref.typed_idx();
			}
			assert(false); 
			return TypedIdx();
		} //copy


		bool contains(const UnsaveRef ref, const TypedIdx to_contain)
		{
			return fold::simple_fold<fold::FindTrue>(ref, 
				[to_contain](const UnsaveRef ref) -> fold::FindTrue
				{ return ref.typed_idx() == to_contain; }
			);
		} //contains

		TypedIdx establish_basic_order(MutRef ref)
		{
			const TypedIdx combine_result = tree::combine(ref, true);
			tree::sort(ref.new_at(combine_result));
			return combine_result;
		} //establish_basic_order

		TypedIdx* find_subtree_owner(MathStore& store, TypedIdx& head, const TypedIdx subtree)
		{
			if (head == subtree) {
				return &head;
			}

			const Type type = head.get_type();
			if (type.is<Function>()) {
				for (TypedIdx& elem : fn::range(MutRef(store, head))) {
					if (TypedIdx* const elem_res = tree::find_subtree_owner(store, elem, subtree)) {
						return elem_res;
					}
				}
			}
			else if (type == PnNode::value_match) {
				pattern::ValueMatchVariable& var = store.at(head.get_index()).value_match;
				if (TypedIdx* const copy_res = tree::find_subtree_owner(store, var.copy_idx, subtree)) {
					return copy_res;
				}
				if (TypedIdx* const match_res = tree::find_subtree_owner(store, var.mtch_idx, subtree)) {
					return match_res;
				}
			}
			return nullptr;

		} //find_subtree_owner

		bool valid_storage(const Ref ref)
		{
			BitVector store_positions = ref.store->storage_occupancy(); //every index in store is represented as bit here. false -> currently free	
			const auto reset_and_check_position = [&store_positions](const Ref ref) -> fold::FindTrue { //doubles in function as 
				if (!ref.type.is<MultiPn>() && !(ref.type == PnNode::value_proxy)) { //else index does not point in store anyway
					const auto set_all_in_range = [&store_positions](std::size_t index, const std::size_t end) {
						while (index < end) {
							if (store_positions.test(index) == false) { //meaning eighter free or already reset by some other node
								return true; //return found double use
							}
							store_positions.reset(index);
							index++;
						}
						return false;
					};
					if (ref.type.is<Function>()) {
						const IndexVector& vec = *ref;
						if (set_all_in_range(ref.index, ref.index + vec.node_count())) return true;
						if (ref.type.is<NamedFn>()) {
							const std::size_t name_index = fn::named_fn_name_index(ref);
							const std::size_t name_node_count = fn::named_fn_name(ref).node_count();
							if (set_all_in_range(name_index, name_index + name_node_count)) return true;
						}
					}					
					else if (ref.type == Literal::variable) {
						const CharVector& vec = *ref;
						if (set_all_in_range(ref.index, ref.index + vec.node_count())) return true;
					}
					else {
						if (set_all_in_range(ref.index, ref.index + 1ull)) return true;
					}
				}
				return false; //found no double use (not at this node anyway)
			};
			const bool found_double_use = fold::simple_fold<fold::FindTrue>(ref, reset_and_check_position);
			return !found_double_use && store_positions.none();
		} //valid_storage

		bool contains_variables(const UnsaveRef ref)
		{
			using namespace pattern;
			const auto test_for_variables = [](const UnsaveRef ref) -> fold::FindTrue {
				return ref.type == Literal::variable || ref.type.is<MatchType>();
			};
			return fold::simple_fold<fold::FindTrue>(ref, test_for_variables);
		} //contains_variables

		TypedIdx search_variable(const UnsaveRef ref, const std::string_view name)
		{
			const auto test_for_name = [name](UnsaveRef ref) -> fold::Find<TypedIdx> {
				return (ref.type == Literal::variable && std::string_view(ref->char_vec.data(), ref->char_vec.size()) == name) ?
					fold::done(TypedIdx(ref.index, ref.type)) : //name was found -> cut tree evaluation here
					fold::more(TypedIdx());
			};
			return *fold::simple_fold<fold::Find<TypedIdx>>(ref, test_for_name);
		} //search_variable	

	} //namespace tree

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

	bool Term::match_and_replace(const intern::pattern::RewriteRule& p) noexcept
	{
		const auto [head_match, deeper_match] = pattern::match::recursive_match_and_replace(p.lhs_ref(), p.rhs_ref(), this->mut_ref());
		if (head_match) {
			this->head = *head_match;
		}
		return head_match || deeper_match;
	}

} //namespace bmath