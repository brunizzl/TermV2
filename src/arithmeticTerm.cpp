
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

		OptionalComplex eval(Fn type, const std::array<OptionalComplex, 4>& param_vals)
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
					else if (pattern::in_domain(*param_vals[1], pattern::Domain::natural)) {
						const std::size_t expo = param_vals[1]->real();
						return nat_pow(*param_vals[0], expo);
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
						const OptionalComplex num = (param_vals[1]->imag() == 0.0) ?
							std::log(param_vals[1]->real()) :
							std::log(*param_vals[1]);
						const OptionalComplex denom = (param_vals[0]->imag() == 0.0) ?
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
				for (const MathIdx elem : fn::unsave_range(ref)) {
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
			case MathType(Literal::symbol): 
				CharVector::free(*ref.store, ref.index);
				break;
			case MathType(Literal::complex):
				ref.store->free_one(ref.index);
				break;
			case MathType(LambdaParam{}):
				break; //no node to free here
			}
		} //free

		//returns true if lambda could be only partially evaluated
		bool replace_lambda_params(const MutRef ref, const StupidBufferVector<MathIdx, 16>& lambda_params, BitVector& used_lambda_params) {
			switch (ref.type) {
			default: {
				assert(ref.type.is<Function>());
				bool curry = false;
				auto range = fn::range(ref);
				const auto stop = end(range);
				for (auto iter = begin(range); iter != stop; ++iter) {
					if (iter->get_type().is<LambdaParam>()) {
						const std::uint32_t index = iter->get_index();
						if (index >= lambda_params.size()) {
							*iter = MathIdx(index - lambda_params.size(), LambdaParam{});
							curry = true;
						}
						else if (used_lambda_params.test(index)) {
							const MathIdx inserted_param = tree::copy(ref.new_at(lambda_params[index]), *ref.store);
							*iter = inserted_param;
						}
						else {
							*iter = lambda_params[index];
							used_lambda_params.set(index);
						}
					}
					else {
						curry |= replace_lambda_params(ref.new_at(*iter), lambda_params, used_lambda_params);
					}
				}
				return curry;
			} break;
			case MathType(LambdaParam{}): //handled in function directly
				assert(false);
				return false;
			case MathType(Literal::symbol):
			case MathType(Literal::complex):
			case MathType(Fn::lambda):
				return false; //nothing to replace
			}
		} //replace_lambda_params

		MathIdx combine(const MutRef ref, const bool exact, const bool in_lambda)
		{
			const auto compute = [exact](auto operate) -> OptionalComplex {
				if (exact) {
					std::feclearexcept(FE_ALL_EXCEPT);
				}
				const OptionalComplex result = operate();
				return (!exact || !std::fetestexcept(FE_ALL_EXCEPT)) ? result : OptionalComplex();
			};

			switch (ref.type) {
			case MathType(Comm::sum): {
				OptionalComplex value_acc = 0.0; //stores sum of values encountered as summands
				StupidBufferVector<MathIdx, 16> new_sum;

				for (MathIdx& summand : fn::unsave_range(ref)) {
					summand = tree::combine(ref.new_at(summand), exact, in_lambda);
					switch (summand.get_type()) {
					case MathType(Comm::sum):
						for (const MathIdx nested_summand : fn::unsave_range(ref.new_at(summand))) {
							new_sum.push_back(nested_summand);
						}						
						IndexVector::free(*ref.store, summand.get_index()); //free nested sum, but not nested summands
						break;
					case MathType(Literal::complex):
						if (const OptionalComplex res = compute([&] { return value_acc + ref.store->at(summand.get_index()).complex; })) {
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
					return MathIdx(IndexVector::build(*ref.store, new_sum), MathType(Comm::sum));
				}
			} break;
			case MathType(Comm::product): {
				//unlike with summands, where an inversion is just flipping the sign bit, not every multiplicativly inverse of a floating point number 
				//  can be stored as floating point number without rounding -> we need two value accumulators. sigh.
				OptionalComplex factor_acc = 1.0;
				OptionalComplex divisor_acc = 1.0; 
				StupidBufferVector<MathIdx, 16> new_product;

				for (MathIdx& factor : fn::unsave_range(ref)) {
					factor = tree::combine(ref.new_at(factor), exact, in_lambda);
					switch (factor.get_type()) {
					case MathType(Fn::pow): {
						IndexVector& power = *ref.new_at(factor);
						if (power[0].get_type() == Literal::complex && power[1].get_type() == Literal::complex) {
							std::array<OptionalComplex, 4> power_params = {
								ref.store->at(power[0].get_index()).complex,
								ref.store->at(power[1].get_index()).complex,
							};
							if (power_params[1]->imag() == 0.0 && power_params[1]->real() < 0.0) {
								power_params[1] *= -1.0;
								if (const OptionalComplex res = compute([&] { return divisor_acc * fn::eval(Fn::pow, power_params); })) {
									divisor_acc = res;
									tree::free(ref.new_at(factor)); //free whole power
									break;
								}
							}
						}
						new_product.push_back(factor);
					} break;
					case MathType(Comm::product):
						for (const MathIdx nested_factor : fn::unsave_range(ref.new_at(factor))) {
							new_product.push_back(nested_factor);
						}
						IndexVector::free(*ref.store, factor.get_index()); //free nested product, but not nested factors
						break;
					case MathType(Literal::complex):
						if (const OptionalComplex res = compute([&] { return factor_acc * ref.store->at(factor.get_index()).complex; })) {
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
				if (const OptionalComplex result_val = compute([&] { return factor_acc / divisor_acc; }); 
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
						new_product.push_back(build_inverted(*ref.store, MathIdx(divisor_idx, Literal::complex)));
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
					return MathIdx(IndexVector::build(*ref.store, new_product), Comm::product);
				}
			} break;
			case MathType(Fn::force): {
				MathIdx& param = ref->parameters[0];
				param = tree::combine(ref.new_at(param), false, in_lambda);
				if (param.get_type() == Literal::complex) {
					const MathIdx result_val = param;
					ref.store->free_one(ref.index);
					return result_val;
				}
			} break;
			case MathType(NonComm::call): {
				if (ref->parameters.size()) [[likely]] {
					const MathIdx lambda_idx = tree::combine(ref.new_at(ref->parameters.front()), exact, false);
					const MutRef lambda = ref.new_at(lambda_idx);
					IndexVector& call_params = *ref; //create this reference only after combining lambda as tree::combine could cause store to reallocate
					if (lambda.type == Fn::lambda) {
						//beta conversion: evaluate lambda
						//call(lambda(expr), params...) -> expr[$... <- params...]
						const MathIdx result = [&]() {
							StupidBufferVector<MathIdx, 16> lambda_params;
							{
								auto call_iter = call_params.begin();
								for (++call_iter; call_iter != call_params.end(); ++call_iter) {
									lambda_params.push_back(*call_iter);
								}
							}
							IndexVector::free(*ref.store, ref.index); //free call, but not lambda nor parameters
							BitVector used_lambda_params(lambda_params.size());

							const MathIdx evaluated_call = [&lambda, &lambda_params, &used_lambda_params, &in_lambda]() {
								MathIdx& definition = lambda->parameters[0];
								if (definition.get_type().is<LambdaParam>()) {
									const std::uint32_t index = definition.get_index();
									if (index >= lambda_params.size() && !in_lambda) {
										definition = MathIdx(index - lambda_params.size(), LambdaParam{});
										return lambda.typed_idx();
									}
									else {
										lambda.store->free_one(lambda.index);
										used_lambda_params.set(index);
										return lambda_params[index];
									}
								}
								else {
									const MathIdx save_definition = definition; //replace_lambda_params might cause store to reallocate (only without garbage collection)
									if (replace_lambda_params(lambda.new_at(save_definition), lambda_params, used_lambda_params) && !in_lambda) 
									{
										return lambda.typed_idx();
									}
									else {
										lambda.store->free_one(lambda.index);
										return save_definition;
									}
								}
							}();
							for (std::size_t i = 0u; i < lambda_params.size(); i++) {
								if (!used_lambda_params.test(i)) {
									tree::free(ref.new_at(lambda_params[i]));
								}
							}
							return evaluated_call;
						}();
						return tree::combine(ref.new_at(result), exact, in_lambda);
					}
					else {
						call_params.front() = lambda_idx;
					}
				}
			} [[fallthrough]];
			default: 
				if (ref.type.is<Fn>()) {
					IndexVector& params = *ref;
					std::array<OptionalComplex, 4> res_vals = { OptionalComplex{}, {}, {}, {} }; //default initialized to NaN
					assert(fn::arity(ref.type) <= 4); //else res_vals size to small
					for (std::size_t i = 0; i < fn::arity(ref.type); i++) {
						params[i] = tree::combine(ref.new_at(params[i]), exact, ref.type == Fn::lambda);
						if (params[i].get_type() == Literal::complex) {
							res_vals[i] = ref.store->at(params[i].get_index()).complex;
						}
					}
					if (const OptionalComplex res = compute([&] { return fn::eval(ref.type.to<Fn>(), res_vals); })) {
						tree::free(ref);
						return build_value(*ref.store, *res);
					}
				} 
				else if (ref.type.is<Variadic>() && fn::is_associative(ref.type)) { //-> flatten nested instances allowed
					StupidBufferVector<MathIdx, 16> new_parameters;
					for (const MathIdx param : fn::unsave_range(ref)) {
						const MathIdx new_param = tree::combine(ref.new_at(param), exact, in_lambda);
						if (new_param.get_type() == ref.type) {
							for (const MathIdx nested_param : fn::unsave_range(ref.new_at(new_param))) {
								new_parameters.push_back(nested_param);
							}
							IndexVector::free(*ref.store, new_param.get_index()); //free nested variadic itself, but not nested params
						}
						else {
							new_parameters.push_back(new_param);
						}
					}
					if (new_parameters.size() == 1u) { //operation on single element is same as that element
						IndexVector::free(*ref.store, ref.index); //free old function, but not old parameters
						return new_parameters.front();
					}
					else if (const auto old_capacity = ref->parameters.capacity(); new_parameters.size() <= old_capacity) [[likely]] {
						IndexVector::emplace(*ref, new_parameters, old_capacity);
					}
					else {
						IndexVector::free(*ref.store, ref.index); //free old variadic itself, but not its params
						return MathIdx(IndexVector::build(*ref.store, new_parameters), ref.type);
					}
				}
				else {
					assert(ref.type.is<Variadic>() || ref.type.is<NamedFn>()); 
					for (MathIdx& param : fn::unsave_range(ref)) {
						param = tree::combine(ref.new_at(param), exact, in_lambda);

					}
				}
				break; 
			case MathType(Literal::symbol):
				break;
			case MathType(Literal::complex):
				break;
			case MathType(LambdaParam{}):
				break;
			}
			return ref.typed_idx();
		} //combine

		bool contains_unwrapped_lambda_parameters(const UnsaveRef ref)
		{
			switch (ref.type) {
			case MathType(Fn::lambda):
				return false;
			case MathType(LambdaParam{}):
				return true;
			default:
				if (ref.type.is<Function>()) {
					for (const MathIdx param : fn::range(ref)) {
						if (contains_unwrapped_lambda_parameters(ref.new_at(param))) {
							return true;
						}
					}
				}
				return false;
			}
		} //contains_unwrapped_lambda_parameters

		

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
			case MathType(NamedFn{}): {
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
			case MathType(Literal::symbol): {
				return compare_char_vecs(*ref_1, *ref_2);
			} break;
			case MathType(Literal::complex): {
				return compare_complex(*ref_1, *ref_2);
			} break;
			case MathType(LambdaParam{}): {
				return ref_1.index <=> ref_2.index;
			} break;
			}
			assert(false); 
			return std::strong_ordering::equal;
		} //compare

		void sort(const MutRef ref)
		{
			const auto sort_variadic = [](const MutRef ref) {
				const auto compare_function = [&](const MathIdx lhs, const MathIdx rhs) {
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

		[[nodiscard]] MathIdx copy(const Ref src_ref, MathStore& dst_store)
		{
			switch (src_ref.type) {			
			default: {
				assert(src_ref.type.is<Function>());
				StupidBufferVector<MathIdx, 12> dst_parameters;
				for (const MathIdx src_param : fn::save_range(src_ref)) {
					const MathIdx dst_param = tree::copy(src_ref.new_at(src_param), dst_store);
					dst_parameters.push_back(dst_param);
				}
				if (src_ref.type.is<NamedFn>()) {
					const CharVector& name_ref = fn::named_fn_name(src_ref);
					std::string name = std::string(name_ref.begin(), name_ref.end());
					return fn::build_named_fn<MathType>(dst_store, std::move(name), dst_parameters);
				}
				else {
					return MathIdx(IndexVector::build(dst_store, dst_parameters), src_ref.type);
				}
			} break;
			case MathType(Literal::symbol): {
				const CharVector& src_var = *src_ref;
				const auto src_name = std::string(src_var.data(), src_var.size());
				const std::size_t dst_index = CharVector::build(dst_store, src_name);
				return MathIdx(dst_index, src_ref.type);
			} break;
			case MathType(Literal::complex): {
				const std::size_t dst_index = dst_store.allocate_one();
				dst_store.at(dst_index) = *src_ref; //bitwise copy of src
				return MathIdx(dst_index, src_ref.type);
			} break;
			case MathType(LambdaParam{}): {
				return src_ref.typed_idx();
			} break;
			}
			assert(false); 
			return MathIdx();
		} //copy


		bool contains(const UnsaveRef ref, const MathIdx to_contain)
		{
			return fold::simple_fold<fold::FindTrue>(ref, 
				[to_contain](const UnsaveRef ref) -> fold::FindTrue
				{ return ref.typed_idx() == to_contain; }
			);
		} //contains

		MathIdx establish_basic_order(const MutRef ref)
		{
			const MathIdx combine_result = tree::combine(ref, true, false);
			tree::sort(ref.new_at(combine_result));
			return combine_result;
		} //establish_basic_order


		bool valid_storage(const MathStore& store, const std::initializer_list<MathIdx> heads)
		{
			BitVector store_positions = store.storage_occupancy(); //every index in store is represented as bit here. false -> currently free	
			const auto reset_and_check_position = [&store_positions](const UnsaveRef ref) -> fold::FindTrue { //doubles in function as 				
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
				else if (ref.type == Literal::symbol) {
					const CharVector& vec = *ref;
					if (set_all_in_range(ref.index, ref.index + vec.node_count())) return true;
				}
				else if (ref.type == LambdaParam{}) {
					return false;
				}
				else {
					if (set_all_in_range(ref.index, ref.index + 1ull)) return true;
				}
				return false; //found no double use (not at this node anyway)
			};

			for (const MathIdx head : heads) {
				if (fold::simple_fold<fold::FindTrue>((UnsaveRef)Ref(store, head), reset_and_check_position)) {
					return false;
				}
			}
			return store_positions.none();
		} //valid_storage

		bool contains_variables(const UnsaveRef ref)
		{
			const auto test_for_variables = [](const UnsaveRef ref) -> fold::FindTrue {
				return ref.type == Literal::symbol;
			};
			return fold::simple_fold<fold::FindTrue>(ref, test_for_variables);
		} //contains_variables

		MathIdx search_variable(const UnsaveRef ref, const std::string_view name)
		{
			const auto test_for_name = [name](UnsaveRef ref) -> fold::Find<MathIdx> {
				return (ref.type == Literal::symbol && std::string_view(ref->characters.data(), ref->characters.size()) == name) ?
					fold::done(MathIdx(ref.index, ref.type)) : //name was found -> cut tree evaluation here
					fold::more(MathIdx());
			};
			return *fold::simple_fold<fold::Find<MathIdx>>(ref, test_for_name);
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
		const auto tokens = tokenize(simple_name);
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

} //namespace bmath