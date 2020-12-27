
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
#include "ioArithmetic.hpp"

/*
	void prototype(const Ref ref)
	{
		switch (ref.type) {
		case Type_T(Variadic::sum): 
			[[fallthrough]];
		case Type_T(Variadic::product): {
			for (const TypedIdx elem : parameters::range(ref)) {
			}
			assert(false);
		} break;
		default: {
			assert(ref.type.is<Fn>());
			for (const TypedIdx param : fn::range(ref)) {
			}
			assert(false);
		} break;
		case Type_T(Leaf::variable): {
			assert(false);
		} break;
		case Type_T(Leaf::complex): {
			assert(false);
		} break;
		case Type_T(PnNode::tree_match): {
			assert(false);
		} break;
		case Type_T(PnNode::value_match): {
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

	//if one pattern may compare equal to a term multiple ways (e.g. sum or product), it has high generality.
	//there are 3 main levels of generality:
	//  - unique (value 1xx): pattern only matches one exact term e.g. pattern "2 'pi'" (note: everything without pattern variables falls tecnically in this category)
	//  - low    (value 2xx): pattern is recursive, but has strong operands order (e.g. all in Fn), thus matches unique on outhermost level, but may hold general operands
	//  - high   (value 3xx): sums / products containing pattern variables can match not only multiple terms, but may also match specific terms in more than one way (also tree variables, duh)
	//the table only differentiates between types, however (as described above) the real generality of a given term may be lower, than that of its outermost node listed here.
	//as the goal of this endavour is (mostly) to sort the most general summands / factors in a pattern to the end, 
	//  the sorting required for efficiently matching patterns may use this table, but has to check more.
	constexpr auto type_generality_table = std::to_array<std::pair<Type, int>>({
		{ Type(Leaf::complex        ), 100 }, 
		{ Type(Leaf::variable       ), 101 },
		{ Type(PnNode::value_match  ), 102 }, //may match different subsets of complex numbers, but always requires an actual value to match against
		{ Type(PnNode::value_proxy  ), 103 }, //dont care really where this sits, as it never ist used in matching anyway
		//values 2xx are not present, as that would require every item in Fn to be listed here (instead look at function generality() below)
		{ Type(NamedFn{}            ), 300 },
		{ Type(Variadic::sum        ), 301 },  
		{ Type(Variadic::product    ), 302 }, 
		{ Type(PnNode::tree_match   ), 303 }, 
		{ Type(MultiPn::summands    ), 304 }, //kinda special, as they always succeed in matching -> need to be matched last 
		{ Type(MultiPn::factors     ), 305 }, //kinda special, as they always succeed in matching -> need to be matched last 
	});
	static_assert(std::is_sorted(type_generality_table.begin(), type_generality_table.end(), [](auto a, auto b) { return a.second < b.second; }));
	static_assert(unsigned(Fn::COUNT) < 99u); //else generality of 300 is used twice

	constexpr int generality(Type type) noexcept 
	{ 
		if (type.is<Fn>()) {
			return 200 + static_cast<unsigned>(type.to<Fn>());
		}
		else {
			return find(type_generality_table, &std::pair<Type, int>::first, type).second; 
		}
	}

	//utility for both Function and NamedFn
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

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////exported in header/////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	namespace tree {

		void free(const MutRef ref)
		{
			switch (ref.type) {
			case Type(NamedFn{}):
				CharVector::free(*ref.store, fn::named_fn_name_index(ref));
				[[fallthrough]];
			default: 
				assert(ref.type.is<Fn>() || ref.type.is<Variadic>());
				for (const TypedIdx elem : fn::range(ref)) {
					tree::free(ref.new_at(elem));
				}
				IndexVector::free(*ref.store, ref.index);
				break;
			case Type(Leaf::variable): 
				CharVector::free(*ref.store, ref.index);
				break;
			case Type(Leaf::complex):
				ref.store->free(ref.index);
				break;
			case Type(PnNode::tree_match):
				ref.store->free(ref.index);
				break;
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
			case Type(Variadic::sum): {
				OptComplex value_acc = 0.0; //stores sum of values encountered as summands
				StupidBufferVector<TypedIdx, 16> new_sum;

				for (TypedIdx& summand : fn::unsave_range(ref)) {
					summand = tree::combine(ref.new_at(summand), exact);
					switch (summand.get_type()) {
					case Type(Variadic::sum):
						for (const TypedIdx nested_summand : fn::unsave_range(ref.new_at(summand))) {
							new_sum.push_back(nested_summand);
						}						
						IndexVector::free(*ref.store, summand.get_index()); //free nested sum, but not nested summands
						break;
					case Type(Leaf::complex):						
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
					return TypedIdx(IndexVector::build(*ref.store, new_sum), Type(Variadic::sum));
				}
			} break;
			case Type(Variadic::product): {
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
									break;
								}
							}
						}
						new_product.push_back(factor);
					} break;
					case Type(Variadic::product):
						for (const TypedIdx nested_factor : fn::unsave_range(ref.new_at(factor))) {
							new_product.push_back(nested_factor);
						}
						IndexVector::free(*ref.store, factor.get_index()); //free nested product, but not nested factors
						break;
					case Type(Leaf::complex):
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
				else {
					if (*factor_acc != 1.0) {
						new_product.push_back(build_value(*ref.store, *factor_acc));
					}
					if (*divisor_acc != 1.0) {
						new_product.push_back(build_value(*ref.store, *divisor_acc));
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
					IndexVector::free(*ref.store, ref.index); //free old sum (but not old summands)
					return TypedIdx(IndexVector::build(*ref.store, new_product), Type(Variadic::product));
				}
			} break;
			case Type(Fn::force): {
				TypedIdx& param = ref->parameters[0];
				param = tree::combine(ref.new_at(param), false);
				if (param.get_type() == Leaf::complex) {
					const TypedIdx result_val = param;
					ref.store->free(ref.index);
					return result_val;
				}
			} break;
			default: 
				if (ref.type.is<Fn>()) {
					IndexVector& params = *ref;
					std::array<OptComplex, 4> res_vals = { OptComplex{}, {}, {}, {} }; //default initialized to NaN
					for (std::size_t i = 0; i < fn::arity(ref.type); i++) {
						params[i] = tree::combine(ref.new_at(params[i]), exact);
						if (params[i].get_type() == Leaf::complex) {
							res_vals[i] = ref.store->at(params[i].get_index()).complex;
						}
					}
					if (const OptComplex res = compute([&] { return fn::eval(ref.type.to<Fn>(), res_vals); })) {
						tree::free(ref);
						return build_value(*ref.store, *res);
					}
					break;
				} 
				else {
					assert(ref.type.is<Variadic>() || ref.type.is<NamedFn>()); 
					for (TypedIdx& param : fn::unsave_range(ref)) {
						param = tree::combine(ref.new_at(param), exact);

					}
					break;
				}
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
			const auto compare_char_vecs = [](const CharVector& vec_1, const CharVector& vec_2) {
				const std::size_t size = std::min(vec_1.size(), vec_2.size());
				if (const std::strong_ordering cmp = compare_arrays(vec_1.data(), vec_2.data(), size); 
					cmp != std::strong_ordering::equal) 
				{
					return cmp;
				}
				return vec_1.size() <=> vec_1.size();
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
				assert(fn::is_function(ref_1.type));
				const IndexVector& vector_1 = *ref_1;
				const IndexVector& vector_2 = *ref_2;
				if (const auto cmp = vector_1.size() <=> vector_2.size(); cmp != std::strong_ordering::equal) {
					return cmp;
				}
				for (auto iter_1 = vector_1.begin(),
				          iter_2 = vector_2.begin();
					iter_1 != vector_1.end(); //both are of equal length
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
			case Type(Leaf::variable): {
				const CharVector& var_1 = *ref_1;
				const CharVector& var_2 = *ref_2;
				return compare_char_vecs(var_1, var_2);
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
			const auto sort_variadic = [](const MutRef ref) {
				const auto compare_function = [&](const TypedIdx lhs, const TypedIdx rhs) {
					return tree::compare(Ref(*ref.store, lhs), Ref(*ref.store, rhs)) == std::strong_ordering::less;
				};

				if (ref.type.is<Variadic>() && fn::is_unordered(ref.type)) {
					IndexVector& operation = *ref;
					std::sort(operation.begin(), operation.end(), compare_function);
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
			default: {
				assert(fn::is_function(src_ref.type));
				StupidBufferVector<TypedIdx, 12> dst_parameters;
				for (const TypedIdx src_param : fn::save_range(src_ref)) {
					const TypedIdx dst_param = tree::copy(src_ref.new_at(src_param), dst_store);
					dst_parameters.push_back(dst_param);
				}
				if (src_ref.type == NamedFn{}) {
					const CharVector& name_ref = fn::named_fn_name(src_ref);
					std::string name = std::string(name_ref.begin(), name_ref.end());
					return fn::build_named_fn(dst_store, std::move(name), dst_parameters);
				}
				else {
					return TypedIdx(IndexVector::build(dst_store, dst_parameters), src_ref.type);
				}
			} break;
			case Type(Leaf::variable): {
				const CharVector& src_var = *src_ref;
				const auto src_name = std::string(src_var.data(), src_var.size());
				const std::size_t dst_index = CharVector::build(dst_store, src_name);
				return TypedIdx(dst_index, src_ref.type);
			} break;
			case Type(Leaf::complex): 
				[[fallthrough]];
			case Type(PnNode::tree_match): {
				const std::size_t dst_index = dst_store.allocate();
				dst_store.at(dst_index) = *src_ref; //bitwise copy of src
				return TypedIdx(dst_index, src_ref.type);
			} break;
			case Type(PnNode::value_match): {
				const pattern::ValueMatchVariable src_var = *src_ref;
				auto dst_var = pattern::ValueMatchVariable(src_var.match_data_idx, src_var.form);
				dst_var.mtch_idx = tree::copy(src_ref.new_at(src_var.mtch_idx), dst_store);
				dst_var.copy_idx = tree::copy(src_ref.new_at(src_var.copy_idx), dst_store);
				const std::size_t dst_index = dst_store.allocate();
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


		bool contains(const Ref ref, const TypedIdx to_contain)
		{
			return fold::simple_fold<fold::FindBool>(ref, 
				[to_contain](const Ref ref) -> fold::FindBool 
				{ return ref.typed_idx() == to_contain; }
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
				default: {
					assert(fn::is_function(type));
					for (TypedIdx& elem : fn::range(MutRef(store, head))) {
						if (TypedIdx* const elem_res = tree::find_subtree_owner(store, elem, subtree)) {
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

		bool valid_storage(const Ref ref)
		{
			BitVector store_positions = ref.store->storage_occupancy(); //every index in store is represented as bit here. false -> currently free	
			const auto reset_and_check_position = [&store_positions](const Ref ref) -> fold::FindBool { //doubles in function as 
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
					if (fn::is_function(ref.type)) {
						const IndexVector& vec = *ref;
						if (set_all_in_range(ref.index, ref.index + vec.node_count())) return true;
						if (ref.type == NamedFn{}) {
							const std::size_t name_index = fn::named_fn_name_index(ref);
							const std::size_t name_node_count = fn::named_fn_name(ref).node_count();
							if (set_all_in_range(name_index, name_index + name_node_count)) return true;
						}
					}					
					else if (ref.type == Leaf::variable) {
						const CharVector& vec = *ref;
						if (set_all_in_range(ref.index, ref.index + vec.node_count())) return true;
					}
					else {
						if (set_all_in_range(ref.index, ref.index + 1ull)) return true;
					}
				}
				return false; //found no double use (not at this node anyway)
			};
			const bool found_double_use = fold::simple_fold<fold::FindBool>(ref, reset_and_check_position);
			return !found_double_use && store_positions.none();
		} //valid_storage

		bool contains_variables(const Ref ref)
		{
			using namespace pattern;
			const auto test_for_variables = [](const Ref ref) -> fold::FindBool {
				return ref.type == Leaf::variable || ref.type.is<MatchType>();
			};
			return fold::simple_fold<fold::FindBool>(ref, test_for_variables);
		} //contains_variables

		TypedIdx search_variable(const Ref ref, const std::string_view name)
		{
			const auto test_for_name = [name](Ref ref) -> fold::Find<TypedIdx> {
				return (ref.type == Leaf::variable && std::string_view(ref->char_vec.data(), ref->char_vec.size()) == name) ?
					fold::done(TypedIdx(ref.index, ref.type)) : //name was found -> cut tree evaluation here
					fold::more(TypedIdx());
			};
			return *fold::simple_fold<fold::Find<TypedIdx>>(ref, test_for_name);
		} //search_variable	

	} //namespace tree


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
				return ref.type.is<Variadic>() || ref.type.is<Fn>();
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
			throw_if(table.tree_table.size() >  match::MatchData::max_tree_match_count, "too many tree_match match variables declared");
			throw_if(table.value_table.size() > match::MatchData::max_value_match_count, "too many value match variables declared");
			throw_if(table.multi_table.size() > match::MatchData::max_multi_match_count, "too many multi match variables declared");
			PatternBuildFunction build_function = { table };
			decltype(PnTerm::lhs_store) lhs_temp; //exists, because actions like rearrange_value_match might produce free slots in store.
			decltype(PnTerm::rhs_store) rhs_temp; //exists, because actions like rearrange_value_match might produce free slots in store.
			this->lhs_head = build_function(lhs_temp, parts.lhs);
			table.build_lhs = false;
			this->rhs_head = build_function(rhs_temp, parts.rhs);

			for (const auto& value : table.value_table) {
				for (const auto lhs_instance : value.lhs_instances) {
					pn_tree::rearrange_value_match(lhs_temp, this->lhs_head, lhs_instance);;
				}
				for (const auto rhs_instance : value.rhs_instances) {
					pn_tree::rearrange_value_match(rhs_temp, this->rhs_head, rhs_instance);
				}
			}

			//sorting and combining is done after rearanging value match to allow constructs 
			//  like "a :real, b | (a+2)+b = ..." to take summands / factors into their value_match match part
			this->lhs_head = tree::combine(MutRef(lhs_temp, this->lhs_head), true);
			this->rhs_head = tree::combine(MutRef(rhs_temp, this->rhs_head), true);

			const auto sort_pattern = [](const MutRef ref) {
				const auto sort_variadic = [&](MutRef ref) {
					const auto compare_patterns = [&](const TypedIdx lhs, const TypedIdx rhs) {
						const Ref lhs_ref = Ref(*ref.store, lhs);
						const Ref rhs_ref = Ref(*ref.store, rhs);					
						{
							const auto contains_general_match_variables = [](const Ref ref) -> bool {
								const auto is_general_match_variable = [](const Ref ref) -> fold::FindBool {
									return ref.type.is<MultiPn>() || ref.type == PnNode::tree_match;
								};
								return fold::simple_fold<fold::FindBool>(ref, is_general_match_variable);
							};
							const bool lhs_contains = contains_general_match_variables(lhs_ref);
							const bool rhs_contains = contains_general_match_variables(rhs_ref);
							if (lhs_contains != rhs_contains) {
								return lhs_contains < rhs_contains;
							}
						}
						{
							const auto highest_literal_depth = [](const Ref ref) -> int {
								struct Accumulator
								{
									int min_operand_depth = std::numeric_limits<int>::max();
									Type ref_type;
									constexpr Accumulator(const Ref ref) noexcept :ref_type(ref.type) {}
									void consume(const int child_depth) noexcept { this->min_operand_depth = std::min(this->min_operand_depth, child_depth); }

									auto result() noexcept 
									{ 
										return this->ref_type == PnNode::value_match ? //literals held by value match dont count >:|
											std::numeric_limits<int>::max() :
											std::max(this->min_operand_depth, this->min_operand_depth + 1);  //std::max because +1 might cause overflow
									}
								};
								const auto leaf_apply = [](const Ref ref) {
									return ref.type.is<Leaf>() ? 0 : std::numeric_limits<int>::max();
								};
								return fold::tree_fold<int, Accumulator>(ref, leaf_apply);
							};
							const int lhs_depth = highest_literal_depth(lhs_ref);
							const int rhs_depth = highest_literal_depth(rhs_ref);
							if (lhs_depth != rhs_depth) {
								return lhs_depth < rhs_depth;
							}
						}
						return tree::compare(lhs_ref, rhs_ref) == std::strong_ordering::less;
					};

					if (ref.type.is<Variadic>() && fn::is_unordered(ref.type)) {
						IndexVector& operation = *ref;
						std::sort(operation.begin(), operation.end(), compare_patterns);
					}
					return fold::Void{};
				};

				fold::simple_fold<fold::Void>(ref, sort_variadic);
			};
			sort_pattern(MutRef(lhs_temp, this->lhs_head));
			sort_pattern(MutRef(rhs_temp, this->rhs_head));

			for (const auto& multi_match : table.multi_table) {
				throw_if(multi_match.lhs_count > 1u, "pattern only allows single use of each MultiPn in lhs.");
				throw_if(multi_match.rhs_count > 1u, "pattern only allows single use of each MultiPn in rhs.");
			}
			{
				//if MultiPn::params occurs in sum / product, it is replaced by legal and matching MultiPn version.
				const auto test_and_replace_multi_pn = [](const MutRef head, const bool do_testing) -> bool {
					const auto inspect_variadic = [do_testing](const MutRef ref) -> fold::FindBool {
						if (ref.type == Variadic::sum || ref.type == Variadic::product) {
							const Type result_type = ref.type == Variadic::sum ? MultiPn::summands : MultiPn::factors;
							for (TypedIdx& elem : fn::unsave_range(ref)) {
								const Type elem_type = elem.get_type();
								if (elem_type == MultiPn::params) {
									elem = TypedIdx(elem.get_index(), result_type); //params can convert to summands / factors
								}
								else if (do_testing && elem_type.is<MultiPn>() && elem_type != result_type) {
									return true;
								}
							}
						}
						return false;
					};
					return fold::simple_fold<fold::FindBool>(head, inspect_variadic);
				};
				throw_if(test_and_replace_multi_pn(MutRef(lhs_temp, this->lhs_head), true), "wrong MultiPn in sum / product in lhs");
				test_and_replace_multi_pn(MutRef(rhs_temp, this->rhs_head), false);
			}
			{
				const auto contains_illegal_value_match = [](const Ref head) -> bool {
					const auto inspect_variadic = [](const Ref ref) -> fold::FindBool {
						if (ref.type == Variadic::sum || ref.type == Variadic::product) {
							std::size_t nr_value_matches = 0u;
							for (const TypedIdx elem : fn::range(ref)) {
								nr_value_matches += (elem.get_type() == PnNode::value_match);
							}
							return nr_value_matches > 1u;
						}
						return false;
					};
					return fold::simple_fold<fold::FindBool>(head, inspect_variadic);
				};
				throw_if(contains_illegal_value_match(Ref(lhs_temp, this->lhs_head)), "no two value match variables may share the same sum / product in lhs.");
			}
			{
				const auto contains_illegal_multi_match = [](const Ref head) -> bool {
					const auto inspect_variadic = [](const Ref ref) -> fold::FindBool {
						if (ref.type == Variadic::sum || ref.type == Variadic::product) {
							std::size_t nr_multi_matches = 0u;
							for (const TypedIdx elem : fn::range(ref)) {
								nr_multi_matches += elem.get_type().is<MultiPn>();
							}
							return nr_multi_matches > 1u;
						}
						return false;
					};
					return fold::simple_fold<fold::FindBool>(head, inspect_variadic);
				};
				throw_if(contains_illegal_multi_match(Ref(lhs_temp, this->lhs_head)), "no two multi match variables may share the same sum / product in lhs.");
			}	
			{
				const auto contains_to_long_variadic = [](const Ref head) -> bool {
					const auto inspect_variadic = [](const Ref ref) -> fold::FindBool {
						if (ref.type == Variadic::sum || ref.type == Variadic::product) {
							return ref->parameters.size() > match::SharedVariadicDatum::max_pn_variadic_size;
						}
						return false;
					};
					return fold::simple_fold<fold::FindBool>(head, inspect_variadic);
				};
				throw_if(contains_to_long_variadic(Ref(lhs_temp, this->lhs_head)), "a sum / product in lhs contains to many operands.");
			}		

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
							case Type(Variadic::sum):     break;
							case Type(Variadic::product): break;
							case Type(Fn::pow):     break;// for now only allow these Fn to be computed in value
							case Type(Fn::sqrt):    break;// for now only allow these Fn to be computed in value  
							default:
								assert(fn::is_function(ref.type)); 
								[[fallthrough]];
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

				const MatchTraits this_traits = classify_subterm(head);
				if (this_traits.computable && this_traits.has_match) {
					return &head;
				}
				else if (this_traits.has_match) { //this contains match, but also other junk -> just return part with match
					assert(fn::is_function(head.get_type()));
					for (TypedIdx& elem : fn::unsave_range(MutRef(store, head))) {
						if (TypedIdx* const elem_res = find_value_match_subtree(store, elem, value)) {
							return elem_res;
						}
					}
					assert(false); //we have match -> we will find it in operands
				}
				return nullptr;
			} //find_value_match_subtree

			void rearrange_value_match(Store& store, TypedIdx& head, const TypedIdx value_match)
			{
				TypedIdx* const value_match_subtree = find_value_match_subtree(store, head, value_match);
				TypedIdx* const value_match_storage = tree::find_subtree_owner(store, head, value_match);

				if (value_match_storage != value_match_subtree) { //else value owns just itself, no nodes upstream
					using VarRef = BasicNodeRef<TypesUnion, ValueMatchVariable, Const::no>;
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
					case Type(Variadic::sum): 
						[[fallthrough]];
					case Type(Variadic::product):
					{
						StupidBufferVector<TypedIdx, 8> result_buffer = { eq.rhs_head };
						for (const TypedIdx elem : fn::range(MutRef(store, eq.lhs_head))) {
							if (tree::contains(Ref(store, elem), to_isolate)) {
								eq.lhs_head = elem; 
							}
							else {
								const TypedIdx new_rhs_elem = (lhs_type == Variadic::sum ? 
									build_negated (store, elem) : 
									build_inverted(store, elem));
								result_buffer.push_back(new_rhs_elem);
							}
						}
						//all factors (summands) have been shifted to rhs -> delete SLC (but not recursively, elems have new owner!)
						IndexVector::free(store, lhs_index);
						//new eq.rhs_head is product (sum) of old eq.rhs_head divided by (minus) eq.lhs_head factors (summands).
						eq.rhs_head = TypedIdx(IndexVector::build(store, result_buffer), lhs_type);  
					} break;
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
						IndexVector* params = &store.at(lhs_index).parameters;
						if (tree::contains(Ref(store, (*params)[0u]), to_isolate)) { //case <contains var>^<computable>
							if ((*params)[1u].get_type() == Leaf::complex && MutRef(store, (*params)[1u])->complex == 2.0) { //special case <contains var>^2 -> use sqrt, not <...>^0.5
								tree::free(MutRef(store, (*params)[1u]));
								(*params)[1u] = TypedIdx();
								params->size() = 1u;
								eq.lhs_head = (*params)[0u];
								eq.rhs_head = TypedIdx(lhs_index, Type(Fn::sqrt));
							}
							else {
								eq.lhs_head = (*params)[0u];
								(*params)[0u] = eq.rhs_head;	
								const TypedIdx inverse_expo = build_inverted(store, (*params)[1u]);
								params = &store.at(lhs_index).parameters;
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
						IndexVector* params = &store.at(lhs_index).parameters;
						eq.lhs_head = (*params)[0u];
						(*params)[0u] = eq.rhs_head;
						const TypedIdx square = build_value(store, 2.0);
						params = &store.at(lhs_index).parameters;
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
						const IndexVector& params = *ref;
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
				case Type(Variadic::sum): {
					OptComplex result_val = 0.0;
					for (auto& summand : fn::range(ref)) {
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
				case Type(Variadic::product): {
					OptComplex result_factor = 1.0;
					OptComplex result_divisor = 1.0;
					for (auto& factor : fn::range(ref)) {
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
					const IndexVector& params = *ref;
					std::array<OptComplex, 4> res_vals;
					for (std::size_t i = 0; i < fn::arity(ref.type); i++) {
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

	namespace pattern::match {

		bool permutation_equals(const Ref pn_ref, const Ref ref, MatchData& match_data)
		{
			if (pn_ref.type.is<MathType>()) {
				if (pn_ref.type != ref.type) [[likely]] {
					return false;
				}
				else {
					switch (pn_ref.type) {
					case Type(NamedFn{}): {
						const CharVector& name = fn::named_fn_name(ref);
						const CharVector& pn_name = fn::named_fn_name(pn_ref);
						if (std::string_view(name.data(), name.size()) != std::string_view(pn_name.data(), pn_name.size())) {
							return false;
						}
					} [[fallthrough]];				
					default: {
						assert(fn::is_function(pn_ref.type));
						if (pn_ref.type.is<Variadic>() && fn::is_unordered(pn_ref.type)) {
							if (pn_ref->parameters.size() > ref->parameters.size()) {
								return false;
							}
							return find_matching_permutation(pn_ref, ref, match_data, 0u, 0u) == FindPermutationRes::matched_all;
						}
						else if (pn_ref.type.is<Fn>()) {
							const IndexVector& pn_range = fn::range(pn_ref);
							const IndexVector& range = fn::range(ref);
							auto pn_iter = pn_range.begin();
							const auto pn_stop = pn_range.end();
							auto iter = range.begin();
							for (; pn_iter != pn_stop; ++pn_iter, ++iter) { //iter and pn_iter both go over same number of params
								if (!match::permutation_equals(pn_ref.new_at(*pn_iter), ref.new_at(*iter), match_data)) {
									return false;
								}
							}
							return true;
						}
						else {
							assert((pn_ref.type.is<Variadic>() && !fn::is_unordered(pn_ref.type)) || pn_ref.type == NamedFn{});
							const IndexVector& pn_range = fn::range(pn_ref);
							const IndexVector& range = fn::range(ref);
							auto pn_iter = pn_range.begin();
							auto iter = range.begin();
							const auto pn_stop = pn_range.end();
							const auto stop = range.end();
							for (; pn_iter != pn_stop && iter != stop; ++pn_iter, ++iter) {
								if (pn_iter->get_type().is<MultiPn>()) {
									SharedMultiDatum& info = match_data.multi_info(pn_iter->get_index());
									assert(info.match_indices.size() == 0u);
									while (iter != stop) {
										info.match_indices.push_back(*iter);
										++iter;
									}
									return true;
								}
								else if (!match::permutation_equals(pn_ref.new_at(*pn_iter), ref.new_at(*iter), match_data)) {
									return false;
								}
							}
							return true;
						}
					} break;
					case Type(Leaf::variable): {
						const CharVector& var = *ref;
						const CharVector& pn_var = *pn_ref;
						return std::string_view(var.data(), var.size()) == std::string_view(pn_var.data(), pn_var.size());
					} break;
					case Type(Leaf::complex): {
						const Complex& complex = *ref;
						const Complex& pn_complex = *pn_ref;
						return compare_complex(complex, pn_complex) == std::strong_ordering::equal;
					} break;
					}
				}
			}
			else {
				assert(pn_ref.type.is<MatchType>());

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
				case Type(PnNode::value_proxy): //may only be encountered in pn_tree::eval_value_match (as value_match does no permutation_equals call)
					assert(false);
					return false;
				case Type(MultiPn::summands):
					if (ref.type == Variadic::sum) {
						SharedMultiDatum& info = match_data.multi_info(pn_ref.index);
						assert(info.match_indices.size() == 0u);
						for (const TypedIdx elem : fn::range(ref)) {
							info.match_indices.push_back(elem);
						}
						return true;
					}
					return false;
				case Type(MultiPn::factors):
					if (ref.type == Variadic::product) {
						SharedMultiDatum& info = match_data.multi_info(pn_ref.index);
						assert(info.match_indices.size() == 0u);
						for (const TypedIdx elem : fn::range(ref)) {
							info.match_indices.push_back(elem);
						}
						return true;
					}
					return false;
				case Type(MultiPn::params): //assumed to be handeled only as param of named_fn or ordered elements in Variadic 
					[[fallthrough]];
				default:
					assert(false);
					return false;
				}
			}

		} //permutation_equals

		void reset_own_matches(const Ref pn_ref, MatchData& match_data) 
		{
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
		} //reset_own_matches

		bool subsequent_permutation_equals(const Ref pn_ref, const Ref ref, MatchData& match_data)
		{
			if (!fn::is_function(pn_ref.type)) {
				return false; //can not rematch at all
			}
			if (pn_ref.type.is<Variadic>() && fn::is_unordered(pn_ref.type)) {
				SharedVariadicDatum& variadic_datum = match_data.variadic_data.at(pn_ref.index);
				const IndexVector& pn_params = *pn_ref;
				assert(variadic_datum.currenty_matched.count() > 0u); //assert this parameters are already matched
				std::uint32_t pn_i = pn_params.size() - 1u;
				if (pn_params[pn_i].get_type().is<MultiPn>()) {
					pn_i--;
				} 
				assert(!pn_params[pn_i].get_type().is<MultiPn>()); //there may only be a single one in each variadic
				const std::uint32_t last_haystack_k = variadic_datum.match_positions[pn_i];
				variadic_datum.currenty_matched.reset(last_haystack_k);
				reset_own_matches(pn_ref, match_data);
				return find_matching_permutation(pn_ref, ref, match_data, pn_i, last_haystack_k + 1u) == FindPermutationRes::matched_all;
			}
			else {
				const auto& pn_range = fn::range(pn_ref);
				const auto& range = fn::range(ref);
				const auto pn_stop = pn_range.end();
				auto pn_iter = pn_range.begin();
				auto iter = range.begin();
				for (; pn_iter != pn_stop; ++pn_iter, ++iter) { //iter and pn_iter both go over same number of params
					if (match::subsequent_permutation_equals(pn_ref.new_at(*pn_iter), ref.new_at(*iter), match_data)) {
						return true;
					}
				}
				return false;
			}
		} //subsequent_permutation_equals

		FindPermutationRes find_matching_permutation(const Ref pn_ref, const Ref haystack_ref, MatchData& match_data, std::uint32_t pn_i, std::uint32_t haystack_k)
		{
			assert(pn_ref.type == haystack_ref.type && (is_one_of<Variadic::sum, Variadic::product>(haystack_ref.type)));

			const IndexVector& pn_params = *pn_ref;
			const IndexVector& haystack_params = *haystack_ref;

			SharedVariadicDatum& variadic_datum = match_data.variadic_data.at_or_insert(pn_ref.index);
			variadic_datum.currenty_matched.set_to_n_false(haystack_params.size()); 

		match_pn_i:
			while (pn_i < pn_params.size()) {
				if (pn_params[pn_i].get_type().is<MultiPn>()) [[unlikely]] {
					assert(haystack_ref.type == Variadic::sum     && pn_params[pn_i].get_type() == MultiPn::summands || // Variadic::sum     may only contain MultiPn::summands
					       haystack_ref.type == Variadic::product && pn_params[pn_i].get_type() == MultiPn::factors  || // Variadic::product may only contain MultiPn::factors

					       haystack_ref.type != Variadic::sum && 
					       haystack_ref.type != Variadic::product && 
					       pn_params[pn_i].get_type() == MultiPn::params
					);
					assert(pn_i + 1ull == pn_params.size() && "MultiPn is only valid as last element");

					SharedMultiDatum& info = match_data.multi_info(pn_params[pn_i].get_index());
					info.match_indices.clear();
					for (std::size_t k = 0u; k < haystack_params.size(); k++) {
						if (!variadic_datum.currenty_matched.test(k)) {
							info.match_indices.push_back(haystack_params[k]);
						}
					}
					return FindPermutationRes::matched_all;
				}
				else {
					const Ref pn_i_ref = pn_ref.new_at(pn_params[pn_i]);
					for (; haystack_k < haystack_params.size(); haystack_k++) {
						if (variadic_datum.currenty_matched.test(haystack_k)) {
							continue;
						}
						if (match::permutation_equals(pn_i_ref, haystack_ref.new_at(haystack_params[haystack_k]), match_data)) {
							variadic_datum.currenty_matched.set(haystack_k);
							variadic_datum.match_positions[pn_i] = haystack_k;
							haystack_k = 0u;
							pn_i++;
							goto match_pn_i; //next while iteration
						}
						reset_own_matches(pn_i_ref, match_data);
					}
				}
				//got here -> could not match element nr pn_i of pattern with any currently unmatched element in haystack
				if (pn_i == 0u) {
					//matching the first element in pattern failed -> there is no hope
					return FindPermutationRes::failed;
				}
				else {
					//this while loop iteration tried matching some element after the first in pattern (and failed)
					// -> perhaps an element preceding the current one could be matched differently
					// -> try that
					pn_i--;
					const Ref pn_i_ref = pn_ref.new_at(pn_params[pn_i]);
					haystack_k = variadic_datum.match_positions[pn_i];
					//try rematching the last successfully matched element in pattern with same part it matched with previously
					// (but it can not match any way that was already tried, duh)
					if (subsequent_permutation_equals(pn_i_ref, haystack_ref.new_at(haystack_params[haystack_k]), match_data)) {
						//success -> try matching the element not matchable this while iteration with (perhaps) now differenty set match variables
						pn_i++;						
					}
					else {
						//could not rematch last successfully matched element with same element in haystack 
						//  -> try succeeding elements in haystack in next loop iteration
						reset_own_matches(pn_i_ref, match_data);
						variadic_datum.currenty_matched.reset(haystack_k);
						haystack_k++;
					}
				}
			}
			return pn_params.size() == haystack_params.size() ? FindPermutationRes::matched_all : FindPermutationRes::matched_some;
		} //find_matching_permutation

		TypedIdx copy(const Ref pn_ref, const MatchData& match_data, const Store& src_store, Store& dst_store)
		{
			switch (pn_ref.type) {
			default: {
				assert(fn::is_function(pn_ref.type));
				StupidBufferVector<TypedIdx, 12> dst_parameters;
				for (const TypedIdx pn_param : fn::save_range(pn_ref)) {
					const TypedIdx dst_param = match::copy(pn_ref.new_at(pn_param), match_data, src_store, dst_store);
					dst_parameters.push_back(dst_param);
				}
				if (pn_ref.type == NamedFn{}) {
					const CharVector& name_ref = fn::named_fn_name(pn_ref);
					std::string name = std::string(name_ref.begin(), name_ref.end());
					return fn::build_named_fn(dst_store, std::move(name), dst_parameters);
				}
				else {
					return TypedIdx(IndexVector::build(dst_store, dst_parameters), pn_ref.type);
				}
			} break;
			case Type(Leaf::variable): {
				const CharVector& src_var = *pn_ref;
				const auto src_name = std::string(src_var.data(), src_var.size());
				const std::size_t dst_index = CharVector::build(dst_store, src_name);
				return TypedIdx(dst_index, pn_ref.type);
			} break;
			case Type(Leaf::complex): {
				const std::size_t dst_index = dst_store.allocate();
				dst_store.at(dst_index) = *pn_ref; //bitwise copy
				return TypedIdx(dst_index, pn_ref.type);
			} break;
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
				const std::size_t dst_index = dst_store.allocate();
				dst_store.at(dst_index) = val;
				return TypedIdx(dst_index, Type(Leaf::complex));
			} break;
			case Type(MultiPn::summands):
				[[fallthrough]];
			case Type(MultiPn::factors): {
				const SharedMultiDatum& info = match_data.multi_info(pn_ref.index);
				const std::uint32_t capacity = IndexVector::smallest_fit_capacity(info.match_indices.size());
				const std::uint32_t nodes_count = IndexVector::_node_count(capacity);

				const std::uint32_t res_idx = dst_store.allocate_n(nodes_count);
				dst_store.at(res_idx) = IndexVector(info.match_indices.size(), capacity);
				auto dst_iter = begin(MutRef(dst_store, res_idx).cast<IndexVector>());

				for (const TypedIdx elem : info.match_indices) {
					const TypedIdx dst_elem = tree::copy(Ref(src_store, elem), dst_store); //call to different copy!
					*dst_iter = dst_elem;
					++dst_iter;
				}
				return TypedIdx(res_idx, pn_ref.type == MultiPn::summands ? Type(Variadic::sum) : Type(Variadic::product));			
			} break;
			case Type(MultiPn::params):  //already handeled in named_fn
				assert(false);
				return TypedIdx();
			}
		} //copy

		std::optional<TypedIdx> match_and_replace(const Ref from, const Ref to, const MutRef ref)
		{		
			if (from.type.is<Variadic>() && fn::is_unordered(from.type) && (from.type == ref.type)) {
				MatchData match_data;
				if (match::find_matching_permutation(from, ref, match_data, 0u, 0u) != FindPermutationRes::failed) {
					Store copy_buffer;
					copy_buffer.reserve(32u);
					const TypedIdx buffer_head = match::copy(to, match_data, *ref.store, copy_buffer);
					StupidBufferVector<TypedIdx, 12> result_variadic;
					{
						const BitVector& matched = match_data.variadic_data.at(from.index).currenty_matched;
						const IndexVector& src_params = *ref; //no allocations in ref.store during src_params livetime -> reference allowed
						for (std::uint32_t k = 0u; k < src_params.size(); k++) {
							if (matched.test(k)) {
								tree::free(ref.new_at(src_params[k])); //delete matched parts
							}
							else {
								result_variadic.push_back(src_params[k]); //keep unmatched parts
							}
						}
					}					
					IndexVector::free(*ref.store, ref.index);  //shallow deletion only of initial sum / product itself, not of its operands
					result_variadic.push_back(tree::copy(Ref(copy_buffer, buffer_head), *ref.store));
					const std::uint32_t res_idx = IndexVector::build(*ref.store, result_variadic);
					return { TypedIdx(res_idx, ref.type) };
				}
			}
			else {
				MatchData match_data;
				if (match::permutation_equals(from, ref, match_data)) {
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
			assert(ref.type.is<MathType>());
			if (fn::is_function(ref.type)) {
				auto range = fn::range(ref);
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
			}
			return std::make_pair(match_and_replace(in, out, ref), false);
		} //recursive_match_and_replace

	} //namespace pattern::match

	namespace fold {

		template<typename Res_T, typename Union_T, typename Type_T, Const is_const, typename Apply>
		Res_T simple_fold(const BasicRef<Union_T, Type_T, is_const> ref, Apply apply)
		{
			constexpr bool return_early_possible = ReturnEarlyPossible<Res_T>::value;

			if (fn::is_function(ref.type)) {
				for (const auto elem : fn::range(ref)) {
					const Res_T elem_res = fold::simple_fold<Res_T>(ref.new_at(elem), apply);
					if constexpr (return_early_possible) { if (elem_res.return_early()) { return elem_res; } }
				}
			}
			else if (ref.type == PnNode::value_match) {
				const pattern::ValueMatchVariable var = *ref;
				const Res_T elem_res_1 = fold::simple_fold<Res_T>(ref.new_at(var.mtch_idx), apply);
				if constexpr (return_early_possible) { if (elem_res_1.return_early()) { return elem_res_1; } }
				const Res_T elem_res_2 = fold::simple_fold<Res_T>(ref.new_at(var.copy_idx), apply);
				if constexpr (return_early_possible) { if (elem_res_2.return_early()) { return elem_res_2; } }
			}
			return apply(ref); 
		} //simple_fold

		template<typename Res_T, typename OpAccumulator, typename Union_T, typename Type_T, Const is_const, typename LeafApply, typename... AccInit>
		Res_T tree_fold(const BasicRef<Union_T, Type_T, is_const> ref, LeafApply leaf_apply, const AccInit... init)
		{
			if (fn::is_function(ref.type)) {
				OpAccumulator acc(ref, init...);
				for (const auto elem : fn::range(ref)) {
					acc.consume(fold::tree_fold<Res_T, OpAccumulator>(ref.new_at(elem), leaf_apply, init...));
				}
				return acc.result();
			}
			else if (ref.type == PnNode::value_match) {
				OpAccumulator acc(ref, init...);
				const pattern::ValueMatchVariable var = *ref;
				acc.consume(fold::tree_fold<Res_T, OpAccumulator>(ref.new_at(var.mtch_idx), leaf_apply, init...));
				acc.consume(fold::tree_fold<Res_T, OpAccumulator>(ref.new_at(var.copy_idx), leaf_apply, init...));
				return acc.result();
			}
			else {
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
		const auto [head_match, deeper_match] = pattern::match::recursive_match_and_replace(p.lhs_ref(), p.rhs_ref(), this->mut_ref());
		if (head_match) {
			this->head = *head_match;
		}
		return head_match || deeper_match;
	}

} //namespace bmath