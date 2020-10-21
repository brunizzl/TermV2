
#include <cassert>
#include <sstream>
#include <array>
#include <algorithm>
#include <cstring>
#include <cfenv>
#include <compare>

#include "termUtility.hpp"
#include "arithmeticTerm.hpp"
#include "termColony.hpp"
#include "ioArithmetic.hpp"

/*
	void prototype(const Store & store, const TypedIdx ref)
	{
		using Type_T = TypedIdx_T::Enum_T;
		using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
		constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

		const auto [index, type] = ref.split();
		switch (type) {
		case Type_T(Node::sum): {
			for (const auto summand : vdc::range(store, index)) {
			}
			assert(false);
		} break;
		case Type_T(Node::product): {
			for (const auto factor : vdc::range(store, index)) {
			}
			assert(false);
		} break;
		case Type_T(Node::generic_function): {
			const GenericFunction& generic_function = store.at(index).generic_function;
			for (const auto param : fn::range(store, generic_function)) {
			}
			assert(false);
		} break;
		default: {
			assert(type.is<Fn>()); //if this assert hits, the switch above needs more cases.
			const FnParams& params = store.at(index).fn_params;
			for (const auto param : fn::range(params, type)) {
			}
			assert(false);
		} break;
		case Type_T(Leaf::variable): {
			const Variable& variable = store.at(index).string;
			assert(false);
		} break;
		case Type_T(Leaf::complex): {
			assert(false);
		} break;
		case Type_T(pattern::_tree_match): if constexpr (pattern) {
			assert(false);
		} break;
		case Type_T(pattern::_value_match): if constexpr (pattern) {
			assert(false);
		} break;
		case Type_T(pattern::_value_proxy):
			assert(false);
			break;
		}
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
		{ Type(Node::generic_function)  , 50 }, //order of parameters is given -> most unique
		{ Type(Node::product         )  , 55 }, //order of operands my vary -> second most unique
		{ Type(Node::sum             )  , 60 }, //order of operands my vary -> second most unique
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
		bool meets_restriction(const TypedIdx_T ref, const Restriction restr)
		{
			if (restr == Restr::any) {
				return true;
			}
			else if (restr.is<Type>()) {
				const Type type = ref.get_type();
				return restr == type;
			}
			else if (restr == Restr::function) {
				const Type type = ref.get_type();
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
			tree::combine_layers(this->lhs_store, this->lhs_head);
			tree::combine_layers(this->rhs_store, this->rhs_head);
			if (const OptComplex lhs_val = tree::combine_values_exact(this->lhs_store, this->lhs_head)) {
				tree::free(this->lhs_store, this->lhs_head);
				this->lhs_head = PnTypedIdx(this->lhs_store.insert(*lhs_val), Type(Leaf::complex));
			}
			if (const OptComplex rhs_val = tree::combine_values_exact(this->rhs_store, this->rhs_head)) {
				tree::free(this->rhs_store, this->rhs_head);
				this->rhs_head = PnTypedIdx(this->rhs_store.insert(*rhs_val), Type(Leaf::complex));
			}
			tree::sort(this->lhs_store, this->lhs_head);
			tree::sort(this->rhs_store, this->rhs_head);
		}

		std::string PnTerm::to_string() const
		{
			std::string str;
			bmath::intern::print::append_to_string(this->lhs_store, this->lhs_head, str);
			str.append(" = ");
			bmath::intern::print::append_to_string(this->rhs_store, this->rhs_head, str);
			return str;
		}

		std::string PnTerm::lhs_memory_layout() const
		{
			return print::to_memory_layout(this->lhs_store, this->lhs_head);
		}

		std::string PnTerm::rhs_memory_layout() const
		{
			return print::to_memory_layout(this->rhs_store, this->rhs_head);
		}
		

	} //namespace pattern

	namespace fn {

		template<typename Store_T1, typename Store_T2>
		std::strong_ordering compare_name(const Store_T1& store_1, const Store_T2& store_2, const GenericFunction& func_1, const GenericFunction& func_2)
		{
			if (func_1.name_size == GenericFunction::NameSize::small &&
				func_2.name_size == GenericFunction::NameSize::small) 
			{//change to comparison of std::string_view when they support <=>
				return compare_arrays(func_1.short_name, func_2.short_name, GenericFunction::short_name_max);				
			}
			else if (func_1.name_size == GenericFunction::NameSize::longer &&
			         func_2.name_size == GenericFunction::NameSize::longer) 			
			{
				return string_compare(store_1, store_2, func_1.long_name_idx, func_2.long_name_idx);
			}
			else {
				return func_1.name_size == GenericFunction::NameSize::small ?
					std::strong_ordering::less : 
					std::strong_ordering::greater;
			}
		}

	} //namespace fn

	namespace tree {

		template<typename Store_T, typename TypedIdx_T>
		void free(Store_T& store, const TypedIdx_T ref)
		{
			using Type_T = TypedIdx_T::Enum_T;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			const auto [index, type] = ref.split();
			switch (type) {
			case Type_T(Node::sum): 
				[[fallthrough]];
			case Type_T(Node::product): {
				for (const auto factor : vdc::range(store, index)) {
					tree::free(store, factor);
				}
				TypedIdxSLC_T::free_slc(store, index);
			} break;
			case Type_T(Node::generic_function): {
				const GenericFunction& generic_function = store.at(index).generic_function;
				for (const auto param : fn::range(store, generic_function)) {
					tree::free(store, param);
				}
				TypedIdxSLC_T::free_slc(store, generic_function.params_idx);
				if (generic_function.name_size == GenericFunction::NameSize::longer) {
					TermString128::free_slc(store, generic_function.long_name_idx);
				}
				store.free(index);
			} break;
			default: {
				assert(type.is<Fn>()); //if this assert hits, the switch above needs more cases.
				const FnParams<TypedIdx_T>& params = store.at(index).fn_params;
				for (const auto param : fn::range(params, type)) {
					tree::free(store, param);
				}
				store.free(index);
			} break;
			case Type_T(Leaf::variable): {
				TermString128::free_slc(store, index);
			} break;
			case Type_T(Leaf::complex): {
				store.free(index);
			} break;
			case Type_T(pattern::_tree_match): if constexpr (pattern) {
				store.free(index);
			} break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				pattern::ValueMatchVariable& var = store.at(index).value_match;
				tree::free(store, var.match_idx);
				tree::free(store, var.copy_idx);
				store.free(index);
			} break;
			case Type_T(pattern::_value_proxy):
				break;
			}
		} //free

		template<typename Store_T, typename TypedIdx_T>
		void combine_layers(Store_T& store, const TypedIdx_T ref)
		{
			using Type_T = TypedIdx_T::Enum_T;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			const auto [index, type] = ref.split();
			switch (type) {
			case Type_T(Node::sum):
				[[fallthrough]];
			case Type_T(Node::product): {
				std::size_t current_append_node = index;
				for (auto& elem : vdc::range(store, index)) {
					const auto [elem_idx, elem_type] = elem.split();
					if (elem_type == type) {
						elem = TypedIdxSLC_T::null_value;
						current_append_node = TypedIdxSLC_T::append(store, current_append_node, elem_idx);
					}
					else {
						tree::combine_layers(store, elem);
					}
				}
			} break;
			case Type_T(Node::generic_function): {
				const GenericFunction& generic_function = store.at(index).generic_function;
				for (const auto param : fn::range(store, generic_function)) {
					tree::combine_layers(store, param);
				}
			} break;
			default: {
				assert(type.is<Fn>()); //if this assert hits, the switch above needs more cases.
				const FnParams<TypedIdx_T>& params = store.at(index).fn_params;
				for (const auto param : fn::range(params, type)) {
					tree::combine_layers(store, param);
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
				tree::combine_layers(store, var.match_idx);
				tree::combine_layers(store, var.copy_idx);
			} break;
			case Type_T(pattern::_value_proxy):
				break;
			}
		} //combine_layers

		template<typename Store_T, typename TypedIdx_T>
		OptComplex combine_values_inexact(Store_T& store, const TypedIdx_T ref)
		{
			using Type_T = TypedIdx_T::Enum_T;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			const auto [index, type] = ref.split();
			switch (type) {
			case Type_T(Node::sum): {
				OptComplex result_val = 0.0;
				bool only_values = true;
				for (auto& summand : vdc::range(store, index)) {
					if (const OptComplex summand_val = tree::combine_values_inexact(store, summand)) {
						result_val += summand_val;
						tree::free(store, summand);
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
					const auto new_summand = TypedIdx_T(store.insert(*result_val), Type(Leaf::complex));
					TypedIdxSLC_T::insert_new(store, index, new_summand);
				}
			} break;
			case Type_T(Node::product): {
				OptComplex result_val = 1.0;
				bool only_values = true;
				for (auto& factor : vdc::range(store, index)) {
					if (const OptComplex factor_val = tree::combine_values_inexact(store, factor)) {
						result_val *= factor_val;
						tree::free(store, factor);
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
					const auto new_factor = TypedIdx_T(store.insert(*result_val), Type(Leaf::complex));
					TypedIdxSLC_T::insert_new(store, index, new_factor);
				}
			} break;
			case Type_T(Node::generic_function): {
				GenericFunction& function = store.at(index).generic_function;
				for (auto& elem : fn::range(store, function)) {
					if (const OptComplex param_res = tree::combine_values_inexact(store, elem)) {
						tree::free(store, elem);
						elem = TypedIdx_T(store.insert(*param_res), Type(Leaf::complex));
					}
				}
			} break;
			default: {
				assert(type.is<Fn>()); //if this assert hits, the switch above needs more cases.
				FnParams<TypedIdx_T>& params = store.at(index).fn_params;
				std::array<OptComplex, 4> results_values;
				bool all_computable = true;
				for (std::size_t i = 0; i < fn::param_count(type); i++) {
					results_values[i] = tree::combine_values_inexact(store, params[i]);
					all_computable &= (bool) results_values[i];
				}
				if (all_computable) {
					return fn::eval(type.to<Fn>(), results_values);
				}
				for (std::size_t i = 0; i < fn::param_count(type); i++) {
					if (results_values[i]) {
						tree::free(store, params[i]);
						params[i] = TypedIdx_T(store.insert(*results_values[i]), Type(Leaf::complex));
					}
				}
			} break; 
			case Type_T(Leaf::variable): 
				break;
			case Type_T(Leaf::complex): 
				return store.at(index).complex;
			case Type_T(pattern::_tree_match): 
				break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				pattern::ValueMatchVariable& var = store.at(index).value_match;
				const Complex match_res = tree::combine_values_inexact(store, var.match_idx);
				const Complex copy_res = tree::combine_values_inexact(store, var.copy_idx);
				assert(!is_valid(match_res)); //pattern variable can not decay to value
				assert(!is_valid(copy_res));  //pattern variable can not decay to value
			} break; 
			case Type_T(pattern::_value_proxy):
				break;
			}
			return {};
		} //combine_values_inexact

		template<typename Store_T, typename TypedIdx_T>
		OptComplex combine_values_exact(Store_T& store, const TypedIdx_T ref)
		{
			using Type_T = TypedIdx_T::Enum_T;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3u>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			const auto get_divisor = [&store](const TypedIdx_T ref) -> std::optional<TypedIdx_T> {
				if (ref.get_type() == Fn::pow) {
					const FnParams<TypedIdx_T>& params = store.at(ref.get_index()).fn_params;
					if (params[1].get_type() == Leaf::complex) {
						const Complex& value = store.at(params[1].get_index()).complex;
						if (value == -1.0) {
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

			const auto [index, type] = ref.split();
			switch (type) {
			case Type_T(Node::sum): {
				OptComplex result_val = 0.0;
				bool only_exact = true;
				for (auto& summand : vdc::range(store, index)) {
					if (const OptComplex summand_val = tree::combine_values_exact(store, summand)) {
						if (const OptComplex res = compute_exact([&] {return result_val + summand_val; })) {
							result_val = res;
							tree::free(store, summand);
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
					const auto new_summand = TypedIdx_T(store.insert(*result_val), Type(Leaf::complex));
					TypedIdxSLC_T::insert_new(store, index, new_summand);
				}
			} break;
			case Type_T(Node::product): {
				OptComplex result_factor = 1.0;
				OptComplex result_divisor = 1.0;
				bool only_exact = true;
				for (auto& factor : vdc::range(store, index)) {
					if (const std::optional<TypedIdx_T> divisor = get_divisor(factor)) {
						if (const OptComplex divisor_val = tree::combine_values_exact(store, *divisor)) {
							if (const OptComplex res = compute_exact([&] { return result_divisor * divisor_val; })) {
								result_divisor = res;
								tree::free(store, factor); //free whole factor including pow() and -1
								factor = TypedIdxSLC_T::null_value;
								continue;
							}
						}
					}
					if (const OptComplex factor_val = tree::combine_values_exact(store, factor)) {
						if (const OptComplex res = compute_exact([&] {return result_factor * factor_val; })) {
							result_factor = res;
							tree::free(store, factor);
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
					const auto new_factor = TypedIdx_T(store.insert(*result_factor), Type(Leaf::complex));
					TypedIdxSLC_T::insert_new(store, index, new_factor);
				}
				else {
					if (*result_factor != 1.0) {
						const auto new_factor = TypedIdx_T(store.insert(*result_factor), Type(Leaf::complex));
						TypedIdxSLC_T::insert_new(store, index, new_factor);
					}
					if (*result_divisor != 1.0) {
						const auto new_divisor = TypedIdx_T(store.insert(*result_divisor), Type(Leaf::complex));
						const auto new_pow = build_inverted<Store_T, TypedIdx_T>(store, new_divisor);
						TypedIdxSLC_T::insert_new(store, index, new_pow);
					}
				}
			} break;
			case Type_T(Node::generic_function): {
				GenericFunction& function = store.at(index).generic_function;
				for (auto& elem : fn::range(store, function)) {
					const OptComplex param_res = tree::combine_values_exact(store, elem);
					if (param_res) {
						tree::free(store, elem);
						elem = TypedIdx_T(store.insert(*param_res), Type(Leaf::complex));
					}
				}
			} break;
			default: {
				assert(type.is<Fn>()); //if this assert hits, the switch above needs more cases.
				FnParams<TypedIdx_T>& params = store.at(index).fn_params;
				std::array<OptComplex, 4> res_vals;
				bool only_exact = true;
				for (std::size_t i = 0; i < fn::param_count(type); i++) {
					res_vals[i] = tree::combine_values_exact(store, params[i]);
					only_exact &= (bool) res_vals[i];
				}
				if (only_exact) {
					if (const auto res = compute_exact([&] { return fn::eval(type.to<Fn>(), res_vals); })) {
						return res;
					}
				}
				for (std::size_t i = 0; i < fn::param_count(type); i++) {
					if (res_vals[i]) {
						tree::free(store, params[i]);
						params[i] = TypedIdx_T(store.insert(*res_vals[i]), Type(Leaf::complex));
					}
				}
			} break;
			case Type_T(Leaf::variable): 
				break;
			case Type_T(Leaf::complex): 
				return store.at(index).complex;
			case Type_T(pattern::_tree_match): 
				break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				pattern::ValueMatchVariable& var = store.at(index).value_match;
				const OptComplex match_res = tree::combine_values_exact(store, var.match_idx);
				const OptComplex copy_res = tree::combine_values_exact(store, var.copy_idx);
				assert(!match_res); //pattern variable can not decay to value
				assert(!copy_res);  //pattern variable can not decay to value
			} break;
			case Type_T(pattern::_value_proxy):
				break;
			}
			return {};
		} //combine_values_exact

		template<typename Store_T1, typename Store_T2, typename TypedIdx_T1, typename TypedIdx_T2>
		std::strong_ordering compare(const Store_T1& store_1, const Store_T2& store_2, const TypedIdx_T1 ref_1, const TypedIdx_T2 ref_2)
		{
			using Type_T = TypedIdx_T1::Enum_T;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			const auto [index_1, type_1] = ref_1.split();
			const auto [index_2, type_2] = ref_2.split();
			if (type_1 != type_2) [[likely]] {
				static_assert((uniqueness(Type(Node::sum)) <=> uniqueness(Type(Node::sum))) == std::strong_ordering::equal); //dont wanna mix with std::strong_ordering::equivalent
				return uniqueness(type_1) <=> uniqueness(type_2);
			}

			switch (type_1) {
			case Type_T(Node::sum):
				[[fallthrough]];
			case Type_T(Node::product): {
				auto range_1 = vdc::range(store_1, index_1);
				auto range_2 = vdc::range(store_2, index_2);
				auto iter_1 = range_1.begin();
				auto iter_2 = range_2.begin();
				for (; iter_1 != range_1.end() && iter_2 != range_2.end(); ++iter_1, ++iter_2) {
					const auto iter_compare = tree::compare(store_1, store_2, *iter_1, *iter_2);
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
			case Type_T(Node::generic_function): {
				const GenericFunction& fn_1 = store_1.at(index_1).generic_function;
				const GenericFunction& fn_2 = store_2.at(index_2).generic_function;
				const auto name_cmp = fn::compare_name(store_1, store_2, fn_2, fn_1); //reverse order, as to_pretty_string reverses again
				if (name_cmp != std::strong_ordering::equal) {
					return name_cmp;
				}
				auto range_1 = fn::range(store_1, fn_1);
				auto range_2 = fn::range(store_2, fn_2);
				auto iter_1 = range_1.begin();
				auto iter_2 = range_2.begin();
				for (; iter_1 != range_1.end() && iter_2 != range_2.end(); ++iter_1, ++iter_2) {
					const auto iter_compare = tree::compare(store_1, store_2, *iter_1, *iter_2);
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
				assert(type_1.is<Fn>() && type_2.is<Fn>()); //if this assert hits, the switch above needs more cases.
				const FnParams<TypedIdx_T1>& params_1 = store_1.at(index_1).fn_params;
				const FnParams<TypedIdx_T2>& params_2 = store_2.at(index_2).fn_params;
				auto range_1 = fn::range(params_1, type_1);
				auto range_2 = fn::range(params_2, type_2);
				auto iter_1 = range_1.begin();
				auto iter_2 = range_2.begin();
				for (; iter_1 != range_1.end(); ++iter_1, ++iter_2) { //iter_1 and iter_2 both go over same number of params
					const auto iter_compare = tree::compare(store_1, store_2, *iter_1, *iter_2);
					if (iter_compare != std::strong_ordering::equal) [[likely]] {
						return iter_compare;
					}
				}
				return std::strong_ordering::equal;
			} break;
			case Type_T(Leaf::variable): {
				return string_compare(store_2, store_1, index_2, index_1); //reverse order, as to_pretty_string reverses again
			} break;
			case Type_T(Leaf::complex): {
				const Complex& complex_1 = store_1.at(index_1).complex;
				const Complex& complex_2 = store_2.at(index_2).complex;
				static_assert(sizeof(double) * 8 == 64, "bit_cast may cast to something of doubles size.");
				const auto real_1 = std::bit_cast<std::uint64_t>(complex_1.real()); //with not actually comparing the doubles as such, strong ordering is possible
				const auto real_2 = std::bit_cast<std::uint64_t>(complex_2.real());
				const auto imag_1 = std::bit_cast<std::uint64_t>(complex_1.imag());
				const auto imag_2 = std::bit_cast<std::uint64_t>(complex_2.imag());
				if (real_1 != real_2) {
					return real_2 <=> real_1; //reverse order, as to_pretty_string reverses again
				}
				if (imag_1 != imag_2) {
					return imag_2 <=> imag_1; //reverse order, as to_pretty_string reverses again
				}
				return std::strong_ordering::equal;
			} break;
			case Type_T(pattern::_tree_match): if constexpr (pattern) {
				const pattern::TreeMatchVariable& var_1 = store_1.at(index_1).tree_match;
				const pattern::TreeMatchVariable& var_2 = store_2.at(index_2).tree_match;
				if (var_1.restr != var_2.restr) {
					return (var_1.restr <=> var_2.restr);
				}
				if (const auto name_cmp = compare_arrays(var_1.name.data(), var_2.name.data(), 4u); name_cmp != std::strong_ordering::equal) {
					return name_cmp;
				}
				if (var_1.match_data_idx != var_2.match_data_idx) {
					return var_1.match_data_idx <=> var_2.match_data_idx; //reverse to make pretty_string prettier
				}
				return std::strong_ordering::equal;
			} assert(false); return std::strong_ordering::equal;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				const pattern::ValueMatchVariable& var_1 = store_1.at(index_1).value_match;
				const pattern::ValueMatchVariable& var_2 = store_2.at(index_2).value_match;
				if (var_1.form != var_2.form) {
					return var_1.form <=> var_2.form;
				}
				if (var_1.match_data_idx != var_2.match_data_idx) {
					return var_1.match_data_idx <=> var_2.match_data_idx;
				}
				if (const auto cmp = tree::compare(store_1, store_2, var_1.match_idx, var_2.match_idx); cmp != std::strong_ordering::equal) {
					return cmp;
				}
				if (const auto cmp = tree::compare(store_1, store_2, var_1.copy_idx, var_2.copy_idx); cmp != std::strong_ordering::equal) {
					return cmp;
				}
				return std::strong_ordering::equal;
			} assert(false); return std::strong_ordering::equal;
			case Type_T(pattern::_value_proxy):
				return index_1 <=> index_2;
			}
		} //compare
		
		template<typename Store_T, typename TypedIdx_T>
		void sort(Store_T& store, const TypedIdx_T ref)
		{
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3u>;

			const auto sort_variadic = [&store](const std::uint32_t index, const auto type) {
				const auto compare_function = [&store](const TypedIdx_T lhs, const TypedIdx_T rhs) {
					return compare(store, store, lhs, rhs) == std::strong_ordering::less;
				};

				if (type == Node::sum || type == Node::product) {
					TypedIdxSLC_T::sort(store, index, compare_function);						
				}
				return fold::Void{};
			};

			fold::simple_fold<fold::Void>(store, ref, sort_variadic);
		} //sort

		template<typename Store_T, typename TypedIdx_T>
		std::size_t count(Store_T& store, const TypedIdx_T ref)
		{
			return fold::tree_fold<std::size_t>(store, ref,
				[](const auto, const auto, const std::size_t acc, const std::size_t elem_res) { return acc + elem_res;  },
				[](const auto, const auto) { return std::size_t(1u); },
				std::size_t(1u));
		} //count
		template std::size_t count<Store, TypedIdx>(Store& store, const TypedIdx ref);

		template<typename DstTypedIdx_T, typename SrcStore_T, typename DstStore_T, typename SrcTypedIdx_T>
		DstTypedIdx_T copy(const SrcStore_T& src_store, DstStore_T& dst_store, const SrcTypedIdx_T src_ref)
		{
			using SrcType_T = SrcTypedIdx_T::Enum_T;
			using DstTypedIdxSLC_T = TermSLC<std::uint32_t, DstTypedIdx_T, 3>;
			constexpr bool src_pattern = std::is_same_v<SrcType_T, pattern::PnType>;

			const auto [src_index, src_type] = src_ref.split();
			switch (src_type) {
			case SrcType_T(Node::sum): 
				[[fallthrough]];
			case SrcType_T(Node::product): {
				const std::size_t dst_index = dst_store.insert(DstTypedIdxSLC_T());
				std::size_t last_node_idx = dst_index;
				for (const auto src_elem : vdc::range(src_store, src_index)) {
					const DstTypedIdx_T dst_elem = tree::copy<DstTypedIdx_T>(src_store, dst_store, src_elem);
					last_node_idx = DstTypedIdxSLC_T::insert_new(dst_store, last_node_idx, dst_elem);
				}
				return DstTypedIdx_T(dst_index, src_type);
			} break;
			case SrcType_T(Node::generic_function): {
				const GenericFunction src_function = src_store.at(src_index).generic_function; //no reference, as src and dst could be same store -> may reallocate
				GenericFunction dst_function;
				std::size_t last_node_idx = dst_store.insert(DstTypedIdxSLC_T());
				dst_function.params_idx = static_cast<std::uint32_t>(last_node_idx);
				for (const auto src_param : fn::range(src_store, src_function)) {
					const DstTypedIdx_T dst_param = tree::copy<DstTypedIdx_T>(src_store, dst_store, src_param);
					last_node_idx = DstTypedIdxSLC_T::insert_new(dst_store, last_node_idx, dst_param);
				}
				std::string src_name; //in most cases the small string optimisation works, else dont care
				read(src_store, src_index, src_name);
				if (src_name.size() <= GenericFunction::short_name_max) [[likely]] {
					std::copy(src_name.begin(), src_name.end(), dst_function.short_name);
					dst_function.name_size = GenericFunction::NameSize::small;
				}
				else {
					dst_function.long_name_idx = insert_string(dst_store, src_name);
					dst_function.name_size = GenericFunction::NameSize::longer;
				}
				return DstTypedIdx_T(dst_store.insert(dst_function), src_type);
			} break;
			default: {
				assert(src_type.is<Fn>());
				const FnParams<SrcTypedIdx_T> src_params = src_store.at(src_index).fn_params; //no reference, as src and dst could be same store -> may reallocate
				auto dst_params = FnParams<DstTypedIdx_T>();
				for (std::size_t i = 0u; i < fn::param_count(src_type); i++) {
					dst_params[i] = tree::copy<DstTypedIdx_T>(src_store, dst_store, src_params[i]);
				}
				return DstTypedIdx_T(dst_store.insert(dst_params), src_type);
			} break;
			case SrcType_T(Leaf::variable): {
				std::string src_name; //in most cases the small string optimisation works, else dont care
				read(src_store, src_index, src_name);
				const std::size_t dst_index = insert_string(dst_store, src_name);
				return DstTypedIdx_T(dst_index, src_type);
			} break;
			case SrcType_T(Leaf::complex): {
				const std::size_t dst_index = dst_store.insert(src_store.at(src_index));
				return DstTypedIdx_T(dst_index, src_type);
			} break;
			case SrcType_T(pattern::_tree_match): if constexpr (src_pattern) {
				const std::size_t dst_index = dst_store.insert(src_store.at(src_index));
				return DstTypedIdx_T(dst_index, src_type);
			} assert(false); return DstTypedIdx_T();
			case SrcType_T(pattern::_value_match):
				assert(false); return DstTypedIdx_T();
			case SrcType_T(pattern::_value_proxy):
				assert(false); return DstTypedIdx_T();
			}
		} //copy
		template TypedIdx copy<TypedIdx, Store, Store, TypedIdx>(const Store& src_store, Store& dst_store, const TypedIdx src_ref);
		template pattern::PnTypedIdx copy<pattern::PnTypedIdx, pattern::PnStore, pattern::PnStore, pattern::PnTypedIdx>
			(const pattern::PnStore& src_store, pattern::PnStore& dst_store, const pattern::PnTypedIdx src_ref);

		template<typename Store_T, typename TypedIdx_T>
		void stupid_solve_for(Store_T& store, Equation<TypedIdx_T>& equation, const TypedIdx_T to_isolate)
		{

			if (!tree::contains(store, equation.lhs_head, to_isolate)) {
				std::swap(equation.lhs_head, equation.rhs_head);
			}
			assert(tree::contains(store, equation.lhs_head, to_isolate));

			while (equation.lhs_head != to_isolate) {
				using Type_T = TypedIdx_T::Enum_T;
				using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
				constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

				const auto [lhs_index, lhs_type] = equation.lhs_head.split();
				switch (lhs_type) {
				case Type_T(Node::sum): 
					[[fallthrough]];
				case Type_T(Node::product): {
					std::size_t last_node_idx = store.insert(TypedIdxSLC_T{ equation.rhs_head });
					equation.rhs_head = TypedIdx_T(last_node_idx, lhs_type); //new rhs_head is product (sum) of old rhs_head divided by (minus) lhs_head factors (summands).
					for (const TypedIdx_T elem : vdc::range(store, lhs_index)) {
						if (tree::contains(store, elem, to_isolate)) {
							equation.lhs_head = elem; 
						}
						else {
							const TypedIdx_T new_rhs_elem = (lhs_type == Node::sum ? 
								build_negated <Store_T, TypedIdx_T>(store, elem) : 
								build_inverted<Store_T, TypedIdx_T>(store, elem));
							last_node_idx = TypedIdxSLC_T::insert_new(store, last_node_idx, new_rhs_elem);
						}
					}
					TypedIdxSLC_T::free_slc(store, lhs_index); //all factors (summands) have been shifted to rhs -> delete SLC (but not recursively, elems have new owner!)
				} break;
				case Type_T(Node::generic_function): 
					assert(false); break;
				case Type_T(Leaf::variable): 
					assert(false); break;
				case Type_T(Leaf::complex): 
					assert(false); break;
				case Type_T(pattern::_tree_match): 
					assert(false); break;
				case Type_T(pattern::_value_match): 
					assert(false); break;
				case Type_T(pattern::_value_proxy):
					assert(false); break;
				default: {
					assert(lhs_type.is<Fn>());
					FnParams<TypedIdx_T>* params = &store.at(lhs_index).fn_params;
					if (fn::param_count(lhs_type) == 1u) {
						equation.lhs_head = params->operator[](0u);
						params->operator[](0u) = equation.rhs_head;						
						switch (Fn(lhs_type)) {
						case Fn::asinh: equation.rhs_head = TypedIdx_T(lhs_index, Fn::sinh ); break;
						case Fn::acosh: equation.rhs_head = TypedIdx_T(lhs_index, Fn::cosh ); break;
						case Fn::atanh: equation.rhs_head = TypedIdx_T(lhs_index, Fn::tanh ); break;
						case Fn::asin:  equation.rhs_head = TypedIdx_T(lhs_index, Fn::sin  ); break;
						case Fn::acos:  equation.rhs_head = TypedIdx_T(lhs_index, Fn::cos  ); break;
						case Fn::atan:  equation.rhs_head = TypedIdx_T(lhs_index, Fn::tan  ); break;
						case Fn::sinh:  equation.rhs_head = TypedIdx_T(lhs_index, Fn::asinh); break;
						case Fn::cosh:  equation.rhs_head = TypedIdx_T(lhs_index, Fn::acosh); break;
						case Fn::tanh:  equation.rhs_head = TypedIdx_T(lhs_index, Fn::atanh); break;
						case Fn::exp:   equation.rhs_head = TypedIdx_T(lhs_index, Fn::ln   ); break;
						case Fn::ln:    equation.rhs_head = TypedIdx_T(lhs_index, Fn::exp  ); break;
						case Fn::sin:   equation.rhs_head = TypedIdx_T(lhs_index, Fn::asin ); break;
						case Fn::cos:   equation.rhs_head = TypedIdx_T(lhs_index, Fn::acos ); break;
						case Fn::sqrt: {	
							__debugbreak();
							//params->type = Fn::pow;
							//const TypedIdx_T square = build_value<TypedIdx_T>(store, 2.0);
							//params = &store.at(lhs_index).fn_params;
							//params->params[1] = square;	
						} break;
						default: assert(false); //function type is not yet added or not invertable
						}
					}
					else if (fn::param_count(lhs_type) == 2u) {
						__debugbreak();
						//const bool to_isolate_in_0 = tree::contains(store, params->params[0], to_isolate);						
						//if (to_isolate_in_0) { //-> TypedIdx carussell is same deal as with single parameter function
						//	switch (params->type) {
						//	case Fn::pow: {
						//		equation.lhs_head = params->params[0];
						//		params->params[0] = equation.rhs_head;
						//		equation.rhs_head = TypedIdx_T(lhs_index, lhs_type);
						//		const TypedIdx_T inverted = build_inverted(store, params->params[1]);
						//		params = &store.at(lhs_index).fn_params;
						//		params->params[1] = inverted;	
						//	} break;
						//	default: assert(false); //function type is not yet added or not invertable
						//	}
						//}
						//else {
						//	assert(tree::contains(store, params->params[1], to_isolate));
						//	assert(false); //this path still needs to be made
						//}
					}
					else {
						assert(false);
					}
				} break;
				}
			}
		} //stupid_solve_for
		 template void stupid_solve_for<Store, TypedIdx>(Store& store, Equation<TypedIdx>& equation, const TypedIdx to_isolate);


		template<typename Store_T, typename TypedIdx_T>
		bool contains(const Store_T& store, const TypedIdx_T ref, const TypedIdx_T to_contain)
		{
			return fold::simple_fold<fold::Bool>(store, ref, 
				[to_contain](const std::uint32_t index, const auto type) -> fold::Bool { return TypedIdx_T(index, type) == to_contain; });
		} //contains

		bool contains_variables(const Store& store, const TypedIdx ref)
		{
			return fold::simple_fold<fold::Bool>(store, ref, [](const auto, const Type type) -> fold::Bool { return type == Leaf::variable; });
		} //contains_variables

		bool contains_variables(const pattern::PnStore& store, const pattern::PnTypedIdx ref)
		{
			using namespace pattern;
			const auto test_for_variables = [](const auto, const PnType type) -> fold::Bool {
				return type == Leaf::variable || type.is<PnVariable>();
			};
			return fold::simple_fold<fold::Bool>(store, ref, test_for_variables);
		} //contains_variables

		TypedIdx search_variable(const Store& store, const TypedIdx head, std::string_view name)
		{
			using Res = fold::MightCut<TypedIdx>;
			const auto test_for_name = [name, &store](const std::uint32_t index, const Type type) -> Res {
				return (type == Leaf::variable && string_compare(store, index, name) == std::strong_ordering::equal) ?
					fold::done(TypedIdx(index, type)) : //name was found -> cut tree evaluation here
					fold::more(TypedIdx());
			};
			return *fold::simple_fold<Res>(store, head, test_for_name);
		} //search_variable

	} //namespace tree

	namespace fold {

		template<typename Res_T, typename Store_T, typename TypedIdx_T, typename Apply>
		Res_T simple_fold(Store_T& store, const TypedIdx_T ref, Apply apply)
		{
			using Type_T = TypedIdx_T::Enum_T;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;
			constexpr bool may_return_early = MightReturnEarly<Res_T>::value;

			const auto [index, type] = ref.split();

			switch (type) {
			case Type_T(Node::sum): 
				[[fallthrough]];
			case Type_T(Node::product): {
				for (const auto elem : vdc::range(store, index)) {
					const Res_T elem_res = fold::simple_fold<Res_T>(store, elem, apply);
					if constexpr (may_return_early) { if (elem_res.return_early()) { return elem_res; } }
				}
			} break;
			case Type_T(Node::generic_function): {
				const GenericFunction generic_function = store.at(index).generic_function;  //no reference, as apply might mutate store
				for (const auto param : fn::range(store, generic_function)) {
					const Res_T elem_res = fold::simple_fold<Res_T>(store, param, apply);
					if constexpr (may_return_early) { if (elem_res.return_early()) { return elem_res; } }
				}
			} break;
			default: {
				assert(type.is<Fn>());
				const FnParams<TypedIdx_T> params = store.at(index).fn_params; //no reference, as apply might mutate store
				for (const auto param : fn::range(params, type)) {
					const Res_T elem_res = fold::simple_fold<Res_T>(store, param, apply);
					if constexpr (may_return_early) { if (elem_res.return_early()) { return elem_res; } }
				}
			} break;
			case Type_T(Leaf::variable): 
				break;
			case Type_T(Leaf::complex): 
				break;
			case Type_T(pattern::_tree_match): 
				break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				const pattern::ValueMatchVariable var = store.at(index).value_match; //no reference, as apply might mutate store
				const Res_T elem_res_1 = fold::simple_fold<Res_T>(store, var.match_idx, apply);
				if constexpr (may_return_early) { if (elem_res_1.return_early()) { return elem_res_1; } }
				const Res_T elem_res_2 = fold::simple_fold<Res_T>(store, var.copy_idx, apply);
				if constexpr (may_return_early) { if (elem_res_2.return_early()) { return elem_res_2; } }
			} break;
			case Type_T(pattern::_value_proxy):
				break;
			}
			return apply(index, type); 
		} //simple_fold

		template<typename Res_T, typename Store_T, typename TypedIdx_T, typename OpApply, typename LeafApply, typename Finally>
		Res_T tree_fold(Store_T& store, const TypedIdx_T ref, OpApply op_apply, LeafApply leaf_apply, const Res_T init, Finally finally)
		{
			using Type_T = TypedIdx_T::Enum_T;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;
			constexpr bool may_return_early = MightReturnEarly<Res_T>::value;

			const auto [index, type] = ref.split();
			switch (type) {
			case Type_T(Node::sum): 
				[[fallthrough]];
			case Type_T(Node::product): {
				Res_T acc = init;
				for (const auto elem : vdc::range(store, index)) {
					const Res_T elem_res = fold::tree_fold<Res_T>(store, elem, op_apply, leaf_apply, init, finally);
					acc = op_apply(index, type, acc, elem_res);
					if constexpr (may_return_early) { if (acc.return_early()) { return acc; } }
				}
				return finally(acc, ref);
			} 
			case Type_T(Node::generic_function): {
				Res_T acc = init;
				const GenericFunction generic_function = store.at(index).generic_function;  //no reference, as apply might mutate store
				for (const auto param : fn::range(store, generic_function)) {
					const Res_T param_res = fold::tree_fold<Res_T>(store, param, op_apply, leaf_apply, init, finally);
					acc = op_apply(index, type, acc, param_res);
					if constexpr (may_return_early) { if (acc.return_early()) { return acc; } }
				}
				return finally(acc, ref);
			} 
			default: {
				assert(type.is<Fn>());
				Res_T acc = init;
				const FnParams<TypedIdx_T> params = store.at(index).fn_params; //no reference, as apply might mutate store
				for (const auto param : fn::range(params, type)) {
					const Res_T param_res = fold::tree_fold<Res_T>(store, param, op_apply, leaf_apply, init, finally);
					acc = op_apply(index, type, acc, param_res);
					if constexpr (may_return_early) { if (acc.return_early()) { return acc; } }
				}
				return finally(acc, ref);			
			}
			case Type_T(Leaf::variable): 
				[[fallthrough]];
			case Type_T(Leaf::complex): 
				[[fallthrough]];
			case Type_T(pattern::_tree_match): 
				[[fallthrough]];
			case Type_T(pattern::_value_match): //sees value_match as leaf :o
				[[fallthrough]];
			case Type_T(pattern::_value_proxy):
				return leaf_apply(index, type);
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
		tree::combine_layers(this->store, this->head);
	}

	void Term::combine_values_inexact() noexcept
	{
		if (const OptComplex val = tree::combine_values_inexact(this->store, this->head)) {
			tree::free(this->store, this->head);
			this->head = TypedIdx(this->store.insert(*val), Leaf::complex);
		}	
	}

	void Term::combine_values_exact() noexcept
	{
		if (const OptComplex val = tree::combine_values_exact(this->store, this->head)) {
			tree::free(this->store, this->head);
			this->head = TypedIdx(this->store.insert(*val), Leaf::complex);
		}
	}

	void Term::sort() noexcept
	{
		tree::sort(this->store, this->head);
	}

	std::string bmath::Term::to_memory_layout() const
	{
		return print::to_memory_layout(this->store, this->head);
	} //to_memory_layout

	std::string Term::to_string() const
	{
		std::string result;
		result.reserve(this->store.size() * 2);
		print::append_to_string(this->store, this->head, result);
		return result;
	}

	std::string Term::to_pretty_string()
	{
		this->combine_layers();
		this->combine_values_exact();
		this->sort();
		return print::to_pretty_string(this->store, this->head);
	}

	std::string Term::to_pretty_string() const
	{
		return print::to_pretty_string(this->store, this->head);
	}

	std::pair<intern::Store*, intern::TypedIdx> Term::data() noexcept
	{
		return std::make_pair(&this->store, this->head);
	}

} //namespace bmath