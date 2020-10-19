
#include <cassert>
#include <sstream>
#include <array>
#include <algorithm>
#include <cstring>
#include <cfenv>

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
		case Type_T(Type::sum): {
			for (const auto summand : vdc::range(store, index)) {
			}
			assert(false);
		} break;
		case Type_T(Type::product): {
			for (const auto factor : vdc::range(store, index)) {
			}
			assert(false);
		} break;
		case Type_T(Type::known_function): {
			const BasicKnownFunction<TypedIdx_T>& function = store.at(index).known_function;
			for (const auto param : fn::range(function)) {
			}
			assert(false);
		} break;
		case Type_T(Type::generic_function): {
			const GenericFunction& generic_function = store.at(index).generic_function;
			for (const auto param : fn::range(store, generic_function)) {
			}
			assert(false);
		} break;
		case Type_T(Type::variable): {
			const Variable& variable = store.at(index).string;
			assert(false);
		} break;
		case Type_T(Type::complex): {
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
		default: assert(false); //if this assert hits, the switch above needs more cases.
		}
	} //prototype
*/

namespace bmath::intern {

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////local definitions//////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	//more unique (meaning harder to match) is smaller
	constexpr auto uniqueness_table = std::to_array<std::pair<pattern::PnType, int>>({
		{ Type::generic_function          , 00 }, //order of parameters is given -> most unique
		{ Type::known_function            , 10 }, //order of parameters is given -> most unique
		{ Type::product                   , 20 }, //order of operands my vary -> second most unique
		{ Type::sum                       , 30 }, //order of operands my vary -> second most unique
		{ pattern::PnVariable::value_match, 50 }, //a bit more unique than complex
		{ pattern::PnVariable::value_proxy, 60 }, //a bit more unique than complex
		{ Type::variable                  , 40 }, //quite not unique
		{ Type::complex                   , 70 }, //quite not unique
		{ pattern::PnVariable::tree_match , 80 }, //can match anything (in princible) -> least unique
	});
	constexpr int uniqueness(pattern::PnType type) noexcept { return find_snd(uniqueness_table, type); }

	//utility for both KnownFunction and GenericFunction
	namespace fn {

		OptComplex eval(FnType type, const std::array<OptComplex, 3>& params)
		{
			if (param_count(type) == 1u) {
				if (params[0]->imag() == 0.0) {
					const double real_param = params[0]->real();
					switch (type) {
					case FnType::asinh: return std::asinh(real_param);
					case FnType::acosh: return (         real_param  >= 1.0 ? std::acosh(real_param) : std::acosh(*params[0]));
					case FnType::atanh: return (std::abs(real_param) <= 1.0 ? std::atanh(real_param) : std::atanh(*params[0]));
					case FnType::asin : return (std::abs(real_param) <= 1.0 ?  std::asin(real_param) :  std::asin(*params[0]));
					case FnType::acos : return (std::abs(real_param) <= 1.0 ?  std::acos(real_param) :  std::acos(*params[0]));
					case FnType::atan : return std::atan (real_param);
					case FnType::sinh : return std::sinh (real_param);
					case FnType::cosh : return std::cosh (real_param);
					case FnType::tanh : return std::tanh (real_param);
					case FnType::sqrt : return (         real_param  >= 0.0 ?  std::sqrt(real_param) :  std::sqrt(*params[0]));
					case FnType::exp  : return std::exp  (real_param);
					case FnType::sin  : return std::sin  (real_param);
					case FnType::cos  : return std::cos  (real_param);
					case FnType::tan  : return std::tan  (real_param);
					case FnType::abs  : return std::abs  (real_param);
					case FnType::arg  : return std::arg  (real_param);
					case FnType::ln   : return std::log  (real_param);
					case FnType::re   : return real_param;
					case FnType::im   : return 0.0;
					default: assert(false);
					}
				}
				else {
					switch (type) {
					case FnType::asinh: return std::asinh(*params[0]);
					case FnType::acosh: return std::acosh(*params[0]);
					case FnType::atanh: return std::atanh(*params[0]);
					case FnType::asin : return std::asin (*params[0]);
					case FnType::acos : return std::acos (*params[0]);
					case FnType::atan : return std::atan (*params[0]);
					case FnType::sinh : return std::sinh (*params[0]);
					case FnType::cosh : return std::cosh (*params[0]);
					case FnType::tanh : return std::tanh (*params[0]);
					case FnType::sqrt : return std::sqrt (*params[0]);
					case FnType::exp  : return std::exp  (*params[0]);
					case FnType::sin  : return std::sin  (*params[0]);
					case FnType::cos  : return std::cos  (*params[0]);
					case FnType::tan  : return std::tan  (*params[0]);
					case FnType::abs  : return std::abs  (*params[0]);
					case FnType::arg  : return std::arg  (*params[0]);
					case FnType::ln   : return std::log  (*params[0]);
					case FnType::re   : return std::real (*params[0]);
					case FnType::im   : return std::imag (*params[0]);
					default: assert(false);
					}
				}
			}
			else if (param_count(type) == 2u) {
				if (params[0]->imag() == 0.0 && params[1]->imag() == 0.0) {
					const double real_0 = params[0]->real();
					const double real_1 = params[1]->real();
					switch (type) {
					case FnType::pow  : return ((real_1 == -1.0) ? (1.0 / real_0) : std::pow(real_0, real_1));
					case FnType::log  : return std::log(real_1) / std::log(real_0);
					default: assert(false);
					}
				}
				else if (params[0]->imag() == 0.0) {
					const double real_0 = params[0]->real();
					switch (type) {
					case FnType::pow  : return std::pow(real_0, *params[1]);
					case FnType::log  : return std::log(*params[1]) / std::log(real_0); 
					default: assert(false);
					}
				}
				else if (params[1]->imag() == 0.0) {
					const double real_1 = params[1]->real();
					switch (type) {
					case FnType::pow  : return ((real_1 == -1.0) ? (1.0 / *params[0]) : std::pow(*params[0], real_1));
					case FnType::log  : return std::log(real_1) / std::log(*params[0]); 
					default: assert(false);
					}
				}
				else {
					switch (type) {
					case FnType::pow  : return std::pow(*params[0], *params[1]);
					case FnType::log  : return std::log(*params[1]) / std::log(*params[0]); //https://en.wikipedia.org/wiki/Complex_logarithm#Generalizations
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
				return type == Type::known_function || type == Type::generic_function;
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
				this->lhs_head = PnTypedIdx(this->lhs_store.insert(*lhs_val), Type::complex);
			}
			if (const OptComplex rhs_val = tree::combine_values_exact(this->rhs_store, this->rhs_head)) {
				tree::free(this->rhs_store, this->rhs_head);
				this->rhs_head = PnTypedIdx(this->rhs_store.insert(*rhs_val), Type::complex);
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
		
		PnTypedIdx find_value_match_subtree(const PnStore& store, const PnTypedIdx head, const PnTypedIdx value_match)
		{

			enum class What { computable, contains_value_match, no_value_match_and_not_computable, final_result };
			struct Res
			{
				PnTypedIdx result;
				What what;
				constexpr bool return_early() const noexcept { return this->what == What::final_result; }
			};
			static_assert(fold::MightReturnEarly<Res>::value);

			const auto op_apply = [](const std::uint32_t, const PnType, const Res acc, const Res elem_res) -> Res {
				switch (elem_res.what) {
				case What::computable:
					return acc;
				case What::contains_value_match:
					if (acc.what == What::computable) {
						return { elem_res.result,  What::contains_value_match };
					}
					if (acc.what == What::no_value_match_and_not_computable) {
						return { elem_res.result, What::final_result };
					}
					return acc;
				case What::no_value_match_and_not_computable:
					if (acc.what == What::computable) {
						return elem_res;
					}
					if (acc.what == What::contains_value_match) {
						return { acc.result, What::final_result };
					}
					return acc;
				case What::final_result:
					return elem_res;
				default:
					assert(false);
					return acc;
				}
			};

			const auto leaf_apply = [value_match](const std::uint32_t index, const PnType type) -> Res { 
				switch (type) {
				default: assert(false);
					[[fallthrough]];
				case PnType(Type::complex):
					return { PnTypedIdx(), What::computable };
				case PnType(PnVariable::value_match):
					if (const auto ref = PnTypedIdx(index, type); ref == value_match) {
						return { ref, What::contains_value_match };
					}
					[[fallthrough]];
				case PnType(Type::variable):
					[[fallthrough]];
				case PnType(PnVariable::value_proxy):
					[[fallthrough]];
				case PnType(PnVariable::tree_match):
					return { PnTypedIdx(), What::no_value_match_and_not_computable };
				}
			};

			const auto finally = [&store](const Res acc, PnTypedIdx ref) -> Res {
				const auto [index, type] = ref.split();
				switch (type) {
				default: assert(false);
					[[fallthrough]];
				case PnType(Type::sum):
					[[fallthrough]];
				case PnType(Type::product):
					return { ref, acc.what };
				case PnType(Type::known_function): {
					const PnKnownFunction& function = store.at(index).known_function;
					if (is_one_of<FnType::pow, FnType::sqrt>(function.type)) {
						return { ref, acc.what };
					}
				} [[fallthrough]];
				case PnType(Type::generic_function):
					if (acc.what == What::contains_value_match) {
						return { acc.result, What::final_result };
					}
					else {
						return { acc.result, What::no_value_match_and_not_computable };
					}
				}
			};

			return fold::tree_fold<Res>(store, head, op_apply, leaf_apply, { PnTypedIdx(), What::computable }, finally).result;
		} //find_value_match_subtree
		

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
			case Type_T(Type::sum): 
				[[fallthrough]];
			case Type_T(Type::product): {
				for (const auto factor : vdc::range(store, index)) {
					tree::free(store, factor);
				}
				TypedIdxSLC_T::free_slc(store, index);
			} break;
			case Type_T(Type::known_function): {
				const BasicKnownFunction<TypedIdx_T>& known_function = store.at(index).known_function;
				for (const auto param : fn::range(known_function)) {
					tree::free(store, param);
				}
				store.free(index);
			} break;
			case Type_T(Type::generic_function): {
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
			case Type_T(Type::variable): {
				TermString128::free_slc(store, index);
			} break;
			case Type_T(Type::complex): {
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
			default: assert(false); //if this assert hits, the switch above needs more cases.
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
			case Type_T(Type::sum):
				[[fallthrough]];
			case Type_T(Type::product): {
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
			case Type_T(Type::known_function): {
				const BasicKnownFunction<TypedIdx_T>& known_function = store.at(index).known_function;
				for (const auto param : fn::range(known_function)) {
					tree::combine_layers(store, param);
				}
			} break;
			case Type_T(Type::generic_function): {
				const GenericFunction& generic_function = store.at(index).generic_function;
				for (const auto param : fn::range(store, generic_function)) {
					tree::combine_layers(store, param);
				}
			} break;
			case Type_T(Type::variable):
				break;
			case Type_T(Type::complex):
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
			default: assert(false); //if this assert hits, the switch above needs more cases.
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
			case Type_T(Type::sum): {
				OptComplex result_val = 0.0;
				bool only_values = true;
				for (auto& summand : vdc::range(store, index)) {
					if (const OptComplex summand_val = tree::combine_values_inexact(store, summand)) {
						result_val += summand_val;
						summand = TypedIdxSLC_T::null_value;
					}
					else {
						only_values = false;
					}
				}
				if (only_values) {
					TypedIdxSLC_T::free_slc(store, index); //all summands have already (implicitly) been freed in the loop above
					return  result_val;
				}
				else if (result_val != 0.0) {
					const auto new_summand = TypedIdx_T(store.insert(*result_val), Type::complex);
					TypedIdxSLC_T::insert_new(store, index, new_summand);
				}
			} break;
			case Type_T(Type::product): {
				OptComplex result_val = 1.0;
				bool only_values = true;
				for (auto& factor : vdc::range(store, index)) {
					if (const OptComplex factor_val = tree::combine_values_inexact(store, factor)) {
						result_val *= factor_val;
						factor = TypedIdxSLC_T::null_value;
					}
					else {
						only_values = false;
					}
				}
				if (only_values) {
					TypedIdxSLC_T::free_slc(store, index); //all factors have already been freed in the loop above
					return result_val;
				}
				else if (result_val != 1.0) {
					const auto new_factor = TypedIdx_T(store.insert(*result_val), Type::complex);
					TypedIdxSLC_T::insert_new(store, index, new_factor);
				}
			} break;
			case Type_T(Type::known_function): {
				BasicKnownFunction<TypedIdx_T>& function = store.at(index).known_function;
				std::array<OptComplex, 3> results_values;
				bool all_computable = true;
				for (std::size_t i = 0; i < fn::param_count(function.type); i++) {
					results_values[i] = tree::combine_values_inexact(store, function.params[i]);
					all_computable &= (bool) results_values[i];
				}
				if (all_computable) {
					store.free(index);
					return fn::eval(function.type, results_values);
				}
				for (std::size_t i = 0; i < fn::param_count(function.type); i++) {
					if (results_values[i]) {
						function.params[i] = TypedIdx_T(store.insert(*results_values[i]), Type::complex);
					}
				}
			} break;
			case Type_T(Type::generic_function): {
				GenericFunction& function = store.at(index).generic_function;
				for (auto& elem : fn::range(store, function)) {
					if (const OptComplex param_res = tree::combine_values_inexact(store, elem)) {
						elem = TypedIdx_T(store.insert(*param_res), Type::complex);
					}
				}
			} break;
			case Type_T(Type::variable): 
				break;
			case Type_T(Type::complex): {
				const Complex value = store.at(index).complex;
				store.free(index);
				return value;
			} break;
			case Type_T(pattern::_tree_match): 
				break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				pattern::ValueMatchVariable& var = store.at(index).value_match;
				const Complex match_res = tree::combine_values_inexact(store, var.match_idx);
				const Complex copy_res = tree::combine_values_inexact(store, var.copy_idx);
				assert(!is_valid(match_res)); //pattern variable can not decay to value
				assert(!is_valid(copy_res));  //pattern variable can not decay to value
				break;
			} break; 
			case Type_T(pattern::_value_proxy):
				break;
			default: assert(false); //if this assert hits, the switch above needs more cases.
			}
			return {};
		} //combine_values_inexact

		template<typename Store_T, typename TypedIdx_T>
		OptComplex combine_values_exact(Store_T& store, const TypedIdx_T ref)
		{
			using Type_T = TypedIdx_T::Enum_T;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3u>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;
			
			const auto compute_exact = [](auto operate) -> OptComplex {
				std::feclearexcept(FE_ALL_EXCEPT);
				const OptComplex result = operate();
				return (!std::fetestexcept(FE_ALL_EXCEPT)) ? result : OptComplex();
			};

			const auto [index, type] = ref.split();
			switch (type) {
			case Type_T(Type::sum): {
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
				else if (result_val != 0.0) {
					const auto new_summand = TypedIdx_T(store.insert(*result_val), Type::complex);
					TypedIdxSLC_T::insert_new(store, index, new_summand);
				}
			} break;
			case Type_T(Type::product): {
				OptComplex result_val = 1.0;
				bool only_exact = true;
				for (auto& factor : vdc::range(store, index)) {
					if (const OptComplex factor_val = tree::combine_values_exact(store, factor)) {
						if (const OptComplex res = compute_exact([&] {return result_val * factor_val; })) {
							result_val = res;
							tree::free(store, factor);
							factor = TypedIdxSLC_T::null_value;
							continue;
						}
					}
					only_exact = false;
				}
				if (only_exact) {
					return result_val;
				}
				else if (result_val != 1.0) {
					const auto new_summand = TypedIdx_T(store.insert(*result_val), Type::complex);
					TypedIdxSLC_T::insert_new(store, index, new_summand);
				}
			} break;
			case Type_T(Type::known_function): {
				BasicKnownFunction<TypedIdx_T>& function = store.at(index).known_function;
				std::array<OptComplex, 3> res_vals;
				bool only_exact = true;
				for (std::size_t i = 0; i < fn::param_count(function.type); i++) {
					res_vals[i] = tree::combine_values_exact(store, function.params[i]);
					only_exact &= (bool) res_vals[i];
				}
				if (only_exact) {
					if (const auto res = compute_exact([&] { return fn::eval(function.type, res_vals); })) {
						return res;
					}
				}
				for (std::size_t i = 0; i < fn::param_count(function.type); i++) {
					if (res_vals[i]) {
						tree::free(store, function.params[i]);
						function.params[i] = TypedIdx_T(store.insert(*res_vals[i]), Type::complex);
					}
				}
			} break;
			case Type_T(Type::generic_function): {
				GenericFunction& function = store.at(index).generic_function;
				for (auto& elem : fn::range(store, function)) {
					const OptComplex param_res = tree::combine_values_exact(store, elem);
					if (param_res) {
						tree::free(store, elem);
						elem = TypedIdx_T(store.insert(*param_res), Type::complex);
					}
				}
			} break;
			case Type_T(Type::variable): 
				break;
			case Type_T(Type::complex): 
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
			default: assert(false); //if this assert hits, the switch above needs more cases.
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
				static_assert((uniqueness(Type::sum) <=> uniqueness(Type::sum)) == std::strong_ordering::equal); //dont wanna mix with std::strong_ordering::equivalent
				return uniqueness(type_1) <=> uniqueness(type_2);
			}

			switch (type_1) {
			case Type_T(Type::sum):
				[[fallthrough]];
			case Type_T(Type::product): {
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
			case Type_T(Type::known_function): {
				const BasicKnownFunction<TypedIdx_T1>& fn_1 = store_1.at(index_1).known_function;
				const BasicKnownFunction<TypedIdx_T2>& fn_2 = store_2.at(index_2).known_function;
				if (fn_1.type != fn_2.type) {
					return fn_1.type <=> fn_2.type;
				}
				auto range_1 = fn::range(fn_1);
				auto range_2 = fn::range(fn_2);
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
			case Type_T(Type::generic_function): {
				const GenericFunction& fn_1 = store_1.at(index_1).generic_function;
				const GenericFunction& fn_2 = store_2.at(index_2).generic_function;
				const auto name_cmp = fn::compare_name(store_1, store_2, fn_2, fn_1); //reverse order, as to_pretty_string is reversed again
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
			case Type_T(Type::variable): {
				return string_compare(store_2, store_1, index_2, index_1); //reverse order, as to_pretty_string is reversed again
			} break;
			case Type_T(Type::complex): {
				const Complex& complex_1 = store_1.at(index_1).complex;
				const Complex& complex_2 = store_2.at(index_2).complex;
				static_assert(sizeof(double) * 8 == 64, "bit_cast may cast to something of doubles size.");
				const auto real_1 = std::bit_cast<std::uint64_t>(complex_1.real()); //with not actually comparing the doubles as such, strong ordering is possible
				const auto real_2 = std::bit_cast<std::uint64_t>(complex_2.real());
				const auto imag_1 = std::bit_cast<std::uint64_t>(complex_1.imag());
				const auto imag_2 = std::bit_cast<std::uint64_t>(complex_2.imag());
				if (real_1 != real_2) {
					return real_2 <=> real_1; //reverse order, as to_pretty_string is reversed again
				}
				if (imag_1 != imag_2) {
					return imag_2 <=> imag_1; //reverse order, as to_pretty_string is reversed again
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
			default: assert(false); //if this assert hits, the switch above needs more cases.
				return std::strong_ordering::equal;
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
				if (type == Type::sum || type == Type::product) {
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
			case SrcType_T(Type::sum): 
				[[fallthrough]];
			case SrcType_T(Type::product): {
				const std::size_t dst_index = dst_store.insert(DstTypedIdxSLC_T());
				std::size_t last_node_idx = dst_index;
				for (const auto src_elem : vdc::range(src_store, src_index)) {
					const DstTypedIdx_T dst_elem = tree::copy<DstTypedIdx_T>(src_store, dst_store, src_elem);
					last_node_idx = DstTypedIdxSLC_T::insert_new(dst_store, last_node_idx, dst_elem);
				}
				return DstTypedIdx_T(dst_index, src_type);
			} break;
			case SrcType_T(Type::known_function): {
				const BasicKnownFunction<SrcTypedIdx_T> src_function = src_store.at(src_index).known_function; //no reference, as src and dst could be same store -> may reallocate
				auto dst_function = BasicKnownFunction<DstTypedIdx_T>{ src_function.type };
				for (std::size_t i = 0u; i < fn::param_count(src_function.type); i++) {
					dst_function.params[i] = tree::copy<DstTypedIdx_T>(src_store, dst_store, src_function.params[i]);
				}
				return DstTypedIdx_T(dst_store.insert(dst_function), src_type);
			} break;
			case SrcType_T(Type::generic_function): {
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
			case SrcType_T(Type::variable): {
				std::string src_name; //in most cases the small string optimisation works, else dont care
				read(src_store, src_index, src_name);
				const std::size_t dst_index = insert_string(dst_store, src_name);
				return DstTypedIdx_T(dst_index, src_type);
			} break;
			case SrcType_T(Type::complex): {
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
			default: assert(false); //if this assert hits, the switch above needs more cases.
				return DstTypedIdx_T();
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
				case Type_T(Type::sum): 
					[[fallthrough]];
				case Type_T(Type::product): {
					std::size_t last_node_idx = store.insert(TypedIdxSLC_T{ equation.rhs_head });
					equation.rhs_head = TypedIdx_T(last_node_idx, lhs_type); //new rhs_head is product (sum) of old rhs_head divided by (minus) lhs_head factors (summands).
					for (const TypedIdx_T elem : vdc::range(store, lhs_index)) {
						if (tree::contains(store, elem, to_isolate)) {
							equation.lhs_head = elem; 
						}
						else {
							const TypedIdx_T new_rhs_elem = (lhs_type == Type::sum ? 
								build_negated <Store_T, TypedIdx_T>(store, elem) : 
								build_inverted<Store_T, TypedIdx_T>(store, elem));
							last_node_idx = TypedIdxSLC_T::insert_new(store, last_node_idx, new_rhs_elem);
						}
					}
					TypedIdxSLC_T::free_slc(store, lhs_index); //all factors (summands) have been shifted to rhs -> delete SLC (but not recursively, elems have new owner!)
				} break;
				case Type_T(Type::known_function): {
					BasicKnownFunction<TypedIdx_T>* function = &store.at(lhs_index).known_function;
					if (fn::param_count(function->type) == 1u) {
						equation.lhs_head = function->params[0];
						function->params[0] = equation.rhs_head;
						equation.rhs_head = TypedIdx_T(lhs_index, lhs_type);
						switch (function->type) {
						case FnType::asinh: function->type = FnType::sinh;  break;
						case FnType::acosh: function->type = FnType::cosh;  break;
						case FnType::atanh: function->type = FnType::tanh;  break;
						case FnType::asin:  function->type = FnType::sin;   break;
						case FnType::acos:  function->type = FnType::cos;   break;
						case FnType::atan:  function->type = FnType::tan;   break;
						case FnType::sinh:  function->type = FnType::asinh; break;
						case FnType::cosh:  function->type = FnType::acosh; break;
						case FnType::tanh:  function->type = FnType::atanh; break;
						case FnType::exp:   function->type = FnType::ln;    break;
						case FnType::ln:    function->type = FnType::exp;   break;
						case FnType::sin:   function->type = FnType::asin;  break;
						case FnType::cos:   function->type = FnType::acos;  break;
						case FnType::sqrt: {	
							function->type = FnType::pow;
							const TypedIdx_T square = build_value<TypedIdx_T>(store, 2.0);
							function = &store.at(lhs_index).known_function;
							function->params[1] = square;	
						} break;
						default: assert(false); //function type is not yet added or not invertable
						}
					}
					else if (fn::param_count(function->type) == 2u) {
						const bool to_isolate_in_0 = tree::contains(store, function->params[0], to_isolate);						
						if (to_isolate_in_0) { //-> TypedIdx carussell is same deal as with single parameter function
							switch (function->type) {
							case FnType::pow: {
								equation.lhs_head = function->params[0];
								function->params[0] = equation.rhs_head;
								equation.rhs_head = TypedIdx_T(lhs_index, lhs_type);
								const TypedIdx_T inverted = build_inverted(store, function->params[1]);
								function = &store.at(lhs_index).known_function;
								function->params[1] = inverted;	
							} break;
							default: assert(false); //function type is not yet added or not invertable
							}
						}
						else {
							assert(tree::contains(store, function->params[1], to_isolate));
							assert(false); //this path still needs to be made
						}
					}
					else {
						assert(false);
					}
				} break;
				case Type_T(Type::generic_function): 
					assert(false); break;
				case Type_T(Type::variable): 
					assert(false); break;
				case Type_T(Type::complex): 
					assert(false); break;
				case Type_T(pattern::_tree_match): 
					assert(false); break;
				case Type_T(pattern::_value_match): 
					assert(false); break;
				case Type_T(pattern::_value_proxy):
					assert(false); break;
				default: assert(false); //if this assert hits, the switch above needs more cases.
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
			return fold::simple_fold<fold::Bool>(store, ref, [](const auto, const Type type) -> fold::Bool { return type == Type::variable; });
		} //contains_variables

		bool contains_variables(const pattern::PnStore& store, const pattern::PnTypedIdx ref)
		{
			using namespace pattern;
			const auto test_for_variables = [](const auto, const PnType type) -> fold::Bool {
				return type == Type::variable || type.is<PnVariable>();
			};
			return fold::simple_fold<fold::Bool>(store, ref, test_for_variables);
		} //contains_variables

		TypedIdx search_variable(const Store& store, const TypedIdx head, std::string_view name)
		{
			using Res = fold::MightCut<TypedIdx>;
			const auto test_for_name = [name, &store](const std::uint32_t index, const Type type) -> Res {
				return (type == Type::variable && string_compare(store, index, name) == std::strong_ordering::equal) ?
					fold::done(TypedIdx(index, type)) : //name was found -> cut tree evaluation here
					fold::more(TypedIdx());
			};
			return *fold::simple_fold<Res>(store, head, test_for_name);
		} //search_variable

		template<typename Store_T, typename TypedIdx_T>
		bool change_subtree(Store_T& store, const TypedIdx_T ref, const TypedIdx_T from, const TypedIdx_T to)
		{
			using Type_T = TypedIdx_T::Enum_T;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			const auto [index, type] = ref.split();
			switch (type) {
			case Type_T(Type::sum): 
				[[fallthrough]];
			case Type_T(Type::product): {
				for (TypedIdx_T& elem : vdc::range(store, index)) {
					if (elem == from) {
						elem = to;
						return true;
					}
					else if (tree::change_subtree(store, elem, from, to)) {
						return true;
					}
				}
			} break;
			case Type_T(Type::generic_function): {
				GenericFunction& generic_function = store.at(index).generic_function;
				for (TypedIdx_T& param : fn::range(store, generic_function)) {
					if (param == from) {
						param = to;
						return true;
					}
					else if (tree::change_subtree(store, param, from, to)) {
						return true;
					}
				}
			} break;
			case Type_T(Type::known_function): {
				BasicKnownFunction<TypedIdx_T>& known_function = store.at(index).known_function;
				for (TypedIdx_T& param : fn::range(known_function)) {
					if (param == from) {
						param = to;
						return true;
					}
					else if (tree::change_subtree(store, param, from, to)) {
						return true;
					}
				}
			} break;
			case Type_T(Type::variable): 
				break;
			case Type_T(Type::complex): 
				break;
			case Type_T(pattern::_tree_match): 
				break;
			case Type_T(pattern::_value_match): if constexpr (pattern) {
				pattern::ValueMatchVariable& var = store.at(index).value_match;
				if (var.match_idx == from) {
					var.match_idx = to; 
					return true;
				}
				if (var.copy_idx == from) {
					var.copy_idx = to;
					return true;
				}
			} break;
			case Type_T(pattern::_value_proxy):
				break;
			default: assert(false); //if this assert hits, the switch above needs more cases.
			}
			return false;
		} //change_subtree		
		template bool change_subtree<Store, TypedIdx>(Store& store, const TypedIdx ref, const TypedIdx from, const TypedIdx to);

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
			case Type_T(Type::sum): 
				[[fallthrough]];
			case Type_T(Type::product): {
				for (const auto elem : vdc::range(store, index)) {
					const Res_T elem_res = fold::simple_fold<Res_T>(store, elem, apply);
					if constexpr (may_return_early) { if (elem_res.return_early()) { return elem_res; } }
				}
			} break;
			case Type_T(Type::known_function): {
				const BasicKnownFunction<TypedIdx_T> known_function = store.at(index).known_function; //no reference, as apply might mutate store
				for (const auto param : fn::range(known_function)) {
					const Res_T elem_res = fold::simple_fold<Res_T>(store, param, apply);
					if constexpr (may_return_early) { if (elem_res.return_early()) { return elem_res; } }
				}
			} break;
			case Type_T(Type::generic_function): {
				const GenericFunction generic_function = store.at(index).generic_function;  //no reference, as apply might mutate store
				for (const auto param : fn::range(store, generic_function)) {
					const Res_T elem_res = fold::simple_fold<Res_T>(store, param, apply);
					if constexpr (may_return_early) { if (elem_res.return_early()) { return elem_res; } }
				}
			} break;
			case Type_T(Type::variable): 
				break;
			case Type_T(Type::complex): 
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
			default: assert(false); //if this assert hits, the switch above needs more cases.
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
			case Type_T(Type::sum): 
				[[fallthrough]];
			case Type_T(Type::product): {
				Res_T acc = init;
				for (const auto elem : vdc::range(store, index)) {
					const Res_T elem_res = fold::tree_fold<Res_T>(store, elem, op_apply, leaf_apply, init, finally);
					acc = op_apply(index, type, acc, elem_res);
					if constexpr (may_return_early) { if (acc.return_early()) { return acc; } }
				}
				return finally(acc, ref);
			} 
			case Type_T(Type::known_function): {
				Res_T acc = init;
				const BasicKnownFunction<TypedIdx_T> known_function = store.at(index).known_function; //no reference, as apply might mutate store
				for (const auto param : fn::range(known_function)) {
					const Res_T param_res = fold::tree_fold<Res_T>(store, param, op_apply, leaf_apply, init, finally);
					acc = op_apply(index, type, acc, param_res);
					if constexpr (may_return_early) { if (acc.return_early()) { return acc; } }
				}
				return finally(acc, ref);
			} 
			case Type_T(Type::generic_function): {
				Res_T acc = init;
				const GenericFunction generic_function = store.at(index).generic_function;  //no reference, as apply might mutate store
				for (const auto param : fn::range(store, generic_function)) {
					const Res_T param_res = fold::tree_fold<Res_T>(store, param, op_apply, leaf_apply, init, finally);
					acc = op_apply(index, type, acc, param_res);
					if constexpr (may_return_early) { if (acc.return_early()) { return acc; } }
				}
				return finally(acc, ref);
			} 
			default:
				assert(false); //if this assert hits, the switch above needs more cases. 
				[[fallthrough]];
			case Type_T(Type::variable): 
				[[fallthrough]];
			case Type_T(Type::complex): 
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

	ArithmeticTerm::ArithmeticTerm(std::string& name)
		:store(name.size() / 2)
	{
		auto parse_string = ParseString(name);
		parse_string.allow_implicit_product();
		parse_string.remove_space();
		const std::size_t error_pos = find_first_not_arithmetic(TokenView(parse_string.tokens));
		intern::throw_if<ParseFailure>(error_pos != TokenView::npos, error_pos, "illegal character");
		this->head = build(this->store, parse_string);
		name = std::move(parse_string.name); //give content of name back to name
	} //ArithmeticTerm

	void ArithmeticTerm::combine_layers() noexcept
	{
		tree::combine_layers(this->store, this->head);
	}

	void ArithmeticTerm::combine_values_inexact() noexcept
	{
		if (const OptComplex val = tree::combine_values_inexact(this->store, this->head)) {
			this->head = TypedIdx(this->store.insert(*val), Type::complex);
		}	
	}

	void ArithmeticTerm::combine_values_exact() noexcept
	{
		if (const OptComplex val = tree::combine_values_exact(this->store, this->head)) {
			tree::free(this->store, this->head);
			this->head = TypedIdx(this->store.insert(*val), Type::complex);
		}
	}

	void ArithmeticTerm::sort() noexcept
	{
		tree::sort(this->store, this->head);
	}

	std::string bmath::ArithmeticTerm::to_memory_layout() const
	{
		return print::to_memory_layout(this->store, this->head);
	} //to_memory_layout

	std::string ArithmeticTerm::to_string() const
	{
		std::string result;
		result.reserve(this->store.size() * 2);
		print::append_to_string(this->store, this->head, result);
		return result;
	}

	std::string ArithmeticTerm::to_pretty_string()
	{
		this->combine_layers();
		this->combine_values_exact();
		this->sort();
		return print::to_pretty_string(this->store, this->head);
	}

	std::string ArithmeticTerm::to_pretty_string() const
	{
		return print::to_pretty_string(this->store, this->head);
	}

	std::pair<intern::Store*, intern::TypedIdx> ArithmeticTerm::data() noexcept
	{
		return std::make_pair(&this->store, this->head);
	}

} //namespace bmath