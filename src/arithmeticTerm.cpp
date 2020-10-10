
#include <cassert>
#include <sstream>
#include <array>
#include <algorithm>
#include <cstring>
#include <bit>

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
			const BasicKnownFunction<TypedIdx_T>& known_function = store.at(index).known_function;
			for (const auto param : fn::range(known_function)) {
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
		case Type_T(pattern::_match_variable): if constexpr (pattern) {
			assert(false);
		} break;
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
		{ Type::generic_function            , 00 }, //order of parameters is given -> most unique
		{ Type::known_function              , 16 }, //order of parameters is given -> most unique
		{ Type::product                     , 32 }, //order of operands my vary -> second most unique
		{ Type::sum                         , 48 }, //order of operands my vary -> second most unique
		{ Type::variable                    , 64 }, //can only match pattern variable directly -> least unique
		{ Type::complex                     , 80 }, //can only match pattern variable directly -> least unique
		{ pattern::PnSpecial::match_variable, 96 }, //always place at end, not really part of uniqueness, but still.
	});
	constexpr int uniqueness(pattern::PnType type) noexcept { return find(uniqueness_table, type); }

	//utility for both KnownFunction and GenericFunction
	namespace fn {

		Complex eval(FnType type, const std::array<Complex, 3>& params)
		{
			switch (type) {
			case FnType::asinh: return std::asinh(params[0]);
			case FnType::acosh: return std::acosh(params[0]);
			case FnType::atanh: return std::atanh(params[0]);
			case FnType::asin : return std::asin (params[0]);
			case FnType::acos : return std::acos (params[0]);
			case FnType::atan : return std::atan (params[0]);
			case FnType::sinh : return std::sinh (params[0]);
			case FnType::cosh : return std::cosh (params[0]);
			case FnType::tanh : return std::tanh (params[0]);
			case FnType::sqrt : return std::sqrt (params[0]);
			case FnType::pow  : return std::pow  (params[0], params[1]);
			case FnType::log  : return std::log  (params[1]) / std::log(params[0]); //https://en.wikipedia.org/wiki/Complex_logarithm#Generalizations
			case FnType::exp  : return std::exp  (params[0]);
			case FnType::sin  : return std::sin  (params[0]);
			case FnType::cos  : return std::cos  (params[0]);
			case FnType::tan  : return std::tan  (params[0]);
			case FnType::abs  : return std::abs  (params[0]);
			case FnType::arg  : return std::arg  (params[0]);
			case FnType::ln   : return std::log  (params[0]);
			case FnType::re   : return std::real (params[0]);
			case FnType::im   : return std::imag (params[0]);
			default: assert(false);
				return Complex(0.0, 0.0);
			}
		} //eval

	} //namespace fn

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////exported in header/////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	namespace pattern {
		
		template<typename Store_T, typename TypedIdx_T>
		bool has_form(const Store_T & store, const TypedIdx_T ref, const Form form)
		{
			return false;
		}

		PnTerm::PnTerm(std::string& name) 
			:shared_match_data({ SharedMatchData{ TypedIdx() }, { TypedIdx() }, { TypedIdx() }, { TypedIdx() }, { TypedIdx() }, { TypedIdx() }, })
		{
			auto parse_string = ParseString(name);
			parse_string.allow_implicit_product();
			parse_string.remove_space();
			const auto parts = split(parse_string);
			const auto name_map = parse_declarations(parts.declarations);
			throw_if<ParseFailure>(name_map.size() > max_var_count, 0u, "too many variables declared");
			const PatternBuildFunction build_function = { name_map };
			this->lhs_head = build_function(this->lhs_store, parts.lhs);
			this->rhs_head = build_function(this->rhs_store, parts.rhs);
			tree::combine_layers(this->lhs_store, this->lhs_head);
			tree::combine_layers(this->rhs_store, this->rhs_head);
			if (const auto val = tree::combine_values_exact(this->lhs_store, this->lhs_head)) {
				this->lhs_head = PnTypedIdx(this->lhs_store.insert(*val), Type::complex);
			}
			if (const auto val = tree::combine_values_exact(this->rhs_store, this->rhs_head)) {
				this->rhs_head = PnTypedIdx(this->rhs_store.insert(*val), Type::complex);
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
			return print::show_memory_layout(this->lhs_store, this->lhs_head);
		}

		std::string PnTerm::rhs_memory_layout() const
		{
			return print::show_memory_layout(this->rhs_store, this->rhs_head);
		}

	} //namespace pattern

	namespace fn {

		template<typename Store_T1, typename Store_T2>
		std::strong_ordering compare_name(const Store_T1& store_1, const Store_T2& store_2, const GenericFunction& func_1, const GenericFunction& func_2)
		{
			if (func_1.name_size == GenericFunction::NameSize::small &&
				func_2.name_size == GenericFunction::NameSize::small) 
			{ //std::string_view does not currently support <=>
				const int cmp = std::strcmp(func_1.short_name, func_2.short_name);
				if (cmp < 0) {
					return std::strong_ordering::less;
				}
				if (cmp > 0) {
					return std::strong_ordering::greater;
				}
				else {
					return std::strong_ordering::equal;
				}
			}
			if (func_1.name_size == GenericFunction::NameSize::longer &&
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
					free(store, factor);
				}
				TypedIdxSLC_T::free_slc(store, index);
			} break;
			case Type_T(Type::known_function): {
				const BasicKnownFunction<TypedIdx_T>& known_function = store.at(index).known_function;
				for (const auto param : fn::range(known_function)) {
					free(store, param);
				}
				store.free(index);
			} break;
			case Type_T(Type::generic_function): {
				const GenericFunction& generic_function = store.at(index).generic_function;
				for (const auto param : fn::range(store, generic_function)) {
					free(store, param);
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
			} break;
			case Type_T(pattern::_match_variable): if constexpr (pattern) {
				store.free(index);
			} break;
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
						combine_layers(store, elem);
					}
				}
			} break;
			case Type_T(Type::known_function): {
				const BasicKnownFunction<TypedIdx_T>& known_function = store.at(index).known_function;
				for (const auto param : fn::range(known_function)) {
					combine_layers(store, param);
				}
			} break;
			case Type_T(Type::generic_function): {
				const GenericFunction& generic_function = store.at(index).generic_function;
				for (const auto param : fn::range(store, generic_function)) {
					combine_layers(store, param);
				}
			} break;
			case Type_T(Type::variable):
				break;
			case Type_T(Type::complex):
				break;
			case Type_T(pattern::_match_variable):
				break;
			default: assert(false); //if this assert hits, the switch above needs more cases.
			}
		} //combine_layers

		std::optional<Complex> combine_values_unexact(Store& store, const TypedIdx ref)
		{
			const auto [index, type] = ref.split();
			switch (type) {
			case Type::sum: {
				Complex result_val = 0.0;
				bool only_values = true;
				for (auto& summand : vdc::range(store, index)) {
					if (const auto summand_val = combine_values_unexact(store, summand)) {
						result_val += *summand_val;
						summand = Sum::null_value;
					}
					else {
						only_values = false;
					}
				}
				if (only_values) {
					Sum::free_slc(store, index); //all summands have already (implicitly) been freed in the loop above
					return { result_val };
				}
				else if (result_val != 0.0) {
					const auto new_summand = TypedIdx(store.insert(result_val), Type::complex);
					Sum::insert_new(store, index, new_summand);
				}
				return {};
			} break;
			case Type::product: {
				Complex result_val = 1.0;
				bool only_values = true;
				for (auto& factor : vdc::range(store, index)) {
					if (const auto factor_val = combine_values_unexact(store, factor)) {
						result_val *= *factor_val;
						factor = Product::null_value;
					}
					else {
						only_values = false;
					}
				}
				if (only_values) {
					Product::free_slc(store, index); //all factors have already been freed in the loop above
					return { result_val };
				}
				else if (result_val != 1.0) {
					const auto new_factor = TypedIdx(store.insert(result_val), Type::complex);
					Product::insert_new(store, index, new_factor);
				}
				return {};
			} break;
			case Type::known_function: {
				KnownFunction& known_function = store.at(index).known_function;
				std::array<Complex, 3> results_values;
				std::bitset<3> results_computable = 0;
				for (std::size_t i = 0; i < fn::param_count(known_function.type); i++) {
					if (const auto param_res = combine_values_unexact(store, known_function.params[i])) {
						results_values[i] = *param_res;
						results_computable.set(i);
					}
				}
				if (results_computable.count() == fn::param_count(known_function.type)) {
					const FnType type = known_function.type;
					store.free(index);
					return { fn::eval(type, results_values) };
				}
				else {
					for (std::size_t i = 0; i < fn::param_count(known_function.type); i++) {
						if (results_computable.test(i)) {
							known_function.params[i] = TypedIdx(store.insert(results_values[i]), Type::complex);
						}
					}
					return {};
				}
			} break;
			case Type::generic_function: {
				GenericFunction& function = store.at(index).generic_function;
				for (auto& elem : fn::range(store, function)) {
					if (const auto param_res = combine_values_unexact(store, elem)) {
						elem = TypedIdx(store.insert(*param_res), Type::complex);
					}
				}
				return {};
			} break;
			case Type::variable: {
				return {};
			} break;
			case Type::complex: {
				const Complex value = store.at(index).complex;
				store.free(index);
				return { value };
			} break;
			default: assert(false); //if this assert hits, the switch above needs more cases.
				return {};
			}
		} //combine_values_unexact

		template<typename Store_T, typename TypedIdx_T>
		std::optional<Complex> combine_values_exact(Store_T& store, const TypedIdx_T ref)
		{
			using Type_T = TypedIdx_T::Enum_T;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			const auto [index, type] = ref.split();
			switch (type) {
			case Type_T(Type::sum): {
				Complex result_val = 0.0;
				bool only_values = true;
				for (auto& summand : vdc::range(store, index)) {
					if (const auto summand_val = combine_values_exact(store, summand)) {
						result_val += *summand_val;
						summand = TypedIdxSLC_T::null_value;
					}
					else {
						only_values = false;
					}
				}
				if (only_values) {
					TypedIdxSLC_T::free_slc(store, index); //all summands have already (implicitly) been freed in the loop above
					return { result_val };
				}
				else if (result_val != 0.0) {
					const auto new_summand = TypedIdx_T(store.insert(result_val), Type::complex);
					TypedIdxSLC_T::insert_new(store, index, new_summand);
				}
				return {};
			} break;
			case Type_T(Type::product): {
				Complex result_val = 1.0;
				bool only_values = true;
				for (auto& factor : vdc::range(store, index)) {
					if (const auto factor_val = combine_values_exact(store, factor)) {
						result_val *= *factor_val;
						factor = TypedIdxSLC_T::null_value;
					}
					else {
						only_values = false;
					}
				}
				if (only_values) {
					TypedIdxSLC_T::free_slc(store, index); //all factors have already been freed in the loop above
					return { result_val };
				}
				else if (result_val != 1.0) {
					const auto new_factor = TypedIdx_T(store.insert(result_val), Type::complex);
					TypedIdxSLC_T::insert_new(store, index, new_factor);
				}
				return {};
			} break;
			case Type_T(Type::known_function): {
				BasicKnownFunction<TypedIdx_T>& function = store.at(index).known_function;
				for (auto& elem : fn::range(function)) {
					if (const auto param_res = combine_values_exact(store, elem)) {
						elem = TypedIdx_T(store.insert(*param_res), Type::complex);
					}
				}
				return {};
			} break;
			case Type_T(Type::generic_function): {
				GenericFunction& function = store.at(index).generic_function;
				for (auto& elem : fn::range(store, function)) {
					if (const auto param_res = combine_values_exact(store, elem)) {
						elem = TypedIdx_T(store.insert(*param_res), Type::complex);
					}
				}
				return {};
			} break;
			case Type_T(Type::variable): 
				return {};
			case Type_T(Type::complex): {
				const Complex value = store.at(index).complex;
				store.free(index);
				return { value };
			} break;
			case Type_T(pattern::_match_variable): 
				return {};
			default: assert(false); //if this assert hits, the switch above needs more cases.
				return {};
			}
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
					const auto iter_compare = compare(store_1, store_2, *iter_1, *iter_2);
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
					const auto iter_compare = compare(store_1, store_2, *iter_1, *iter_2);
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
					const auto iter_compare = compare(store_1, store_2, *iter_1, *iter_2);
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
				return string_compare(store_1, store_2, index_2, index_1); //reverse order, as to_pretty_string is reversed again
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
			case Type_T(pattern::_match_variable): if constexpr (pattern) {
				const pattern::MatchVariable& var_1 = store_1.at(index_1).match_variable;
				const pattern::MatchVariable& var_2 = store_2.at(index_2).match_variable;
				if (var_1.form != var_2.form) {
					return var_1.form <=> var_2.form;
				}
				//if (var_1.name != var_2.name) { //c++20 i need you :(
				//	return var_1.name <=> var_2.name; //reverse to make pretty_string prettier
				//}
				if (var_1.shared_data_idx != var_2.shared_data_idx) {
					return var_1.shared_data_idx <=> var_2.shared_data_idx; //reverse to make pretty_string prettier
				}
				return std::strong_ordering::equal;
			} break;
			default: assert(false); //if this assert hits, the switch above needs more cases.
			}
			return std::strong_ordering::equal;
		} //compare

		template<typename Store_T, typename TypedIdx_T>
		void sort(Store_T& store, const TypedIdx_T ref)
		{
			using Type_T = TypedIdx_T::Enum_T;
			using TypedIdxSLC_T = TermSLC<std::uint32_t, TypedIdx_T, 3>;
			constexpr bool pattern = std::is_same_v<Type_T, pattern::PnType>;

			const auto [index, type] = ref.split();
			switch (type) {
			case Type_T(Type::sum):
				[[fallthrough]];
			case Type_T(Type::product): {
				for (const auto elem : vdc::range(store, index)) {
					sort(store, elem);
				}
				TypedIdxSLC_T::sort(store, index,
					[&store](const TypedIdx_T lhs, const TypedIdx_T rhs) {
						return compare(store, store, lhs, rhs) == std::strong_ordering::less;
					});
			} break;
			case Type_T(Type::known_function): {
				const BasicKnownFunction<TypedIdx_T>& known_function = store.at(index).known_function;
				for (const auto param : fn::range(known_function)) {
					sort(store, param);
				}
			} break;
			case Type_T(Type::generic_function): {
				const GenericFunction& generic_function = store.at(index).generic_function;
				for (const auto param : fn::range(store, generic_function)) {
					sort(store, param);
				}
			} break;
			case Type_T(Type::variable):
				break;
			case Type_T(Type::complex):
				break;
			case Type_T(pattern::_match_variable): 
				break;
			default: assert(false); //if this assert hits, the switch above needs more cases.
			}
		} //sort

	} //namespace tree

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

	void ArithmeticTerm::combine_values_unexact() noexcept
	{
		if (const auto val = tree::combine_values_unexact(this->store, this->head)) {
			this->head = TypedIdx(this->store.insert(*val), Type::complex);
		}	
	}

	void ArithmeticTerm::combine_values_exact() noexcept
	{
		if (const auto val = tree::combine_values_exact(this->store, this->head)) {
			this->head = TypedIdx(this->store.insert(*val), Type::complex);
		}	
	}

	void ArithmeticTerm::sort() noexcept
	{
		tree::sort(this->store, this->head);
	}

	std::string bmath::ArithmeticTerm::show_memory_layout() const
	{
		return print::show_memory_layout(this->store, this->head);
	} //show_memory_layout

	std::string ArithmeticTerm::to_string() const
	{
		std::string result;
		result.reserve(this->store.size() * 2);
		print::append_to_string(this->store, this->head, result);
		return result;
	}

	std::string ArithmeticTerm::to_pretty_string() const
	{
		return print::to_pretty_string(this->store, this->head);
	}

} //namespace bmath