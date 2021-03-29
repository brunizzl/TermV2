#pragma once

#include <string_view>
#include <string>

#include "types.hpp"
#include "parseTerm.hpp"

namespace simp {

	struct Head
	{
		std::size_t where;
		enum class Type
		{
			or_,		   //where specifies position of found operator
			and_,		   //where specifies position of found operator
			equals,		   //where specifies position of found operator
			not_equals,	   //where specifies position of found operator
			greater,	   //where specifies position of found operator
			smaller,	   //where specifies position of found operator
			greater_eq,    //where specifies position of found operator
			smaller_eq,    //where specifies position of found operator
			plus,	       //where specifies position of found operator
			minus,	       //where specifies position of found operator
			times,         //where specifies position of found operator
			divided,       //where specifies position of found operator
			power,         //where specifies position of found operator
			not_,		   //where is unspecified
			negate,	       //where is unspecified
			real_value,    //where is unspecified
			imag_value,    //where is unspecified
			symbol,        //where is unspecified
			call,          //where specifies position of opening parenthesis
			lambda,        //where is unspecified
			group,         //where is unspecified
		} type;
	};

	//decides what type the outhermost element has
	//offset is used to determine error position relative to begin of whole term
	[[nodiscard]] Head find_head_type(const bmath::intern::ParseView view);

	namespace name_lookup {

		//all names that can be looked up will result in a "shallow" TypedIdx, meaning no node in the Store is attatched.
		//thus this simple structure is permitted.
		struct NameInfo
		{
			std::string_view name;
			TypedIdx value;
		};

		struct NameInfos
		{
			//name_infos can contain lambda parameter or the different types of match variables.
			std::vector<NameInfo> lambda_infos = {};
			std::vector<NameInfo> match_infos = {};
		};

		//looks up global function names and the names contained in info (expected to refer to lambda parameters)
		//unknown names are returned as symbols
		TypedIdx build_literal_from_name(Store& store, const std::vector<NameInfo>& lambda_infos, std::string_view name);

		//same as above, but unwraps unknown beginning and ending in single quotes
		TypedIdx build_pattern_from_name(Store& store, const NameInfos& infos, std::string_view name);

		//expects argument list of lambda starting with '\\', adds names to infos, returns number of parameters added
		unsigned add_lambda_params(std::vector<NameInfo>& lambda_infos, bmath::intern::ParseView params);

	} //namespace name_lookup

	template<bmath::intern::StoreLike Store_T>
	[[nodiscard]] TypedIdx build_value(Store_T& store, const std::complex<double> complex) noexcept
	{
		const std::size_t result_idx = store.allocate_one();
		new (&store.at(result_idx)) TermNode(complex);
		return TypedIdx(result_idx, MathType::complex);
	}

	template<bmath::intern::StoreLike Store_T>
	[[nodiscard]] TypedIdx build_negated(Store_T& store, const TypedIdx to_negate) noexcept
	{
		const TypedIdx minus_1 = build_value(store, -1.0);
		const std::size_t result_idx = store.allocate_one();
		new (&store.at(result_idx)) TermNode(Call{ fn::to_typed_idx(fn::Comm::product), minus_1, to_negate });
		return TypedIdx(result_idx, MathType::call);
	}

	template<bmath::intern::StoreLike Store_T>
	[[nodiscard]] TypedIdx build_inverted(Store_T& store, const TypedIdx to_invert) noexcept
	{
		const TypedIdx minus_1 = build_value(store, -1.0);
		const std::size_t result_idx = store.allocate_one();
		new (&store.at(result_idx)) TermNode(Call{ fn::to_typed_idx(fn::FixedArity::pow), to_invert, minus_1 });
		return TypedIdx(result_idx, MathType::call);
	}

	//returns the tree representation of view in store
	//lambda_offset is the sum of all parameter counts of all parent lambdas (no outside lambdas -> lambda_offset == 0)
	[[nodiscard]] TypedIdx build_literal(Store& store, std::vector<name_lookup::NameInfo>& lambda_infos, bmath::intern::ParseView view);

	namespace print {

		void append_to_string(const UnsaveRef ref, std::string& str);

	} //namespace print

} //namespace simp