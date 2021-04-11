#pragma once

#include <string_view>
#include <string>
#include <concepts>
#include <tuple>

#include "types.hpp"
#include "parseTerm.hpp"

namespace simp {
	namespace parse {
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

			//all names that can be looked up will result in a "shallow" NodeIdx, meaning no node in the Store is attatched.
			//thus this simple structure is permitted.
			struct NameInfo
			{
				std::string_view name;
				NodeIdx value;
			};

			struct PatternInfos
			{
				std::vector<NameInfo> lambda_params = {};
				std::vector<NameInfo> match_variables = {};
			};
			struct LiteralInfos
			{
				std::vector<NameInfo> lambda_params = {};
			};
			template<typename I>
			concept InfoLike = requires (I i) { {i.lambda_params} -> std::same_as<std::vector<NameInfo>&>; };
			static_assert(InfoLike<PatternInfos>&& InfoLike<LiteralInfos>);

			//looks up global function names and the names contained in info (expected to refer to lambda parameters)
			//unknown names are returned as symbols
			NodeIdx build_symbol(Store& store, const LiteralInfos& lambda_params, bmath::intern::ParseView view);

			//same as above, but unwraps unknown beginning and ending in single quotes
			NodeIdx build_symbol(Store& store, PatternInfos& infos, bmath::intern::ParseView view);

			//expects params_view to be argument list of lambda starting with '\\', adds names to lambda_params
			//returns number of parameters added
			unsigned add_lambda_params(std::vector<NameInfo>& lambda_params, bmath::intern::ParseView params_view);
		} //namespace name_lookup

		template<bmath::intern::StoreLike Store_T>
		[[nodiscard]] NodeIdx build_value(Store_T& store, const std::complex<double> complex) noexcept
		{
			const std::size_t result_idx = store.allocate_one();
			new (&store.at(result_idx)) TermNode(complex);
			return NodeIdx(result_idx, Literal::complex);
		}

		template<bmath::intern::StoreLike Store_T>
		[[nodiscard]] NodeIdx build_negated(Store_T& store, const NodeIdx to_negate) noexcept
		{
			const std::size_t result_idx = store.allocate_one();
			new (&store.at(result_idx)) TermNode(Call{ nv::to_typed_idx(nv::CtoC::negate), to_negate });
			return NodeIdx(result_idx, Literal::call);
		}

		template<bmath::intern::StoreLike Store_T>
		[[nodiscard]] NodeIdx build_inverted(Store_T& store, const NodeIdx to_invert) noexcept
		{
			const std::size_t result_idx = store.allocate_one();
			new (&store.at(result_idx)) TermNode(Call{ nv::to_typed_idx(nv::CtoC::invert), to_invert });
			return NodeIdx(result_idx, Literal::call);
		}

		//returns the tree representation of view in store
		//lambda_offset is the sum of all parameter counts of all parent lambdas 
		//  (no outside lambdas -> lambda_offset == 0)
		template<name_lookup::InfoLike Infos>
		[[nodiscard]] NodeIdx build(Store& store, Infos& infos, bmath::intern::ParseView view);

		//has only activated SingleMatch!
		//build from a string of form "<match side> = <replace side>" 
		//  or form "<match side> | <condition(s)> = <replace side>"
		//   where <match side> and <replace side> are terms similar to LiteralTerm, but they may contain match variables
		//         <condition(s)> is a comma separated listing of extra conditions and relations on single match variables
		//note: conditions are incorporated into match_side
		//returns (match side, repacement side)
		std::pair<NodeIdx, NodeIdx> raw_rule(Store& store, std::string name);


	} //namespace parse

	namespace print {

		void append_to_string(const UnsaveRef ref, std::string& str, const int parent_infixr);

		inline std::string to_string(const UnsaveRef ref)
		{
			std::string name;
			print::append_to_string(ref, name, 0);
			return name;
		}

		std::string to_memory_layout(const Store& store, const std::initializer_list<const NodeIdx> heads);
	} //namespace print

} //namespace simp