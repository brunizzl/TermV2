#pragma once

#include <string_view>
#include <string>
#include <concepts>
#include <tuple>
#include <optional>

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
				of_type,       //where specifies position of found operator
				cons,          //where specifies position of found operator
				not_,		   //where is unspecified
				negate,	       //where is unspecified
				real_value,    //where is unspecified
				imag_value,    //where is unspecified
				symbol,        //where is unspecified
				f_app,          //where specifies position of opening parenthesis
				lambda,        //where is unspecified
				group,         //where is unspecified
			} type;
		};

		//decides what type the outhermost element has
		//offset is used to determine error position relative to begin of whole term
		[[nodiscard]] Head find_head_type(const ParseView view);

		namespace name_lookup {

			struct NameInfo
			{
				//name of pattern stuff always contains the decorator symbols, e.g. "_x", "$x" or "xs..." not just "x" or "xs".
				std::string_view name;
				NodeIndex value;
			};

			struct PatternInfos
			{
				std::vector<NameInfo> lambda_params = {};  //only contains instances of Literal::lambda_param -> always shallow (same as in LiteralInfos)
				std::vector<NameInfo> single_matches = {}; //stored as SingleMatch::weak
				std::vector<NameInfo> multi_matches = {};  //stored as MultiMatch node with .match_state_index currently holding the identification number
				std::vector<NameInfo> value_matches = {};  //stored as call to nv::PatternFn::value_match
				bool parse_match = true; //set to false after match is parsed
			};
			struct LiteralInfos
			{
				//only contains instances of Literal::lambda_param -> always shallow
				//this vector acts as stack only storing the currently visible lambda parameters 
				std::vector<NameInfo> lambda_params = {}; 
			};
			template<typename I>
			concept InfoLike = requires (I i) { {i.lambda_params} -> std::same_as<std::vector<NameInfo>&>; };
			static_assert(InfoLike<PatternInfos>&& InfoLike<LiteralInfos>);

			//looks up global function names and the names contained in info (expected to refer to lambda parameters)
			//unknown names are returned as symbols
			NodeIndex build_symbol(Store& store, const LiteralInfos& lambda_params, ParseView view);

			//same as above, but unwraps unknown beginning and ending in single quotes
			NodeIndex build_symbol(Store& store, PatternInfos& infos, ParseView view);

			//expects params_view to be argument list of lambda starting with '\\', adds names to lambda_params
			//returns number of parameters added
			unsigned add_lambda_params(std::vector<NameInfo>& lambda_params, ParseView params_view);
		} //namespace name_lookup

		template<StoreLike Store_T>
		[[nodiscard]] NodeIndex build_value(Store_T& store, const std::complex<double> complex) noexcept
		{
			const std::size_t result_idx = store.allocate_one();
			new (&store.at(result_idx)) TermNode(complex);
			return NodeIndex(result_idx, Literal::complex);
		}

		template<StoreLike Store_T>
		[[nodiscard]] NodeIndex build_negated(Store_T& store, const NodeIndex to_negate) noexcept
		{
			const std::size_t result_idx = store.allocate_one();
			const NodeIndex minus_1 = simp::parse::build_value(store, std::complex<double>{ -1.0, 0.0 });
			new (&store.at(result_idx)) TermNode(FApp{ from_native(nv::Comm::prod), minus_1, to_negate });
			return NodeIndex(result_idx, Literal::f_app);
		}

		template<StoreLike Store_T>
		[[nodiscard]] NodeIndex build_inverted(Store_T& store, const NodeIndex to_invert) noexcept
		{
			const std::size_t result_idx = store.allocate_one();
			const NodeIndex minus_1 = simp::parse::build_value(store, std::complex<double>{ -1.0, 0.0 });
			new (&store.at(result_idx)) TermNode(FApp{ from_native(nv::CtoC::pow), to_invert, minus_1 });
			return NodeIndex(result_idx, Literal::f_app);
		}

		//returns the tree representation of view in store
		template<name_lookup::InfoLike Infos>
		[[nodiscard]] NodeIndex build(Store& store, Infos& infos, ParseView view);

		struct IAmInformedThisRuleIsNotUsableYet {};
		//has only activated SingleMatch!
		//build from a string of form "<match side> = <replace side>" 
		//  or form "<match side> | <condition(s)> = <replace side>"
		//   where <match side> and <replace side> are terms similar to LiteralTerm, but they may contain match variables
		//         <condition(s)> is a comma separated listing of extra conditions and relations on single match variables
		//note: conditions are incorporated into match_side
		//returns (match side, repacement side)
		//the last parameter is only there to make the function type different from that passed into RuleSet
		std::tuple<RuleHead, name_lookup::PatternInfos> raw_rule(Store& store, std::string& name, IAmInformedThisRuleIsNotUsableYet);

	} //namespace parse

	namespace typecheck {

		//a pattern may not contain a pattern variable and a literal symbol / lambda parameter differing in name only by 
		//  the '_' or '$' marking the pattern variable as such.
		//the first such perpetrator is returned
		std::optional<std::string_view> pattern_var_name_collision(UnsaveRef const ref, parse::name_lookup::PatternInfos const& infos);

		struct MatchVariableProps
		{
			struct Prop 
			{
				//counts how many lambda parameters are in scope e.g.
				// in "\x y . _x" the matchvariable "_x" has 2 in scope (namely "x" and "y")
				// in "\x . \y . \z . _x" the matchvariable "_x" has 3 in scope (namely "x", "y" and "z")
				int lambda_depth = -1;
			};

			//a lhs pattern variable nested inside a lambda may only occur in that same nesting level on the rhs
			//this struct catalogs the occurences of pattern variables in the left hand side.
			std::array<Prop, match::State::max_single_match_count> singles = {};
			std::array<Prop, match::State::max_single_match_count> multis = {};
		}; //struct MatchVariableProps

		//if !in_rhs -> fills props (or thows if inconsistency is detected)
		//if  in_rhs -> throws if inconsistency is detected
		//currently tests for:

		void consistent_pattern_vars(UnsaveRef const ref, MatchVariableProps& props, bool const in_rhs);

	} //namespace typecheck

	namespace print {

		void append_complex(const std::complex<double> val, std::string& dest, int parent_precedence);

		void append_to_string(const UnsaveRef ref, std::string& str, const int parent_precedence, const bool fancy = true);

		inline [[nodiscard]] std::string term_to_string(const UnsaveRef ref, const bool fancy = true)
		{
			std::string name;
			print::append_to_string(ref, name, 0, fancy);
			return name;
		}

		//causes no stackoverflow for large literals
		[[nodiscard]] std::string literal_to_string(const UnsaveRef head, const bool fancy = true);

		template<ContainerOf<NodeIndex> C>
		std::string [[nodiscard]] to_memory_layout(const Store& store, const C& heads);

		inline std::string [[nodiscard]] to_memory_layout(const Store& store, const std::initializer_list<NodeIndex> heads)
		{	
			return to_memory_layout<std::initializer_list<NodeIndex>>(store, heads);
		}

	} //namespace print

} //namespace simp