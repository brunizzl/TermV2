#pragma once

#include "arithmeticTerm.hpp"

namespace bmath::intern::pattern {

	//////////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////  parsing  ///////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////

	struct [[nodiscard]] PatternParts
	{
		ParseView declarations;
		ParseView lhs;
		ParseView rhs;

		//input is assumed to be of form "<declarations> | <lhs> = <rhs>"
		//or, if no MatchVariables occur, of form "<lhs> = <rhs>"
		PatternParts(const ParseView input);
	};

	//data belonging to one TreeMatchVariable relevant while constructing pattern
	struct [[nodiscard]] TreeNameLookup
	{
		std::string_view name;
		pattern::Restriction restr;
		StupidBufferVector<TypedIdx, 4u> lhs_instances;
		StupidBufferVector<TypedIdx, 4u> rhs_instances;

		TreeNameLookup(std::string_view new_name, pattern::Restriction new_restr) noexcept
			:name(new_name), restr(new_restr) {}
	};

	//data belonging to one ValueMatchVariable relevant while constructing pattern
	struct [[nodiscard]] ValueNameLookup
	{
		std::string_view name;
		Form form;
		StupidBufferVector<TypedIdx, 4u> lhs_instances;
		StupidBufferVector<TypedIdx, 4u> rhs_instances;

		ValueNameLookup(std::string_view new_name, Form new_form) noexcept
			:name(new_name), form(new_form) {}
	};

	struct [[nodiscard]] MultiNameLookup
	{
		std::string_view name;
		std::size_t lhs_count; //(only used to throw error if multiple instances exist in lhs)
		std::size_t rhs_count; //(only used to throw error if multiple instances exist in rhs)
		MultiPn type;

		MultiNameLookup(std::string_view new_name, const MultiPn new_type) noexcept
			:name(new_name), lhs_count(0u), rhs_count(0u), type(new_type) {}
	};

	//only exists during construction of pattern
	struct NameLookupTable
	{
		std::vector<TreeNameLookup> tree_table;
		std::vector<ValueNameLookup> value_table;
		std::vector<MultiNameLookup> multi_table;
		bool build_lhs = true; //false -> currently building rhs

		//assumes to only get declarations part of pattern
		NameLookupTable(ParseView declarations);

		TypedIdx insert_instance(MathStore& store, const ParseView input);
	};

	struct PatternBuildFunction
	{
		//the index of name in table is also match_data_idx of TreeMatchVariable
		NameLookupTable& table;

		//equivalent to build() for pattern
		TypedIdx operator()(MathStore& store, ParseView input);
	};


	//////////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////  tree manipulation  /////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////


	using PnType = SumEnum<MatchType, MathType>;
	using PnTypedIdx = BasicTypedIdx<PnType>;

	union PnUnion
	{
		Complex complex;
		IndexVector parameters; //all in Variadic and all in Fn
		CharVector char_vec;
		pattern::TreeMatchVariable tree_match;    //only expected as part of pattern
		pattern::ValueMatchVariable value_match;  //only expected as part of pattern

		constexpr PnUnion(const Complex&                     val) noexcept :complex(val)     {}
		constexpr PnUnion(const IndexVector&                 val) noexcept :parameters(val)  {}
		constexpr PnUnion(const CharVector&                  val) noexcept :char_vec(val)    {}
		constexpr PnUnion(const pattern::TreeMatchVariable&  val) noexcept :tree_match(val)  {}
		constexpr PnUnion(const pattern::ValueMatchVariable& val) noexcept :value_match(val) {}
		constexpr PnUnion()                                       noexcept :complex(0.0)     {}

		constexpr auto operator<=>(const PnUnion&) const = default;

		constexpr operator const Complex                    & () const noexcept { return this->complex;     }
		constexpr operator const IndexVector                & () const noexcept { return this->parameters;  }
		constexpr operator const CharVector                 & () const noexcept { return this->char_vec;    }
		constexpr operator const pattern::TreeMatchVariable & () const noexcept { return this->tree_match;  }
		constexpr operator const pattern::ValueMatchVariable& () const noexcept { return this->value_match; }
		
		constexpr operator Complex                    & () noexcept { return this->complex;     }
		constexpr operator IndexVector                & () noexcept { return this->parameters;  }
		constexpr operator CharVector                 & () noexcept { return this->char_vec;    }
		constexpr operator pattern::TreeMatchVariable & () noexcept { return this->tree_match;  }
		constexpr operator pattern::ValueMatchVariable& () noexcept { return this->value_match; }
	};

	using PnStore = BasicStore<PnUnion>;

	static_assert(StoreLike<PnStore>);

	using UnsavePnRef = BasicUnsaveRef<PnType, PnUnion>;
	using PnRef = BasicSaveRef<PnType, const PnStore>;
	using MutPnRef = BasicSaveRef<PnType, PnStore>;


	static_assert(Reference<UnsavePnRef>);
	static_assert(Reference<PnRef>);
	static_assert(Reference<MutPnRef>);

	bool meets_restriction(const UnsaveRef ref, const Restriction restr);

	bool has_form(const Complex& nr, const Form form);


	enum class Side { lhs, rhs }; //site to match against vs. side to copy from
	enum class Convert { all, basic }; //if basic, only TreeMatch and MultiMatch are converted

	//representation of RewriteRule without pattern specific elements
	// -> allows manipulation of rule with other rules
	//can not be used to match against, but RewriteRule can be build from this
	struct IntermediateRewriteRule
	{
		TypedIdx lhs_head = TypedIdx{};
		TypedIdx rhs_head = TypedIdx{};
		MathStore store = {}; //acts as both store for rhs and lhs

		MutRef lhs_mut_ref() noexcept { return MutRef(this->store, this->lhs_head); }
		MutRef rhs_mut_ref() noexcept { return MutRef(this->store, this->rhs_head); }
		Ref lhs_ref() const noexcept { return Ref(this->store, this->lhs_head); }
		Ref rhs_ref() const noexcept { return Ref(this->store, this->rhs_head); }

		IntermediateRewriteRule(std::string name, Convert convert = Convert::all);
		std::string to_string() const;
		std::string lhs_memory_layout() const;
		std::string rhs_memory_layout() const;
		std::string lhs_tree(const std::size_t offset = 0u) const;
		std::string rhs_tree(const std::size_t offset = 0u) const;
	};

	//this is the form needed for a rule to be applied, however once it is in this form, 
	//  it can not be manipulated further by other rules
	struct RewriteRule
	{
		PnTypedIdx lhs_head;
		PnTypedIdx rhs_head;
		PnStore store; //acts as both store for rhs and lhs

		//if convert == Convert::basic only TreeMatch and MultiPn are converted from their NamedFn form
		//  (enables to use rewrite rules to help building value match patterns)
		RewriteRule(std::string name, Convert convert = Convert::all);

		MutPnRef lhs_mut_ref() noexcept { return MutPnRef(this->store, this->lhs_head); }
		MutPnRef rhs_mut_ref() noexcept { return MutPnRef(this->store, this->rhs_head); }
		PnRef lhs_ref() const noexcept { return PnRef(this->store, this->lhs_head); }
		PnRef rhs_ref() const noexcept { return PnRef(this->store, this->rhs_head); }
	};

	//algorithms specific to patterns
	namespace pn_tree {

		//mostly stripped down version of tree::combine_values_exact to find calculate SharedValueDatum.value
		// from the start_val taken out of matched term
		OptComplex eval_value_match(const UnsavePnRef ref, const Complex& start_val);

		//copies math_ref into dst_store but changes math representations of pattern specific nodes 
		//  to their final pattern versions
		PnTypedIdx intermediate_to_pattern(const UnsaveRef math_ref, PnStore& dst_store, const Side side, const Convert convert);

	} //namespace pn_tree

	namespace match {

		//all MatchVariables of same name in pattern (e.g. "a" in pattern "a*b+a" share the same SharedTreeDatum to know 
		//whitch actually matched, and if the name "a" is already matched, even if the current instance is not.
		struct SharedTreeDatum
		{
			TypedIdx match_idx = TypedIdx{}; //indexes in Term to simplify
			TypedIdx responsible = TypedIdx{}; //the instance of TreeMatchVariable that was setting match_idx

			constexpr bool is_set() const noexcept
			{
				assert(equivalent(this->responsible != TypedIdx{}, this->match_idx != TypedIdx{}));
				return  this->responsible != TypedIdx{};
			}
		};

		struct SharedValueDatum
		{
			Complex value = std::numeric_limits<double>::quiet_NaN();
			//the instance of ValueMatchVariable that was setting value_match (thus is also responsible for resetting)
			TypedIdx responsible = TypedIdx{};

			constexpr bool is_set() const noexcept
			{
				assert(equivalent(!std::isnan(this->value.real()), this->responsible != TypedIdx{}));
				return  this->responsible != TypedIdx{};
			}
		};

		struct SharedVariadicDatum
		{
			//no sum or product in a pattern may have more summands / factors than max_pn_variadic_params_count many
			static constexpr std::size_t max_pn_variadic_params_count = 6u;

			using MatchPos_T = decltype(IndexVector::Info::size);

			//every element in pattern (except all MultiPn) has own entry which logs, 
			//  with which element in term to match it currently is associated with.
			std::array<MatchPos_T, max_pn_variadic_params_count> match_positions = {};

			TypedIdx match_idx = TypedIdx{}; //indexes in Term to simplify (the haystack)

			constexpr SharedVariadicDatum() noexcept { this->match_positions.fill(-1u); }

			//checks this->match_positions if needle is contained
			//use with care: might check more than actually contained in specific pattern!
			constexpr bool index_matched(const MatchPos_T needle) const noexcept
			{
				const auto stop = this->match_positions.end();
				return std::find(this->match_positions.begin(), stop, needle) != stop;
			}
		};

		//to allow a constant RewriteRule to be matched against, all match info is stored here
		struct MatchData
		{
			//maximal number of unrelated ValueMatchVariables allowed per pattern
			static constexpr std::size_t max_value_match_count = 2u;
			//maximal number of unrelated TreeMatchVariables allowed per pattern
			static constexpr std::size_t max_tree_match_count = 8u;
			//maximal number of sums and products allowed per pattern
			static constexpr std::size_t max_variadic_count = 8u;    //(max multi_match count is same)

			std::array<SharedValueDatum, max_value_match_count> value_match_data = {};
			std::array<SharedTreeDatum, max_tree_match_count> tree_match_data = {};
			//key is index of sum / product in pattern beeing matched
			StupidLinearMap<std::uint32_t, -1u, SharedVariadicDatum, max_variadic_count> variadic_data = {};

			constexpr auto& info(const TreeMatchVariable& var) noexcept { return this->tree_match_data[var.match_data_idx]; }
			constexpr auto& info(const ValueMatchVariable& var) noexcept { return this->value_match_data[var.match_data_idx]; }
			constexpr auto& info(const TreeMatchVariable& var) const noexcept { return this->tree_match_data[var.match_data_idx]; }
			constexpr auto& info(const ValueMatchVariable& var) const noexcept { return this->value_match_data[var.match_data_idx]; }

			constexpr auto& multi_info(const std::uint32_t idx) noexcept { return this->variadic_data.vals[idx]; }
			constexpr auto& multi_info(const std::uint32_t idx) const noexcept { return this->variadic_data.vals[idx]; }
		};

		//compares term starting at ref.index in ref.store with pattern starting at pn_ref.index in pn_ref.store
		//if match is succsessfull, match_data stores what pattern's match variables matched and true is returned.
		//if match was not succsessfull, match_data is NOT reset and false is returned
		bool permutation_equals(const pattern::UnsavePnRef pn_ref, const UnsaveRef ref, MatchData& match_data);

		//resets not all matched variables appearing in pn_ref, but only the ones also set by pn_ref
		//example: in whole pattern "a+a*b" "a" may be matched as single summand, 
		//  thus resetting own variables in part "a*b" will only reset "b".
		void reset_own_matches(const pattern::UnsavePnRef pn_ref, MatchData& match_data);

		//determines weather there is a way to match pn_ref in haystack_ref (thus pn_ref is assumed to part of a pattern)
		//pn_i is the index of the first element in pn_ref to be matched. 
		//if pn_i is not zero, it is assumed, that all previous elements in pn_ref are already matched.
		//the first haystack_k elements of haystack_ref will be skipped for the first match attemt.
		//it is assumed, that pn_ref and haystack_ref are both the same parameters type (eighter both sum or both product)
		//returns if search was successfull
		bool find_matching_permutation(const pattern::UnsavePnRef pn_ref, const UnsaveRef haystack_ref,
			MatchData& match_data, std::uint32_t pn_i, std::uint32_t haystack_k);

		//expects pn_ref to already be matched to ref via match_data
		//if one exists, this function finds a different match of pn_ref in ref, appearing after the current one
		//  in all permutations
		bool subsequent_permutation_equals(const pattern::UnsavePnRef pn_ref, const UnsaveRef ref, MatchData& match_data);

		//copies pn_ref with match_data into dst_store, returns head of copied result.
		[[nodiscard]] TypedIdx copy(const pattern::UnsavePnRef pn_ref, const MatchData& match_data,
			const MathStore& src_store, MathStore& dst_store);

		//the function will try to match the head of in with the head of ref and if so, replace the matched part with out.
		//if a transformation happened, Just the new subterm is returned to replace the TypedIdx 
		//  of the old subterm in its parent, else nothing.
		//if the result contains something, ref is no longer valid.
		[[nodiscard]] std::optional<TypedIdx> match_and_replace(
			const pattern::UnsavePnRef in, const pattern::UnsavePnRef out, const MutRef ref);

		//tries to match in (in postoreder) against every subterm of ref and finally ref itself. if a deeper match was found, 
		// snd of return value is true. 
		//if fst of return value is valid, ref could be matched and got replaced by *fst of return value, 
		//  meaning ref is no longer valid and caller needs to replace ref with *return_value.first  
		[[nodiscard]] std::pair<std::optional<TypedIdx>, bool> recursive_match_and_replace(
			const pattern::UnsavePnRef in, const pattern::UnsavePnRef out, const MutRef ref);


		//all rules in [start, stop) are applied until no matching rule is found. in between tree::establish_basic_order is called
		//the new head of ref is retuned
		template<intern::IterOver<const intern::pattern::RewriteRule> Iter>
		TypedIdx apply_rule_range(const Iter start, const Iter stop, const MutRef ref)
		{
			TypedIdx head = tree::establish_basic_order(ref);
		try_all_rules:
			for (auto rule = start; rule != stop; ++rule) {
				const auto [head_match, deeper_match] =
					recursive_match_and_replace(rule->lhs_ref(), rule->rhs_ref(), ref.new_at(head));

				if (head_match) {
					head = *head_match;
				}
				if (head_match || deeper_match) {
					//assert(tree::valid_storage(ref.new_at(head)));
					head = tree::establish_basic_order(ref.new_at(head));
					//assert(tree::valid_storage(ref.new_at(head)));
					goto try_all_rules;
				}
			}
			return head;
		}

	} //namespace match

} //namespace bmath::intern::pattern

namespace bmath::intern::fn {

	constexpr auto& range(const pattern::UnsavePnRef ref) noexcept { return ref->parameters; }
	constexpr auto& range(const pattern::PnRef ref) noexcept { return ref->parameters; }
	constexpr auto    save_range(const pattern::PnRef ref) noexcept { return ref.cast<IndexVector>(); }
	constexpr auto         range(const pattern::MutPnRef ref) noexcept { return ref.cast<IndexVector>(); }
	constexpr auto& unsave_range(const pattern::MutPnRef ref) noexcept { return ref->parameters; }

	constexpr const CharVector& named_fn_name(const pattern::UnsavePnRef named_fn) noexcept
	{
		return static_cast<const CharVector&>(*named_fn.raw_at(named_fn_name_index(named_fn)));
	}

} //namespace bmath::intern::fn