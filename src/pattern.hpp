#pragma once

#include "arithmeticTerm.hpp"

namespace bmath::intern::pattern {


	enum class Restriction
	{
		any,
		nn1, //compact for "not negative one" (basically any, but the exact term "-1" will not be accepted)
		no_val, //any, but Literal::complex is forbidden    
		symbol, //only Literal::symbol is accepted
		COUNT
	};

	//specifies more constraints on a value
	enum class Domain
	{
		natural,   //{ 1, 2, 3, ... }
		natural_0, //{ 0, 1, 2, ... }
		integer,
		negative,     //implies real   
		positive,     //implies real     	
		not_negative, //implies real  
		not_positive, //implies real 
		real,
		complex,
		COUNT
	};

	using TreeDomain = OpaqueEnum<1, Domain>;

	using TreeMatchOwning = SumEnum<TreeDomain, Restriction>;

	struct TreeMatchNonOwning :SingleSumEnumEntry {};

	using TreeMatch = SumEnum<TreeMatchOwning, TreeMatchNonOwning>;


	struct ValueProxy :SingleSumEnumEntry {};

	enum class ValueMatch 
	{
		owning, non_owning, COUNT
	};

	//represent not actual nodes in pattern tree, as all match info is stored in VariadicMatchDatum in MatchData	
	//thus all info required in the tree is given in the typed_idx, where the index is repurposed to point elsewhere
	//(elsewhere means that VariadicMatchDatum array in MatchData)
	//Comm can only hold one instance of MultiMatch directly (always fst), NonComm may hold both.
	//occurence outside of Variadic is prohibited.
	enum class MultiMatch { fst, snd, COUNT };

	//all values contained within Proxy dont reference an element in the pattern tree, but in MatchData
	//note: also LambdaParam has this behavior but is unfortunately contained in MathType, thus not in this SumEnum
	using Proxy = SumEnum<ValueProxy, MultiMatch, TreeMatch>;

	using MatchType = SumEnum<ValueMatch, Proxy>;



	using PnType = SumEnum<MatchType, MathType>;
	using PnIdx = BasicTypedIdx<PnType>;
	using PnIdxVector = StoredVector<PnIdx>;



	//in a valid pattern, all TreeMatchVariables of same name share the same restr and the same match_data_idx.
	//it is allowed to have multiple instances of the same TreeMatchVariable per side.
	//this variation of the match variable can match a subtree occuring in a term
	

	//although the type MultiMatchVariable does not exist as type of node in term, other nodes may reference it
	// (now called PnVariable::summands or PnVariable::factors)
	//Meaning: if some TypedIdx has .get_type() == PnVariable::summands, all data of this node are stored in MatchData at
	//  .get_index(), not in the pattern tree (same goes for .get_type() == PnVariable::factors).
	//The concept of a (imagined) MultiMatchVariable is quite similar to TreeMatchVariable with Restr::any, 
	//but it can match multiple summands / factors in same sum / product, not only a single one.
	//This behavor can kinda be simulated by TreeMatchVariable, if all matched elements of MultiMatchVariable are packaged 
	//  together in an extra sum / product.
	//as match::permutation_equals takes a Ref, not a MutRef however, this approach can not be realized here (and is stupid anyway)
	//struct MultiMatchVariable {}; //<- just to help your imagination out a bit.
	//Note: there may be only a single instance of a given MultiMatchVariable with given name per pattern per side.


	using ValueDomain = OpaqueEnum<2, Domain>;
	//in a valid pattern, all ValueMatchVariables of same name (although that is thrown away after building) 
	//share the same domain and the same MatchData index
	//it is allowed to have multiple instances of the same ValueMatchVariable per side.
	//this variation of the match variable can match a value of specific form occuring in a term,
	//  the form of this value may also be specified using arithmetic operations. 
	//for example "2*k+1" with k beeing a ValueMatchVariable can match "5" and set value of SharedValueDatum to "2".
	//that is achieved by not actually storing "2*k+1" as such, but as "k", with match_idx of k containing the 
	//  inverse operation: "(p-1)/2", where "p" is not an actual node, but just a PnVariable::value_proxy 
	//  to indicate the original position of k.
	//in praxis a TypedIdx with .get_type() == PnVariable::value_proxy works similar to (imagined) MultiMatchVariable, as
	//  .get_index() also does not point in store of pattern, but in an array of MatchData.
	struct ValueMatchVariable
	{
		PnIdx mtch_idx;
		std::uint32_t match_data_idx; //indexes in MatchData::value_match_data
		ValueDomain domain;

		constexpr ValueMatchVariable(PnIdx new_match, std::uint32_t new_match_data_idx, ValueDomain new_domain) noexcept
			:mtch_idx(new_match), match_data_idx(new_match_data_idx), domain(new_domain)
		{}
	};

	//in a pattern every Variadic is preceeded by an element of this
	struct VariadicMetaData 
	{
		std::uint32_t match_data_idx = -1u; //indexes in MatchData::variadic_match_data

		//bit i dertermines whether parameter i is rematchable
		BitSet32 rematchable = -1u;
		//bit i determines whether parameter i is only matched with term directly succeding match of parameter i-1
		BitSet32 directly_after_prev = 0u;
		//bit i determines whether parameter i is only matched with terms succeding match of parameter i-1
		BitSet32 always_after_prev = 0u; 
	};

	union PnUnion
	{
		Complex complex;
		PnIdxVector parameters; //all in Variadic and all in Fn
		VariadicMetaData variadic_data;
		CharVector characters;
		ValueMatchVariable value_match;  //only expected as part of pattern

		constexpr PnUnion(const Complex&            val) noexcept :complex(val)       {}
		constexpr PnUnion(const PnIdxVector&        val) noexcept :parameters(val)    {}
		constexpr PnUnion(const VariadicMetaData&   val) noexcept :variadic_data(val) {}
		constexpr PnUnion(const CharVector&         val) noexcept :characters(val)    {}
		constexpr PnUnion(const ValueMatchVariable& val) noexcept :value_match(val)   {}
		constexpr PnUnion()                              noexcept :complex(0.0)       {}

		constexpr auto operator<=>(const PnUnion&) const = default;

		constexpr operator const Complex&            () const noexcept { return this->complex;       }
		constexpr operator const PnIdxVector&        () const noexcept { return this->parameters;    }
		constexpr operator const VariadicMetaData&   () const noexcept { return this->variadic_data; }
		constexpr operator const CharVector&         () const noexcept { return this->characters;    }
		constexpr operator const ValueMatchVariable& () const noexcept { return this->value_match;   }

		constexpr operator Complex&            () noexcept { return this->complex;       }
		constexpr operator PnIdxVector&        () noexcept { return this->parameters;    }
		constexpr operator VariadicMetaData&   () noexcept { return this->variadic_data; }
		constexpr operator CharVector&         () noexcept { return this->characters;    }
		constexpr operator ValueMatchVariable& () noexcept { return this->value_match;   }
	};
	static_assert(sizeof(PnUnion) == sizeof(Complex));

	using PnStore = BasicMonotonicStore<PnUnion>;

	static_assert(StoreLike<PnStore>);

	using UnsavePnRef = BasicUnsaveRef<PnType, PnUnion>;
	using PnRef = BasicSaveRef<PnType, const PnStore>;
	using MutPnRef = BasicSaveRef<PnType, PnStore>;


	static_assert(Reference<UnsavePnRef>);
	static_assert(Reference<PnRef>);
	static_assert(Reference<MutPnRef>);



	constexpr const VariadicMetaData& variadic_meta_data(const UnsavePnRef ref) noexcept 
	{
		return *ref.raw_at(ref.index - 1u);
	}

	constexpr VariadicMetaData& variadic_meta_data(const MutPnRef ref) noexcept 
	{
		return ref.store->at(ref.index - 1u);
	}

	template<typename Parameters_T>
	constexpr std::size_t build_pattern_variadic(PnStore& dst_store, const Parameters_T& dst_parameters,
		const VariadicMetaData& new_meta_data)
	{
		const std::size_t alloc_capacity = PnIdxVector::smallest_fit_capacity(dst_parameters.size());
		const std::size_t vec_idx = dst_store.allocate_n(1u + PnIdxVector::_node_count(alloc_capacity)) + 1u;
		PnIdxVector::emplace(dst_store.at(vec_idx), dst_parameters, alloc_capacity);
		dst_store.at(vec_idx - 1u) = new_meta_data;
		return vec_idx;
	}



	bool meets_restriction(const UnsaveRef ref, const TreeMatchOwning restr);

	bool in_domain(const Complex& nr, const Domain domain);


	enum class Side { lhs, rhs }; //site to match against vs. side to copy from
	enum class Convert { all, basic }; //if basic, only TreeMatch and MultiMatch are converted

	//representation of RewriteRule without pattern specific elements
	// -> allows manipulation of rule with other rules
	//can not be used to match against, but RewriteRule can be build from this
	struct IntermediateRewriteRule
	{
		MathIdx lhs_head = MathIdx{};
		MathIdx rhs_head = MathIdx{};
		MathStore store = {}; //acts as both store for rhs and lhs

		MutRef lhs_mut_ref() noexcept { return MutRef(this->store, this->lhs_head); }
		MutRef rhs_mut_ref() noexcept { return MutRef(this->store, this->rhs_head); }
		Ref lhs_ref() const noexcept { return Ref(this->store, this->lhs_head); }
		Ref rhs_ref() const noexcept { return Ref(this->store, this->rhs_head); }

		IntermediateRewriteRule(std::string name, Convert convert = Convert::all);
		std::string to_string() const;
		std::string to_memory_layout() const;
		std::string lhs_tree(const std::size_t offset = 0u) const;
		std::string rhs_tree(const std::size_t offset = 0u) const;
	};

	//nonowning version of const RewriteRule
	struct RewriteRuleRef
	{
		PnIdx lhs_head;
		PnIdx rhs_head;
		const PnStore* store; 

		std::string to_string() const;
		PnRef lhs_ref() const noexcept { return PnRef(*this->store, this->lhs_head); }
		PnRef rhs_ref() const noexcept { return PnRef(*this->store, this->rhs_head); }
	};

	//this is the form needed for a rule to be applied, however once it is in this form, 
	//  it can not be manipulated further by other rules
	struct RewriteRule
	{
		PnIdx lhs_head;
		PnIdx rhs_head;
		PnStore store; //acts as both store for rhs and lhs

		//if convert == Convert::basic only TreeMatch and MultiParams are converted from their NamedFn form
		//  (enables to use rewrite rules to help building value match patterns)
		RewriteRule(std::string name, Convert convert = Convert::all);

		MutPnRef lhs_mut_ref() noexcept { return MutPnRef(this->store, this->lhs_head); }
		MutPnRef rhs_mut_ref() noexcept { return MutPnRef(this->store, this->rhs_head); }
		PnRef lhs_ref() const noexcept { return PnRef(this->store, this->lhs_head); }
		PnRef rhs_ref() const noexcept { return PnRef(this->store, this->rhs_head); }
		RewriteRuleRef ref() const noexcept { return RewriteRuleRef{ this->lhs_head, this->rhs_head, &this->store }; }
	};

	//algorithms specific to patterns
	namespace pn_tree {

		//mostly stripped down version of tree::combine_values_exact to find calculate SharedValueDatum.value
		// from the start_val taken out of matched term
		OptionalComplex eval_value_match(const UnsavePnRef ref, const Complex& start_val);

		//copies math_ref into dst_store but changes math representations of pattern specific nodes 
		//  to their final pattern versions
		PnIdx intermediate_to_pattern(const UnsaveRef math_ref, PnStore& dst_store, 
			const Side side, const Convert convert, const PnType parent_type);

		//alternative name: pattern_to_pattern
		PnIdx copy(const PnRef src_ref, PnStore& dst_store);

		std::strong_ordering compare(const UnsavePnRef fst, const UnsavePnRef snd);

		//sorts equivalent to tree::sort using pn_tree::compare
		void sort(const MutPnRef ref);

		//returns array with one element for each pattern variable.
		//if (result array at index i == k) then TreeMatch i is the k-th TreeMatch instance to be matched
		auto determine_match_order(const UnsavePnRef ref);

	} //namespace pn_tree

	namespace match {

		//all MatchVariables of same name in pattern (e.g. "a" in pattern "a*b+a" share the same SharedTreeDatum to know 
		//whitch actually matched, and if the name "a" is already matched, even if the current instance is not.
		struct SharedTreeDatum
		{
			MathIdx match_idx = MathIdx{}; //indexes in Term to simplify

			constexpr bool is_set() const noexcept
			{
				return  this->match_idx != MathIdx{};
			}
		};

		struct SharedValueDatum
		{
			Complex value = std::numeric_limits<double>::quiet_NaN();

			bool is_set() const noexcept
			{
				return !std::isnan(this->value.real());
			}
		};

		struct SharedVariadicDatum
		{
			//no sum or product in a pattern may have more summands / factors than max_pn_variadic_params_count many
			static constexpr std::size_t max_pn_variadic_params_count = 6u;

			using MatchPos_T = decltype(IndexVector::Info::size);

			//every element in pattern (except all MultiParams) has own entry which logs, 
			//  with which element in term to match it currently is associated with.
			std::array<MatchPos_T, max_pn_variadic_params_count> match_positions = {};

			MathIdx match_idx = MathIdx{}; //indexes in Term to simplify (the haystack)

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
			//maximal number of variadics allowed per pattern
			static constexpr std::size_t max_variadic_count = 4u;    //(max MultiParams count is same)

			std::array<SharedValueDatum, max_value_match_count> value_match_data = {};
			std::array<SharedTreeDatum, max_tree_match_count> tree_match_data = {};
			std::array<SharedVariadicDatum, max_variadic_count> variadic_data = {};

			constexpr auto& value_info(const ValueMatchVariable& var) noexcept 
				{ return this->value_match_data[var.match_data_idx]; }

			constexpr auto& value_info(const ValueMatchVariable& var) const noexcept 
				{ return this->value_match_data[var.match_data_idx]; }
		};

		//compares term starting at ref.index in ref.store with pattern starting at pn_ref.index in pn_ref.store
		//if match is succsessfull, match_data stores what pattern's match variables matched and true is returned.
		//if match was not succsessfull, match_data is NOT reset and false is returned
		bool permutation_equals(const pattern::UnsavePnRef pn_ref, const UnsaveRef ref, MatchData& match_data);

		//determines weather there is a way to match pn_ref in haystack_ref (thus pn_ref is assumed to part of a pattern)
		//pn_i is the index of the first element in pn_ref to be matched. 
		//if pn_i is not zero, it is assumed, that all previous elements in pn_ref are already matched.
		//the first haystack_k elements of haystack_ref will be skipped for the first match attemt.
		//it is assumed, that pn_ref and haystack_ref are both the same Comm type
		//returns true if a match was found
		bool find_matching_permutation(const pattern::UnsavePnRef pn_ref, const UnsaveRef haystack_ref,
			MatchData& match_data, std::uint32_t pn_i, std::uint32_t haystack_k);

		//expects pn_ref to already be matched to ref via match_data
		//if one exists, this function finds a different match of pn_ref in ref, appearing after the current one
		//  in all permutations
		bool subsequent_permutation_equals(const pattern::UnsavePnRef pn_ref, const UnsaveRef ref, MatchData& match_data);

		//copies pn_ref with match_data into dst_store, returns head of copied result.
		[[nodiscard]] MathIdx copy(const pattern::UnsavePnRef pn_ref, const MatchData& match_data,
			const MathStore& src_store, MathStore& dst_store);

		//the function will try to match the head of in with the head of ref and if so, replace the matched part with out.
		//if a transformation happened, Just the new subterm is returned to replace the TypedIdx 
		//  of the old subterm in its parent, else nothing.
		//if the result contains something, ref is no longer valid.
		[[nodiscard]] std::optional<MathIdx> match_and_replace(
			const pattern::UnsavePnRef in, const pattern::UnsavePnRef out, const MutRef ref);

		//tries to match in (in postoreder) against every subterm of ref and finally ref itself. if a deeper match was found, 
		// snd of return value is true. 
		//if fst of return value is valid, ref could be matched and got replaced by *fst of return value, 
		//  meaning ref is no longer valid and caller needs to replace ref with *return_value.first  
		[[nodiscard]] std::pair<std::optional<MathIdx>, bool> recursive_match_and_replace(
			const pattern::UnsavePnRef in, const pattern::UnsavePnRef out, const MutRef ref);

		//all rules in [start, stop) are applied until no matching rule is found. in between tree::establish_basic_order is called
		//the new head of ref is retuned
		[[nodiscard]] MathIdx apply_rule_range(const RewriteRule* const start, const RewriteRule* const stop, const MutRef ref);

	} //namespace match

} //namespace bmath::intern::pattern

namespace bmath::intern::fn {

	constexpr auto&      range(const pattern::UnsavePnRef ref) noexcept { return ref->parameters; }
	constexpr auto&      range(const pattern::PnRef ref      ) noexcept { return ref->parameters; }
	constexpr auto  save_range(const pattern::PnRef ref      ) noexcept { return ref.cast<pattern::PnIdxVector>(); }
	constexpr auto       range(const pattern::MutPnRef ref   ) noexcept { return ref.cast<pattern::PnIdxVector>(); }

	constexpr const CharVector& named_fn_name(const pattern::UnsavePnRef named_fn) noexcept
	{
		return static_cast<const CharVector&>(*named_fn.raw_at(named_fn_name_index(named_fn)));
	}

} //namespace bmath::intern::fn

namespace bmath::intern {

	//if one pattern may compare equal to a term multiple ways (e.g. sum or product), it has high generality.
	//there are 3 main levels of generality:
	//  - unique (value 1xxx): pattern only matches one exact term e.g. pattern "2 'pi'" 
	//                         (note: everything without pattern variables falls tecnically in this category)
	//  - low    (value 2xxx): pattern is recursive, but has strong operands order (e.g. all in Fn), 
	//                         thus matches unique on outhermost level, but may hold general operands
	//  - high   (value 2xxx and 3xxx): sums / products containing pattern variables can match not only multiple terms, 
	//                         but may also match specific terms in more than one way (also tree variables, duh)
	//the table only differentiates between types, however (as described above) the real generality of a given term may be lower, 
	//  than that of its outermost node listed here.
	//as the goal of this endavour is (mostly) to sort the most general summands / factors in a pattern to the end, 
	//  the sorting required for efficiently matching patterns may use this table, but has to check more.
	constexpr int generality(const pattern::PnType type) noexcept
	{
		using namespace pattern;
		struct TypeGenerality { PnType type; int generality; };
		constexpr auto type_generality_table = std::to_array<TypeGenerality>({
			{ Literal::complex      , 1000 },
			{ Literal::symbol       , 1001 },
			{ ValueMatch::owning    , 1002 }, //may match different subsets of complex numbers, but always requires an actual value to match against
			{ ValueMatch::non_owning, 1003 }, //dont care really where this sits, as it never ist used in matching anyway
			{ LambdaParam{}         , 1004 },
			//values 2xxx are not present, as that would require every item in Fn to be listed here (instead default_generality kicks in here)	
			//values 3xxx are not present, as that would require every item in Domain to be listed here
		});
		static_assert(std::is_sorted(type_generality_table.begin(), type_generality_table.end(),
			[](auto a, auto b) { return a.generality < b.generality; }));
		static_assert(static_cast<unsigned>(PnType::COUNT) == 
			type_generality_table.size() + static_cast<unsigned>(Function::COUNT) + static_cast<unsigned>(Proxy::COUNT),
			"all but elements in Function and Proxy need to be listed in table");

		if (type.is<Function>()) {
			return static_cast<unsigned>(type) + 2000;
		}
		else if (type.is<Proxy>()) {
			return static_cast<unsigned>(type) + 3000;
		}
		else {
			return find(type_generality_table, &TypeGenerality::type, type).generality;
		}
	}
} //namespace bmath::intern