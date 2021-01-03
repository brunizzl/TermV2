#pragma once

#include <complex>
#include <optional>
#include <compare>
#include <string_view>
#include <array>

#include "utility/sumEnum.hpp"
#include "utility/misc.hpp"
#include "utility/vector.hpp"

#include "typedIndex.hpp"
#include "termStore.hpp"
#include "parseTerm.hpp"
#include "termVector.hpp"

namespace bmath::intern {

	enum class Variadic
	{
		sum,      //associative and commutative
		product,  //associative and commutative
		strict_sum,     //associative but not commutative
		strict_product, //associative but not commutative
		multiset,
		list,
		COUNT
	};

	//all functions known at compile time will not store their name with every instance in the store. this does.
	//the memory is sectioned in two parts: 
	//the index points at the IndexVector containing the parameters.
	//the function name follwos as CharVector in direct succession
	//(by starting with the parameters, most functions need no extra case to handle NamedFn)
	UNIT_ENUM(NamedFn);
	//using NamedFn = UnitEnum<"NamedFn">;

	//these are lumped together, because they behave the same in most cases -> can be seperated easily from rest
	//behavior for every specific element in Fn is (at least) defined at array fn::fn_props_table specifying name and arity
	// if the element in Fn is of order one (no functions as arguments / results), 
	//  function fn::eval specifies how to evaluate
	enum class Fn //most common version of Function with fixed arity and no commutivity
	{
		pow,    //params[0] := base      params[1] := expo    
		log,	//params[0] := base      params[1] := argument
		sqrt,	//params[0] := argument
		exp,	//params[0] := argument
		ln,		//params[0] := argument
		sin,	//params[0] := argument
		cos,	//params[0] := argument
		tan,	//params[0] := argument
		sinh,	//params[0] := argument
		cosh,	//params[0] := argument
		tanh,	//params[0] := argument
		asin,	//params[0] := argument
		acos,	//params[0] := argument
		atan,	//params[0] := argument
		asinh,	//params[0] := argument
		acosh,	//params[0] := argument
		atanh,	//params[0] := argument
		abs,	//params[0] := argument
		arg,	//params[0] := argument
		re,		//params[0] := argument
		im,		//params[0] := argument
		force,  //params[0] := argument   forces evaluation even if it is unexact
		diff,   //params[0] := function  params[1] := variable the derivation is done in respect to
		COUNT
	};

	using Function = SumEnum<Fn, NamedFn, Variadic>;

	//the only leaves in MathType
	enum class Literal
	{
		variable,
		complex,
		COUNT
	};

	using MathType = SumEnum<Literal, Function>;


	//not expeced to be present in normal Term, only in Patterns
	enum class PnNode 
	{ 
		tree_match,  //real nodes appearing as elements in the store, just as everything of MathType
		value_match, //real nodes appearing as elements in the store, just as everything of MathType
		value_proxy, //not actual node in tree, just "end" indicator for value subtrees
		COUNT 
	};

	//not expeced to be present in normal Term, only in Patterns
	//represent not actual nodes in pattern tree, as all match info is stored in VariadicMatchDatum in MatchData	
	//thus all info required in the tree is given in the typed_idx, where the index is repurposed to point elsewhere
	//(elsewhere means that VariadicMatchDatum array in MatchData)
	enum class MultiPn 
	{ 
		params, //only of MultiPn allowed in valid pattern on lhs (exchanged in RewriteRule's constructor)
		summands, //only expected at rhs of valid pattern, to allow a sum (of all summands) as factor
		factors,  //only expected at rhs of valid pattern, to allow a product (of all factors) as summand
		COUNT 
	};

	using MatchType = SumEnum<MultiPn, PnNode>;


	using Type = SumEnum<MatchType, MathType>;

	using TypedIdx = BasicTypedIdx<Type>;

	using IndexVector = StoredVector<TypedIdx>;
	using CharVector = StoredVector<char>;
	using Complex = std::complex<double>;

	namespace pattern {

		enum class Restr
		{
			any,
			nn1, //compact for "not negative one" (basically any, but the exact term "-1" will not be accepted)
			no_val, //basically any, but Literal::complex is forbidden    
			function, //packs Variadic, NamedFn and Fn together
			COUNT
		};

		//note: of Type, only sum, product, complex or variable may be used, as there is (currently)
		//  no need to differentiate between any of the functions of Fn and unknown_function.
		using Restriction = SumEnum<Restr, MathType>; 

		//in a valid pattern, all TreeMatchVariables of same name share the same restr and the same match_data_idx.
		//it is allowed to have multiple instances of the same TreeMatchVariable per side.
		//this variation of the match variable can match a subtree occuring in a term
		struct TreeMatchVariable
		{
			std::uint32_t match_data_idx; //indexes in MatchData::tree_match_data
			Restriction restr = pattern::Restr::any;
		};

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

		//specifies more constraints on a value
		enum class Form :std::uint32_t
		{
			natural,   //{1, 2, 3, ...}
			natural_0, //{0, 1, 2, ...}
			integer,
			real,
			complex,
			negative,     //implies real   
			positive,     //implies real     	
			not_negative, //implies real  
			not_positive, //implies real 
			COUNT
		};

		//in a valid pattern, all ValueMatchVariables of same name (although that is thrown away after building) 
		//share the same form and the same MatchData index
		//it is allowed to have multiple instances of the same ValueMatchVariable per side.
		//this variation of the match variable can match a value of specific form occuring in a term,
		//  the form of this value may also be specified using arithmetic operations. 
		//for example "2*k+1" with k beeing a ValueMatchVariable can match "5" and set value of SharedValueDatum to "2".
		//that is achieved by not actually storing "2*k+1" as such, but as "k", with match_idx of k containing the 
		//  inverse operation: "(p-1)/2", where "p" is not an actual node, but just a PnVariable::value_proxy 
		//  to indicate the original position of k.
		//to still allow to copy "2*k+1", the copy_idx of k contains a copy of the original suroundings of k, but again with a "p"
		//  in there: "2*p+1". this "p" is required to also store ValueMatchVariable::match_data_idx as it's index.
		//in praxis a TypedIdx with .get_type() == PnVariable::value_proxy works similar to (imagined) MultiMatchVariable, as
		//  .get_index() also does not point in store of pattern, but in an array of MatchData.
		struct ValueMatchVariable
		{
			TypedIdx mtch_idx;
			TypedIdx copy_idx;
			std::uint32_t match_data_idx; //indexes in MatchData::value_match_data
			Form form = Form::real;

			ValueMatchVariable(std::uint32_t new_match_data_idx, Form new_form)
				:mtch_idx(TypedIdx(new_match_data_idx, PnNode::value_proxy)),
				copy_idx(TypedIdx(new_match_data_idx, PnNode::value_proxy)),
				match_data_idx(new_match_data_idx), form(new_form)
			{}
		};

	} //namespace pattern



	union TypesUnion
	{
		Complex complex;
		IndexVector parameters; //all in Variadic and all in Fn
		CharVector char_vec;
		pattern::TreeMatchVariable tree_match;    //only expected as part of pattern
		pattern::ValueMatchVariable value_match;  //only expected as part of pattern

		constexpr TypesUnion(const Complex                    & val) noexcept :complex(val)     {}
		constexpr TypesUnion(const IndexVector                & val) noexcept :parameters(val)   {}
		constexpr TypesUnion(const CharVector                 & val) noexcept :char_vec(val)    {} 
		constexpr TypesUnion(const pattern::TreeMatchVariable & val) noexcept :tree_match(val)  {} 
		constexpr TypesUnion(const pattern::ValueMatchVariable& val) noexcept :value_match(val) {} 
		constexpr TypesUnion()                                       noexcept :complex(0.0)     {} 

		constexpr auto operator<=>(const TypesUnion&) const = default;

		constexpr operator const Complex                     &() const noexcept { return this->complex; }
		constexpr operator const IndexVector                 &() const noexcept { return this->parameters; }
		constexpr operator const CharVector                  &() const noexcept { return this->char_vec; }
		constexpr operator const pattern::TreeMatchVariable  &() const noexcept { return this->tree_match; }
		constexpr operator const pattern::ValueMatchVariable &() const noexcept { return this->value_match; }

		constexpr operator Complex                     &() noexcept { return this->complex; }
		constexpr operator IndexVector                 &() noexcept { return this->parameters; }
		constexpr operator CharVector                  &() noexcept { return this->char_vec; }
		constexpr operator pattern::TreeMatchVariable  &() noexcept { return this->tree_match; }
		constexpr operator pattern::ValueMatchVariable &() noexcept { return this->value_match; }
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);
	using Store = BasicStore<TypesUnion>;


	using MutRef = BasicMutRef<TypesUnion, Type>;
	using Ref = BasicRef<TypesUnion, Type>;


	//utility for NamedFn, types in Fn and types in Variadic
	namespace fn {

		struct FnProperties
		{
			Fn type = Fn::COUNT;
			std::string_view name = "";
			std::size_t arity = 0u; //number of arguments the function expects
		};

		//every item enumerated in Fn (except COUNT, duh) may be listed here in order of apperance in Fn
		constexpr auto fn_props_table = std::to_array<FnProperties>({
			{ Fn::pow  , "pow"  , 2u },   
			{ Fn::log  , "log"  , 2u }, 
			{ Fn::sqrt , "sqrt" , 1u },	
			{ Fn::exp  , "exp"  , 1u },	
			{ Fn::ln   , "ln"   , 1u },
			{ Fn::sin  , "sin"  , 1u },	
			{ Fn::cos  , "cos"  , 1u },	
			{ Fn::tan  , "tan"  , 1u },	
			{ Fn::sinh , "sinh" , 1u },	
			{ Fn::cosh , "cosh" , 1u },	
			{ Fn::tanh , "tanh" , 1u },	
			{ Fn::asin , "asin" , 1u },	
			{ Fn::acos , "acos" , 1u },	
			{ Fn::atan , "atan" , 1u },		
			{ Fn::asinh, "asinh", 1u },	
			{ Fn::acosh, "acosh", 1u },
			{ Fn::atanh, "atanh", 1u },	
			{ Fn::abs  , "abs"  , 1u },	
			{ Fn::arg  , "arg"  , 1u },	
			{ Fn::re   , "re"   , 1u },	
			{ Fn::im   , "im"   , 1u },		
			{ Fn::force, "force", 1u },
			{ Fn::diff , "diff" , 2u },   	
		});
		static_assert(static_cast<unsigned>(fn_props_table.front().type) == 0u);
		static_assert(std::is_sorted(fn_props_table.begin(), fn_props_table.end(), 
			[](auto lhs, auto rhs) { return lhs.type < rhs.type; }));
		static_assert(fn_props_table.size() == static_cast<unsigned>(Fn::COUNT));

		constexpr std::string_view name_of(const Fn type) noexcept 
		{ return fn_props_table[static_cast<unsigned>(type)].name; }

		//returns Fn::COUNT if name is not in fn_props_table
		constexpr Fn fn_type_of(const std::string_view name) noexcept 
		{ return search(fn_props_table, &FnProperties::name, name).type; }

		constexpr std::size_t arity(const Fn type) noexcept 
		{ return fn_props_table[static_cast<unsigned>(type)].arity; }

		constexpr std::size_t arity(const Type type) noexcept 
		{ return fn_props_table[static_cast<unsigned>(type.to<Fn>())].arity; }

		struct VariadicProperties
		{
			Variadic type = Variadic::COUNT;
			std::string_view name = "";

			//matching algorithm will try to also match permutations if true
			//-> advised to add entry to function generality() found in .cpp if so
			bool commutative = false; 

			bool associative = false; //allows to flatten nested instances if true
		};

		//every item enumerated in Variadic (except COUNT, duh) may be listed here in order of apperance in Variadic
		constexpr auto variadic_props_table = std::to_array<VariadicProperties>({
			{ Variadic::sum           , "sum"     , true , true  },
			{ Variadic::product       , "product" , true , true  },
			{ Variadic::strict_sum    , "sum'"    , false, true  },
			{ Variadic::strict_product, "product'", false, true  },
			{ Variadic::multiset      , "multiset", true , false },
			{ Variadic::list          , "list"    , false, false },
		});
		static_assert(static_cast<unsigned>(variadic_props_table.front().type) == 0u);
		static_assert(std::is_sorted(variadic_props_table.begin(), variadic_props_table.end(), 
			[](auto lhs, auto rhs) { return lhs.type < rhs.type; }));
		static_assert(variadic_props_table.size() == static_cast<unsigned>(Variadic::COUNT));

		constexpr std::string_view name_of(const Variadic type) noexcept 
		{ return variadic_props_table[static_cast<unsigned>(type)].name; }

		//returns Variadic::COUNT if name is not in variadic_props_table
		constexpr Variadic variadic_type_of(const std::string_view name) noexcept 
		{ return search(variadic_props_table, &VariadicProperties::name, name).type; }

		constexpr bool is_unordered(const Variadic type) noexcept 
		{ return variadic_props_table[static_cast<unsigned>(type)].commutative; }

		constexpr bool is_unordered(const Type type) noexcept 
		{ return is_unordered(type.to<Variadic>()); }

		constexpr bool is_associative(const Variadic type) noexcept 
		{ return variadic_props_table[static_cast<unsigned>(type)].associative; }

		constexpr bool is_associative(const Type type) noexcept 
		{ return is_associative(type.to<Variadic>()); }



		constexpr auto         range(const MutRef ref) noexcept { return ref.cast<IndexVector>(); }
		constexpr auto& unsave_range(const MutRef ref) noexcept { return ref->parameters; }
		constexpr auto&        range(const    Ref ref) noexcept { return ref->parameters; }
		constexpr auto    save_range(const    Ref ref) noexcept { return ref.cast<IndexVector>(); }



		constexpr std::uint32_t named_fn_name_index(const Ref named_fn) noexcept
		{
			const IndexVector& parameters = *named_fn;
			return named_fn.index + parameters.node_count();
		}

		constexpr const CharVector& named_fn_name(const Ref named_fn) noexcept
		{
			return static_cast<const CharVector&>(named_fn.store->at(named_fn_name_index(named_fn)));
		}

		constexpr CharVector& named_fn_name(const MutRef named_fn) noexcept
		{
			return static_cast<CharVector&>(named_fn.store->at(named_fn_name_index(named_fn)));
		}

		template<typename Store_T, typename Name_T, typename Parameters_T>
		inline TypedIdx build_named_fn(Store_T& store, const Name_T name, const Parameters_T& params)
		{
			const std::size_t parameter_capacity = IndexVector::smallest_fit_capacity(params.size());
			const std::size_t nr_parameter_nodes = IndexVector::_node_count(parameter_capacity);

			const std::size_t name_capacity = CharVector::smallest_fit_capacity(name.size());
			const std::size_t nr_name_nodes = CharVector::_node_count(name_capacity);

			const std::size_t result_index = store.allocate_n(nr_parameter_nodes + nr_name_nodes);
			IndexVector::emplace(store.at(result_index), params, parameter_capacity);
			CharVector::emplace(store.at(result_index + nr_parameter_nodes), name, name_capacity);
			return TypedIdx(result_index, Type(NamedFn{}));
		}

	} //namespace fn

	//general purpose recursive tree traversal
	namespace tree {

		//removes subtree starting at ref from store
		void free(const MutRef ref);

		//flattens sums holding sums as summands and products holding products as factors
		//evaluates parts that can be evaluated (if exact, only those, that can be exact evaluated)
		//eliminates unescesary indirections like sums with a single summand or products with a single factor
		//returns new location and type of ref
		[[nodiscard]] TypedIdx combine(const MutRef ref, const bool exact);

		//compares two subterms of perhaps different stores, assumes both to have their parameters parts sorted
		[[nodiscard]] std::strong_ordering compare(const Ref ref_1, const Ref ref_2);

		//sorts parameters parts by compare
		void sort(const MutRef ref);

		//counts number of logical nodes of subtree (note: logical nodes might be fewer than used slots in store)
		std::size_t count(const Ref ref);

		//copies subtree starting at src_ref into dst_store and returns its head
		[[nodiscard]] TypedIdx copy(const Ref src_ref, Store& dst_store);

		//returns true iff subtree starting at ref contains to_contain (or is to_contain itself)
		bool contains(const Ref ref, const TypedIdx to_contain);

		bool contains_variables(const Ref ref);

		//returns TypedIdx() if unsuccsessfull
		TypedIdx search_variable(const Ref ref, const std::string_view name);

		//calls first tree::combine, then tree::sort
		[[nodiscard]] TypedIdx establish_basic_order(MutRef ref);

		//returns pointer to field of parent of subtree, where subtree is held
		//only the non-const return value requires this function to not take a const store, 
		//  else handing in a reference as head might not be desired.
		TypedIdx* find_subtree_owner(Store& store, TypedIdx& head, const TypedIdx subtree);

		//expects ref to be head of term and ref.store to house ref exclusively 
		//(-> every position in store eighter free or used by (children of) ref exactly once)
		bool valid_storage(const Ref ref);

	} //namespace tree

	//algorithms to compare pattern to usual term and find match
	namespace pattern {

		bool meets_restriction(const Ref ref, const Restriction restr);

		bool has_form(const Complex& nr, const Form form);

		struct RewriteRule
		{
			TypedIdx lhs_head;
			TypedIdx rhs_head;
			Store lhs_store;
			Store rhs_store;

			RewriteRule(std::string name);
			std::string to_string() const;
			std::string lhs_memory_layout() const;
			std::string rhs_memory_layout() const;
			std::string lhs_tree(const std::size_t offset = 0u) const;
			std::string rhs_tree(const std::size_t offset = 0u) const;

			MutRef lhs_mut_ref() noexcept { return MutRef(this->lhs_store, this->lhs_head); }
			MutRef rhs_mut_ref() noexcept { return MutRef(this->rhs_store, this->rhs_head); }
			Ref lhs_ref() const noexcept { return Ref(this->lhs_store, this->lhs_head); }
			Ref rhs_ref() const noexcept { return Ref(this->rhs_store, this->rhs_head); }
		};

		//algorithms specific to patterns
		namespace pn_tree {

			//returns pointer to position in parent of value_match, where value_match is to be held in future
			// (currently value_match is only somewhere in the subtree that it will own, not nesseccarily at the root.
			//assumes head to be passed at reference to its own storage position
			TypedIdx* find_value_match_subtree(Store& store, TypedIdx& head, const TypedIdx value_match);

			//changes value_match from its state holding two value_proxy directly to actually
			//having copy_idx and match_idx initialized (thus value_match also bubbles up a bit in term)
			void rearrange_value_match(Store& store, TypedIdx& head, const TypedIdx value_match);

			struct Equation { TypedIdx lhs_head, rhs_head; };

			//reorders lhs and rhs until to_isolate is lhs_head, returns updated lhs_head and rhs_head
			//(possible other subtrees identical to to_isolate are not considered, thus the name prefix)
			//currently only used in rearrange_value_match, thus quite specialized
			[[nodiscard]] Equation stupid_solve_for(Store& store, Equation eq, const TypedIdx to_isolate);

			//mostly stripped down version of tree::combine_values_exact to find calculate SharedValueDatum.value
			// from the start_val taken out of matched term
			OptComplex eval_value_match(const Ref ref, const Complex& start_val);

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
				//indexes in Term to simplify (only usefull during rematch to only match later elements)
				TypedIdx match_idx = TypedIdx{}; 
				//the instance of ValueMatchVariable that was setting value_match (thus is also responsible for resetting)
				TypedIdx responsible = TypedIdx{};

				constexpr bool is_set() const noexcept
				{
					assert(equivalent(!std::isnan(this->value.real()), 
						this->responsible != TypedIdx{}, 
						this->match_idx != TypedIdx{}));
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
				//use with care: might check more then are actually contained in specific pattern!
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
				static constexpr std::size_t max_tree_match_count = 4u;	 
				//maximal number of sums and products allowed per pattern
				static constexpr std::size_t max_variadic_count = 4u;    //(max multi_match count is same)

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
			bool permutation_equals(const Ref pn_ref, const Ref ref, MatchData& match_data);

			//resets not all matched variables appearing in pn_ref, but only the ones also set by pn_ref
			//example: in whole pattern "a+a*b" "a" may be matched as single summand, 
			//  thus resetting own variables in part "a*b" will only reset "b".
			void reset_own_matches(const Ref pn_ref, MatchData& match_data);

			//if not all summands / factors in pattern could be matched, unmatchable is returned.
			//if not all summands / factors in the haystack are matched, matched_some is returned
			//(relevant if the current parameters is the outhermost, as then only a partial match may be successfull)
			enum class FindPermutationRes { matched_all, unmatchable, matched_some };

			//determines weather there is a way to match pn_ref in haystack_ref (thus pn_ref is assumed to part of a pattern)
			//pn_i is the index of the first element in pn_ref to be matched. 
			//if pn_i is not zero, it is assumed, that all previous elements in pn_ref are already matched.
			//the first haystack_k elements of haystack_ref will be skipped for the first match attemt.
			//it is assumed, that pn_ref and haystack_ref are both the same parameters type (eighter both sum or both product)
			FindPermutationRes find_matching_permutation(const Ref pn_ref, const Ref haystack_ref, 
				MatchData& match_data,	std::uint32_t pn_i, std::uint32_t haystack_k);

			//expects pn_ref to already be matched to ref via match_data
			//if one exists, this function finds a different match of pn_ref in ref, appearing after the current one
			//  in all permutations
			bool subsequent_permutation_equals(const Ref pn_ref, const Ref ref, MatchData& match_data);

			//copies pn_ref with match_data into dst_store, returns head of copied result.
			[[nodiscard]] TypedIdx copy(const Ref pn_ref, const MatchData& match_data, 
				const Store& src_store, Store& dst_store);

			//this function is the primary function designed to be called from outside of this namespace.
			//the function will try to match the head of in with the head of ref and if so, replace the matched part with out.
			//if a transformation happened, Just the new subterm is returned to replace the TypedIdx 
			//  of the old subterm in its parent, else nothing.
			//if the result contains something, ref is no longer valid.
			[[nodiscard]] std::optional<TypedIdx> match_and_replace(const Ref in, const Ref out, const MutRef ref);

			//tries to match in (in postoreder) against every subterm of ref and finally ref itself. if a deeper match was found, 
			// snd of return value is true. 
			//if fst of return value is valid, ref could be matched and got replaced by *fst of return value, 
			//  meaning ref is no longer valid and caller needs to replace ref with *return_value.first  
			[[nodiscard]] std::pair<std::optional<TypedIdx>, bool> recursive_match_and_replace(
				const Ref in, const Ref out, const MutRef ref);

		} //namespace match

	} //namespace pattern

	namespace fold {

		template<typename Wrapped_T>
		struct Find //the simple_fold evaluation stops early if cut is true and returns value (and cut)
		{
			Wrapped_T value;
			bool cut = false;

			constexpr bool return_early() const noexcept { return this->cut; }
			constexpr Wrapped_T& operator*() noexcept { return this->value; }
			constexpr const Wrapped_T& operator*() const noexcept { return this->value; }
		};
		template<typename Wrapped_T> constexpr Find<Wrapped_T> done(const Wrapped_T w) { return { w, true  }; }
		template<typename Wrapped_T> constexpr Find<Wrapped_T> more(const Wrapped_T w) { return { w, false }; }
		
		struct FindBool //the simple_fold evaluation stops early if cut is true and returns true
		{
			bool cut = false;

			constexpr bool return_early() const noexcept { return this->cut; }
			constexpr operator bool() const noexcept { return this->cut; }
			constexpr FindBool(bool init) :cut(init) {}
			constexpr FindBool() = default;
		};


		template<typename T, typename = void> 
		struct ReturnEarlyPossible :std::false_type {};

		template<typename T> 
		struct ReturnEarlyPossible <T, std::void_t<decltype(std::declval<T>().return_early())>> :std::true_type {};

		static_assert(ReturnEarlyPossible<Find<TypedIdx>>::value);
		static_assert(ReturnEarlyPossible<FindBool>::value);
		static_assert(!ReturnEarlyPossible<bool>::value);


		struct Void {}; //used if there is nothing to be returned from simple_fold

		//calls apply with every node (postorder), parameter is (BasicRef<Union_T, Type_T, is_const> ref), apply returns Res_T
		//Res_T might have nonstatic member return_early, to indicate if the fold may be stopped early, as the result is already known
		//not really a fold function in the classical sense, as there is no information accumulated - 
		//  eighter you have the final result or not. (or you only mutate the term and not return any result at all)
		template<typename Res_T, typename Union_T, typename Type_T, Const is_const, typename Apply>
		Res_T simple_fold(const BasicRef<Union_T, Type_T, is_const> ref, Apply apply);

		//this fold differentiates between recursive nodes (Variadic's, Fn's and ValueMatchVariable) and Leafes (values and variables)
		//OpAccumulator is constructed before a recursive call is made and consumes each recursive result. It thus needs to at least
		//  have a Constructor taking as arguments (BasicRef<Union_T, Type_T, Const> ref, AccInit... init) 
		//  and a consume method taking as single parameter (Res_T elem_res)
		//  a result method taking no parameters and returning Res_T
		//leaf_apply has parameters (BasicRef<Union_T, Type_T, Const> ref) and returns Res_T.		
		template<typename Res_T, typename OpAccumulator, typename Union_T, typename Type_T, Const is_const, 
			typename LeafApply, typename... AccInit>
		Res_T tree_fold(const BasicRef<Union_T, Type_T, is_const> ref, LeafApply leaf_apply, const AccInit... init);

	} //namespace fold

}	//namespace bmath::intern

namespace bmath {

	class Term
	{
	public:
		intern::Store store;
		intern::TypedIdx head;

		Term(std::string& name); //allows whitespace and implicit product
		Term(const std::string_view simple_name); //simple_name may not contain any whitespace

		void establish_order() noexcept; 

		std::string to_memory_layout() const noexcept;
		std::string to_string() const noexcept;
		std::string to_pretty_string() noexcept; //will call establish_order first
		std::string to_pretty_string() const noexcept; //assumes sorted term
		std::string to_tree() const noexcept;

		intern::MutRef mut_ref() noexcept;
		intern::Ref ref() const noexcept;
		bool match_and_replace(const intern::pattern::RewriteRule& p) noexcept; //returns true if match was found 
	};	//class Term

}	//namespace bmath



