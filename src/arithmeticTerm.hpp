#pragma once

#include <complex>
#include <span>
#include <optional>
#include <compare>
#include <string_view>

#include "typedIndex.hpp"
#include "termStore.hpp"
#include "parseTerm.hpp"
#include "termVector.hpp"

namespace bmath::intern {

	enum class Leaf
	{
		variable,
		complex,
		COUNT
	};

	enum class Variadic
	{
		sum,
		product,
		COUNT
	};

	//these are lumped together, because they behave the same in most cases -> can be seperated easily from rest
	//behavior for every specific element in Fn is (at least) defined at these places:
	//  1. function fn::eval specifies how (and if at all) to evaluate
	//  2. array fn::props_table specifies name and parameter count
	enum class Fn //short for Function
	{
		diff,   //params[0] := function  params[1] := variable the derivation is done in respect to
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
		COUNT
	};

	using MathType = SumEnum<Fn, Leaf, Variadic>;



	//not expeced to be present in normal Term, only in Patterns
	enum class PnNode 
	{ 
		tree_match,  //real nodes appearing as elements in the store, just as everything of MathType
		value_match, //real nodes appearing as elements in the store, just as everything of MathType
		value_proxy, //not actual node in tree, just "end" indicator for value subtrees
		COUNT 
	};

	//not expeced to be present in normal Term, only in Patterns
	//represent not actual nodes in pattern tree, as all match info is stored in MultiMatchDatum in MatchData	
	//thus all info required in the tree is given in the typed_idx, where the index is repurposed to point elsewhere
	enum class MultiPn 
	{ 
		summands, 
		factors, 
		params, 
		COUNT 
	};

	using MatchType = SumEnum<MultiPn, PnNode>;


	using Type = SumEnum<MatchType, MathType>;

	using TypedIdx = BasicTypedIdx<Type>;

	using FnParams = std::array<TypedIdx, 4>;
	using VariadicParams = StoredVector<TypedIdx>;
	using Variable = StoredVector<char>;
	using Complex = std::complex<double>;

	namespace pattern {

		enum class Restr
		{
			any,
			nn1, //compact for "not negative one" (basically any, but the exact term "-1" will not be accepted)
			no_val, //basically any, but Leaf::complex is forbidden    
			function, //packs named_fn and all in Fn together
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
		//as match::equals takes a Ref, not a MutRef however, this approach can not be realized here (and is stupid anyway)
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
		FnParams fn_params;
		Complex complex;
		VariadicParams variadic; //Sum and Product 
		Variable variable;
		pattern::TreeMatchVariable tree_match;    //only expected as part of pattern
		pattern::ValueMatchVariable value_match;  //only expected as part of pattern

		constexpr TypesUnion(const FnParams                   & val) noexcept :fn_params(val)   {}
		constexpr TypesUnion(const Complex                    & val) noexcept :complex(val)     {}
		constexpr TypesUnion(const VariadicParams             & val) noexcept :variadic(val)    {}
		constexpr TypesUnion(const Variable                   & val) noexcept :variable(val)    {} 
		constexpr TypesUnion(const pattern::TreeMatchVariable & val) noexcept :tree_match(val)  {} 
		constexpr TypesUnion(const pattern::ValueMatchVariable& val) noexcept :value_match(val) {} 
		constexpr TypesUnion()                                       noexcept :complex(0.0)     {} 

		constexpr auto operator<=>(const TypesUnion&) const = default;

		constexpr operator const FnParams                    &() const noexcept { return this->fn_params; }
		constexpr operator const Complex                     &() const noexcept { return this->complex; }
		constexpr operator const VariadicParams              &() const noexcept { return this->variadic; }
		constexpr operator const Variable                    &() const noexcept { return this->variable; }
		constexpr operator const pattern::TreeMatchVariable  &() const noexcept { return this->tree_match; }
		constexpr operator const pattern::ValueMatchVariable &() const noexcept { return this->value_match; }

		constexpr operator FnParams                    &() noexcept { return this->fn_params; }
		constexpr operator Complex                     &() noexcept { return this->complex; }
		constexpr operator VariadicParams              &() noexcept { return this->variadic; }
		constexpr operator Variable                    &() noexcept { return this->variable; }
		constexpr operator pattern::TreeMatchVariable  &() noexcept { return this->tree_match; }
		constexpr operator pattern::ValueMatchVariable &() noexcept { return this->value_match; }
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);
	using Store = BasicStore<TypesUnion>;


	using MutRef = BasicMutRef<TypesUnion, Type>;
	using Ref = BasicRef<TypesUnion, Type>;


	namespace pattern {

		bool meets_restriction(const Ref ref, const Restriction restr);

		bool has_form(const Complex& nr, const Form form);

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

		struct SharedMultiDatum
		{
			StupidBufferVector<TypedIdx, 8> match_indices; //indexes in Term to simplify
		};

		struct SharedValueDatum
		{
			Complex value = 0.0;
			TypedIdx mtch_idx = TypedIdx{}; //indexes in Term to simplify (only usefull during rematch to only match later elements)
			TypedIdx responsible = TypedIdx{}; //the instance of ValueMatchVariable that was setting value_match

			constexpr bool is_set() const noexcept
			{
				assert(equivalent(this->value != 0.0, this->responsible != TypedIdx{}, this->mtch_idx != TypedIdx{}));
				return  this->responsible != TypedIdx{};
			}
		};

		struct SharedVariadicDatum
		{
			//no sum or product in a pattern may have more summands / factors than max_pn_variadic_size many
			static constexpr std::size_t max_pn_variadic_size = 8u;
			//if currenty_matched.test(i), then element i in term to match is currently matched by an element in pattern.
			BitVector currenty_matched = {}; 
			//every element in pattern (except all MultiPn) has own entry which logs, 
			//  with which element in term to match it currently is associated with.
			std::array<decltype(VariadicParams::size), max_pn_variadic_size> match_positions = {};
		};

		//to allow a constant PnTerm to be matched against, all match info is stored here
		struct MatchData
		{
			static constexpr std::size_t max_value_match_count = 2u; //maximal number of unrelated ValueMatchVariables allowed per pattern
			static constexpr std::size_t max_tree_match_count = 4u;	 //maximal number of unrelated TreeMatchVariables allowed per pattern
			static constexpr std::size_t max_multi_match_count = 2u; //maximal number of MultiMatchVariables allowed per pattern
			static constexpr std::size_t max_variadic_count = 3u;    //maximal number of sums and products allowed per pattern

			std::array<SharedValueDatum, max_value_match_count> value_match_data = {};
			std::array<SharedTreeDatum, max_tree_match_count> tree_match_data = {};
			std::array<SharedMultiDatum, max_multi_match_count> multi_match_data = {};
			StupidLinearMap<std::uint32_t, -1u, SharedVariadicDatum, max_variadic_count> variadic_data = {};

			constexpr auto& info(const TreeMatchVariable& var) noexcept { return this->tree_match_data[var.match_data_idx]; }
			constexpr auto& info(const ValueMatchVariable& var) noexcept { return this->value_match_data[var.match_data_idx]; }
			constexpr auto& info(const TreeMatchVariable& var) const noexcept { return this->tree_match_data[var.match_data_idx]; }
			constexpr auto& info(const ValueMatchVariable& var) const noexcept { return this->value_match_data[var.match_data_idx]; }

			constexpr auto& multi_info(const std::uint32_t idx) noexcept { return this->multi_match_data[idx]; }
			constexpr auto& multi_info(const std::uint32_t idx) const noexcept { return this->multi_match_data[idx]; }
		};

		struct PnTerm
		{
			TypedIdx lhs_head;
			TypedIdx rhs_head;
			Store lhs_store;
			Store rhs_store;

			PnTerm(std::string name);
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

	} //namespace pattern

	//utility for both NamedFn and the types in Fn 
	namespace fn {

		struct FnProps //short for Function Properties
		{
			Fn type = Fn::COUNT;
			std::string_view name = "";
			std::size_t param_count = 0u;
		};

		//every item enumerated in Fn (except COUNT) may be listed here in order of apperance in Fn
		constexpr auto props_table = std::to_array<FnProps>({
			{ Fn::diff , "diff" , 2u },   
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
		});
		static_assert(static_cast<unsigned>(props_table.front().type) == 0u);
		static_assert(std::is_sorted(props_table.begin(), props_table.end(), [](auto lhs, auto rhs) { return lhs.type < rhs.type; }));
		static_assert(props_table.size() == static_cast<unsigned>(Fn::COUNT));

		constexpr std::string_view name_of(const Fn type) noexcept { return props_table[static_cast<unsigned>(type)].name; }

		//returns Fn::COUNT if name is not in props_table
		constexpr Fn type_of(const std::string_view name) noexcept { return search(props_table, &FnProps::name, name).type; }

		constexpr std::size_t param_count(const Fn type) noexcept 
		{ return props_table[static_cast<unsigned>(type)].param_count; }

		constexpr std::size_t param_count(const Type type) noexcept 
		{ return props_table[static_cast<unsigned>(type.to<Fn>())].param_count; }



		constexpr std::span<const TypedIdx> range(const Ref ref) noexcept
		{ 
			return { ref->fn_params.data(), param_count(ref.type) }; 
		}

		constexpr std::span<TypedIdx> range(FnParams& params, const Type type) noexcept
		{ 
			return { params.data(), param_count(type) }; 
		}

	} //namespace fn

	//utility for variadic types (Sum and Product)
	namespace variadic {

		inline auto         range(const MutRef ref) noexcept { return ref.cast<VariadicParams>(); }
		inline auto& unsave_range(const MutRef ref) noexcept { return ref->variadic; }
		inline auto&        range(const    Ref ref) noexcept { return ref->variadic; }

	} //namespace variadic

	//general purpose recursive tree traversal
	namespace tree {

		//removes subtree starting at ref from store
		void free(const MutRef ref);

		//flattens sums holding sums as summands and products holding products as factors
		//evaluates parts that can be evaluated (if exact, only those, that can be exact evaluated)
		//eliminates unescesary indirections like sums with a single summand or products with a single factor
		//returns new location and type of ref
		[[nodiscard]] TypedIdx combine(const MutRef ref, const bool exact);

		//compares two subterms of perhaps different stores, assumes both to have their variadic parts sorted
		[[nodiscard]] std::strong_ordering compare(const Ref ref_1, const Ref ref_2);

		//sorts variadic parts by compare
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

	} //namespace tree

	//algorithms to compare pattern to usual term and find match
	namespace match {

		//compares term starting at ref.index in ref.store with pattern starting at pn_ref.index in pn_ref.store
		//if match is succsessfull, match_data stores what pattern's match variables matched and true is returned.
		//if match was not succsessfull, match_data is NOT reset and false is returned
		bool equals(const Ref pn_ref, const Ref ref, pattern::MatchData& match_data);

		enum class FindPermutationRes { matched_all, failed, matched_some };

		//determines weather there is a way to match pn_ref in haystack_ref (thus pn_ref is assumed to part of a pattern)
		//pn_i is the index of the first element in pn_ref to be matched. 
		//if pn_i is not zero, it is assumed, that all previous elements in pn_ref are already matched.
		//the first haystack_k elements of haystack_ref will be skipped for the first match attemt.
		//it is assumed, that pn_ref and haystack_ref are both the same variadic type (eighter both sum or both product)
		FindPermutationRes find_matching_permutation(const Ref pn_ref, const Ref haystack_ref, 
			pattern::MatchData& match_data,	std::uint32_t pn_i, std::uint32_t haystack_k);

		//copies pn_ref with match_data into store, returns head of copied result.
		[[nodiscard]] TypedIdx copy(const Ref pn_ref, const pattern::MatchData& match_data, 
			const Store& src_store, Store& dst_store);

		//this function is the primary function designed to be called from outside of this namespace.
		//the function will try to match the head of in with the head of ref and if so, replace the matched part with out.
		//if a transformation happened, Just the new subterm is returned to replace the TypedIdx of the old subterm in its parent,
		//  else nothing.
		//if the result contains something, ref is no longer valid.
		[[nodiscard]] std::optional<TypedIdx> match_and_replace(const Ref in, const Ref out, const MutRef ref);

		//tries to match in (in postoreder) against every subterm of ref and finally ref itself. if a deeper match was found, 
		// snd of return value is true. 
		//if fst of return value is valid, ref could be matched and got replaced by *fst of return value, meaning ref is no longer valid
		// and caller needs to replace ref with *return_value.first  
		[[nodiscard]] std::pair<std::optional<TypedIdx>, bool> recursive_match_and_replace(
			const Ref in, const Ref out, const MutRef ref);

	} //namespace match

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
		intern::Store store;
		intern::TypedIdx head;

	public:
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
		bool match_and_replace(const intern::pattern::PnTerm& p) noexcept; //returns true if match was found 
	};	//class Term

}	//namespace bmath



