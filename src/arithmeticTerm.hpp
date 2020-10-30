#pragma once

#include <complex>
#include <span>
#include <optional>
#include <compare>
#include <string_view>

#include "typedIndex.hpp"
#include "termStore.hpp"
#include "termColony.hpp"
#include "parseTerm.hpp"

namespace bmath::intern {

	enum class Op //Short for Operation (Fn's are operations as well, but organisation stuff)
	{
		sum,
		product,
		generic_fn,
		COUNT
	};

	enum class Leaf
	{
		variable,
		complex,
		COUNT
	};

	//these are luped together, because they behave the same in most cases -> can be seperated easily from rest
	enum class Fn //short for Function (note that generic_fn is not listed here, at it's behavior is more complicated)
	{
		pow,    //params[0] := base      params[1] := expo    
		log,	//params[0] := base      params[1] := argument
		exp,	//params[0] := argument
		sqrt,	//params[0] := argument
		asinh,	//params[0] := argument
		acosh,	//params[0] := argument
		atanh,	//params[0] := argument
		asin,	//params[0] := argument
		acos,	//params[0] := argument
		atan,	//params[0] := argument
		sinh,	//params[0] := argument
		cosh,	//params[0] := argument
		tanh,	//params[0] := argument
		sin,	//params[0] := argument
		cos,	//params[0] := argument
		tan,	//params[0] := argument
		abs,	//params[0] := argument
		arg,	//params[0] := argument
		ln,		//params[0] := argument
		re,		//params[0] := argument
		im,		//params[0] := argument
		COUNT
	};

	using Type = SumEnum<Fn, Leaf, Op>;

	using TypedIdx = BasicTypedIdx<Type>;
	using TypedIdxSLC = TermSLC<TypedIdx>;

	using Sum     = TypedIdxSLC;
	using Product = TypedIdxSLC;

	//if any buildin funtion exeeds a parameter count of 4, a more involved structure needs to replace this.
	template<typename TypedIdx_T>
	using FnParams = std::array<TypedIdx_T, 4>;

	struct GenericFn
	{
		static constexpr std::size_t short_name_max = 8 + 3; //plus '\0' at end, supplied by name_size

		//if name_size == NameSize::small short_name is used, but as if would be of length 11
		//this is perhaps undefined behavior, but im feeling unsave today >:)	
		union
		{
			char short_name[8] = "";
			std::uint32_t long_name_idx;	//points to StringSLC containing name (if active)
		};
	private:
		char short_name_extension[3] = ""; //just implementation detail, no one needs to see this
	public:
		//if small is active, it doubles in purpose as '\0' character to end the name
		enum class NameSize :char { small = '\0', longer } name_size = NameSize::small;

		std::uint32_t params_idx = 0; //points to TypedIdxSLC containing the parameters
	};

	using  Variable = StringSLC;
	using Complex = std::complex<double>;

	union TypesUnion
	{
		FnParams<TypedIdx> fn_params;
		GenericFn generic_fn;
		Complex complex;
		TypedIdxSLC index_slc; //representing GenericFn's extra parameters, Sum or Product 
		StringSLC string;	//Variable is a string and GenericFn may allocate additional string nodes

		TypesUnion(const FnParams<TypedIdx>& val) :fn_params(val)        {}
		TypesUnion(const GenericFn&    val) :generic_fn(val) {}
		TypesUnion(const Complex&            val) :complex(val)          {}
		TypesUnion(const TypedIdxSLC&        val) :index_slc(val)        {}
		TypesUnion(const StringSLC&          val) :string(val)           {} 

		constexpr auto operator<=>(const TypesUnion&) const = default;

		constexpr operator const FnParams<TypedIdx> &() const noexcept { return this->fn_params; }
		constexpr operator const GenericFn    &() const noexcept { return this->generic_fn; }
		constexpr operator const Complex            &() const noexcept { return this->complex; }
		constexpr operator const TypedIdxSLC        &() const noexcept { return this->index_slc; }
		constexpr operator const StringSLC          &() const noexcept { return this->string; }

		constexpr operator FnParams<TypedIdx> &() noexcept { return this->fn_params; }
		constexpr operator GenericFn    &() noexcept { return this->generic_fn; }
		constexpr operator Complex            &() noexcept { return this->complex; }
		constexpr operator TypedIdxSLC        &() noexcept { return this->index_slc; }
		constexpr operator StringSLC          &() noexcept { return this->string; }
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);
	using Store = BasicStore<TypesUnion>;


	using MutRef = BasicMutRef<TypesUnion, Type>;
	using Ref = BasicRef<TypesUnion, Type>;


	namespace pattern {

		enum class PnVariable 
		{ 
			tree_match, 
			value_match,
			value_proxy, //not actual node in tree, just "end" indicator
			COUNT 
		};

		using PnType = SumEnum<PnVariable, Type>; //dont list all enums making up Type directly, to allow converion to Type

		//as the usual Term does not know special pattern elements, these constants serve as placeholders
		//for the actual PnType instances to also allow them beeing used in templates compiled to both pattern and normal term.
		static constexpr unsigned _tree_match  = unsigned(PnType(PnVariable::tree_match));
		static constexpr unsigned _value_match = unsigned(PnType(PnVariable::value_match));
		static constexpr unsigned _value_proxy = unsigned(PnType(PnVariable::value_proxy));

		using PnTypedIdx = BasicTypedIdx<PnType>;
		using PnTypedIdxSLC = TermSLC<PnTypedIdx>;

		using PnSum = PnTypedIdxSLC;
		using PnProduct = PnTypedIdxSLC;

		enum class Restr
		{
			function, //packs generic_fn and any in Fn together
			any,    
			unknown, //used only as error value
			COUNT
		};

		//note: of Type, only sum, product, complex or variable may be used, as there is (currently)
		//no need to differentiate between any of the functions of Fn and unknown_function.
		using Restriction = SumEnum<Restr, Type>; 

		template<typename TypedIdx_T>
		bool meets_restriction(const TypedIdx_T head, const Restriction restr);

		//in a valid pattern, all TreeMatchVariables of same name share the same restr and the same match_data_idx.
		//it is allowed to have multiple instances of the same TreeMatchVariable per side.
		//this variation of the match variable can match a subtree occuring in a term
		struct TreeMatchVariable
		{
			std::uint32_t match_data_idx; //indexes in MatchData::tree_match_data
			Restriction restr = Restr::any;
		};

		//specifies more constraints on a value
		enum class Form :std::uint32_t
		{
			natural,   //{1, 2, 3, ...}
			natural_0, //{0, 1, 2, ...}
			integer,
			real,
			negative,     //implies real   
			positive,     //implies real     	
			not_negative, //implies real  
			not_positive, //implies real 
			not_minus_one,  
			COUNT
		};

		bool has_form(const Complex& nr, const Form form);

		//in a valid pattern, all ValueMatchVariables of same name (thrown away after building) 
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
		struct ValueMatchVariable
		{
			PnTypedIdx match_idx;
			PnTypedIdx copy_idx;
			std::uint32_t match_data_idx; //indexes in MatchData::value_match_data
			Form form = Form::real;

			ValueMatchVariable(std::uint32_t new_match_data_idx, Form new_form)
				:match_idx(PnTypedIdx(new_match_data_idx, PnVariable::value_proxy)),
				copy_idx(PnTypedIdx(new_match_data_idx, PnVariable::value_proxy)),
				match_data_idx(new_match_data_idx), form(new_form)
			{}
		};

		union PnTypesUnion
		{
			FnParams<PnTypedIdx> fn_params;
			GenericFn generic_fn;
			Complex complex;
			PnTypedIdxSLC index_slc; //representing GenericFn's extra parameters, Sum or Product 
			StringSLC string;	//PnVariable is a string and GenericFn may allocate additional string nodes
			TreeMatchVariable tree_match;
			ValueMatchVariable value_match;

			PnTypesUnion(const FnParams<PnTypedIdx>& val) :fn_params(val)        {}
			PnTypesUnion(const GenericFn&      val) :generic_fn(val) {}
			PnTypesUnion(const Complex&              val) :complex(val)          {}
			PnTypesUnion(const PnTypedIdxSLC&        val) :index_slc(val)        {}
			PnTypesUnion(const StringSLC&            val) :string(val)           {} 
			PnTypesUnion(const TreeMatchVariable&    val) :tree_match(val)       {} 
			PnTypesUnion(const ValueMatchVariable&   val) :value_match(val)      {} 

			constexpr auto operator<=>(const PnTypesUnion&) const = default;

			constexpr operator const FnParams<PnTypedIdx> &() const noexcept { return this->fn_params; }
			constexpr operator const GenericFn      &() const noexcept { return this->generic_fn; }
			constexpr operator const Complex              &() const noexcept { return this->complex; }
			constexpr operator const PnTypedIdxSLC        &() const noexcept { return this->index_slc; }
			constexpr operator const StringSLC            &() const noexcept { return this->string; }
			constexpr operator const TreeMatchVariable    &() const noexcept { return this->tree_match; }
			constexpr operator const ValueMatchVariable   &() const noexcept { return this->value_match; }

			constexpr operator FnParams<PnTypedIdx> &() noexcept { return this->fn_params; }
			constexpr operator GenericFn      &() noexcept { return this->generic_fn; }
			constexpr operator Complex              &() noexcept { return this->complex; }
			constexpr operator PnTypedIdxSLC        &() noexcept { return this->index_slc; }
			constexpr operator StringSLC            &() noexcept { return this->string; }
			constexpr operator TreeMatchVariable    &() noexcept { return this->tree_match; }
			constexpr operator ValueMatchVariable   &() noexcept { return this->value_match; }
		};
		static_assert(sizeof(PnTypesUnion) * 8 == 128);

		using PnStore = BasicStore<PnTypesUnion>;

		using PnMutRef = BasicMutRef<PnTypesUnion, PnType>;
		using PnRef = BasicRef<PnTypesUnion, PnType>;

		//all MatchVariables of same name in pattern (e.g. "a" in pattern "a*b+a" share the same SharedTreeDatum to know 
		//whitch actually matched, and if the name "a" is already matched, even if the current instance is not.
		struct SharedTreeDatum
		{
			TypedIdx match_idx = TypedIdx{}; //indexes in Term to simplify
			PnTypedIdx responsible = PnTypedIdx{}; //the instance of TreeMatchVariable that was setting match_idx
		};

		struct SharedValueDatum
		{
			double value = {};
			PnTypedIdx responsible = PnTypedIdx{}; //the instance of ValueMatchVariable that was setting value
		};

		//to allow a constant PnTerm to be matched against, all match info is stored here
		struct MatchData
		{
			static constexpr std::size_t max_value_match_count = 4u; //maximal number of unrelated ValueMatchVariables allowed per pattern
			static constexpr std::size_t max_tree_match_count = 8u;	 //maximal number of unrelated TreeMatchVariables allowed per pattern

			std::array<SharedValueDatum, max_value_match_count> value_match_data;
			std::array<SharedTreeDatum, max_tree_match_count> tree_match_data;
		};

		struct PnTerm
		{
			PnTypedIdx lhs_head;
			PnTypedIdx rhs_head;
			PnStore lhs_store;
			PnStore rhs_store;

			PnTerm(std::string& name);
			std::string to_string() const;
			std::string lhs_memory_layout() const;
			std::string rhs_memory_layout() const;

			PnMutRef lhs_ref() noexcept { return PnMutRef(this->lhs_store, this->lhs_head); }
			PnMutRef rhs_ref() noexcept { return PnMutRef(this->rhs_store, this->rhs_head); }
			PnRef lhs_ref() const noexcept { return PnRef(this->lhs_store, this->lhs_head); }
			PnRef rhs_ref() const noexcept { return PnRef(this->rhs_store, this->rhs_head); }
		};

		//algorithms specific to patterns
		namespace pn_tree {

			//returns pointer to position in parent of value_match, where value_match is to be held in future
			// (currently value_match is only somewhere in the subtree that it will own, not nesseccarily at the root.
			//assumes head to be passed at reference to its own storage position
			PnTypedIdx* find_value_match_subtree(PnStore& store, PnTypedIdx& head, const PnTypedIdx value_match);

			//changes value_match from its state holding two value_proxy directly to actually
			//having copy_idx and match_idx initialized (thus value_match also bubbles up a bit in term)
			void rearrange_value_match(PnStore& store, PnTypedIdx& head, const PnTypedIdx value_match);

			struct Equation { PnTypedIdx lhs_head, rhs_head; };

			//reorders lhs and rhs until to_isolate is lhs_head, returns updated lhs_head and rhs_head
			//(possible other subtrees identical to to_isolate are not considered, thus the name prefix)
			//currently only used in rearrange_value_match, thus quite specialized
			[[nodiscard]] Equation stupid_solve_for(PnStore& store, Equation eq, const PnTypedIdx to_isolate);

		} //namespace pn_tree

	} //namespace pattern

	//utility for both Function and GenericFn
	namespace fn {

		constexpr auto param_count_table = std::to_array<std::pair<Fn, std::size_t>>({
			{ Fn::asinh, 1 },	
			{ Fn::acosh, 1 },
			{ Fn::atanh, 1 },	
			{ Fn::asin , 1 },	
			{ Fn::acos , 1 },	
			{ Fn::atan , 1 },	
			{ Fn::sinh , 1 },	
			{ Fn::cosh , 1 },	
			{ Fn::tanh , 1 },	
			{ Fn::sqrt , 1 },	
			{ Fn::pow  , 2 }, //<- only for these two fuckers >:(  
			{ Fn::log  , 2 }, //<- 
			{ Fn::exp  , 1 },	
			{ Fn::sin  , 1 },	
			{ Fn::cos  , 1 },	
			{ Fn::tan  , 1 },	
			{ Fn::abs  , 1 },	
			{ Fn::arg  , 1 },	
			{ Fn::ln   , 1 },	
			{ Fn::re   , 1 },	
			{ Fn::im   , 1 },	
		});
		constexpr std::size_t param_count(Fn type) noexcept 
		{ return find_snd(param_count_table, type); }

		constexpr std::size_t param_count(pattern::PnType type) noexcept 
		{ return find_snd(param_count_table, type.to<Fn>()); }


		template<typename TypedIdx_T, typename Type_T>
		std::span<TypedIdx_T> range(FnParams<TypedIdx_T>& params, const Type_T type) noexcept
		{ return { params.data(), param_count(type) }; }

		template<typename TypedIdx_T, typename Type_T>
		std::span<const TypedIdx_T> range(const FnParams<TypedIdx_T>& params, const Type_T type) noexcept
		{ return { params.data(), param_count(type) }; }


		inline auto range(MutRef ref) noexcept 
		{ 
			assert(ref.type == Op::generic_fn);
			return TypedIdxSLC::SLCMutRef<TypesUnion>(*ref.store, ref->generic_fn.params_idx); 
		}

		inline auto range(Ref ref) noexcept 
		{ 
			assert(ref.type == Op::generic_fn);
			return TypedIdxSLC::SLCRef<TypesUnion>(*ref.store, ref->generic_fn.params_idx); 
		}

		inline auto range(pattern::PnMutRef ref) noexcept 
		{ 
			assert(ref.type == Op::generic_fn);
			return pattern::PnTypedIdxSLC::SLCMutRef<pattern::PnTypesUnion>(*ref.store, ref->generic_fn.params_idx); 
		}

		inline auto range(pattern::PnRef ref) noexcept 
		{ 
			assert(ref.type == Op::generic_fn);
			return pattern::PnTypedIdxSLC::SLCRef<pattern::PnTypesUnion>(*ref.store, ref->generic_fn.params_idx); 
		}

	} //namespace fn

	//utility for variadic types (Sum and Product)
	namespace vc {

		inline auto range(MutRef ref) noexcept
		{ return TypedIdxSLC::SLCMutRef<TypesUnion>(*ref.store, ref.index); }

		inline auto range(Ref ref) noexcept 
		{ return TypedIdxSLC::SLCRef<TypesUnion>(*ref.store, ref.index); }

		inline auto range(pattern::PnMutRef ref) noexcept
		{ return pattern::PnTypedIdxSLC::SLCMutRef<pattern::PnTypesUnion>(*ref.store, ref.index); }

		inline auto range(pattern::PnRef ref) noexcept 
		{ return pattern::PnTypedIdxSLC::SLCRef<pattern::PnTypesUnion>(*ref.store, ref.index); }

	} //namespace vc

	//recursive tree traversal (some of these traversal functions are also found in namespace print)
	namespace tree {

		//removes subtree starting at ref from store
		template<typename Union_T, typename Type_T>
		void free(const BasicMutRef<Union_T, Type_T> ref);

		//flatten sums holding sums as summands and products holding products as factors
		//also eliminates unessecary indirections like sums with a single summand or products with a single factor
		//  to enable this second behavior, the index and type of ref may change, thus this is returned.
		template<typename Union_T, typename Type_T>
		[[nodiscard]] BasicTypedIdx<Type_T> combine_layers(const BasicMutRef<Union_T, Type_T> ref);

		//if a subtree can be fully evaluated, it will be, even if the result can not be stored exactly in 
		//floating point or the computation is unexact.
		//if a value is returned, the state of the subtree is unspecified but valid (from a storage perspective) 
		//and is expected to be deleted and replaced with the result.
		template<typename Union_T, typename Type_T>
		[[nodiscard]] OptComplex combine_values_inexact(const BasicMutRef<Union_T, Type_T> ref);

		//if evaluation of subtree was inexact / impossible, returns Complex(NAN, undefined), else returns result.
		//if an exact value is returned, the state of the subtree is unspecified but valid (from a storage perspective) 
		//and is expected to be deleted and replaced with the result. (equivalent behavior to combine_values_inexact)
		template<typename Union_T, typename Type_T>
		[[nodiscard]] OptComplex combine_values_exact(const BasicMutRef<Union_T, Type_T> ref);

		//compares two subterms of perhaps different stores, assumes both to have their variadic parts sorted
		template<typename Union_T1, typename Type_T1, typename Union_T2, typename Type_T2>
		[[nodiscard]] std::strong_ordering compare(const BasicRef<Union_T1, Type_T1> ref_1, const BasicRef<Union_T2, Type_T2> ref_2);

		//sorts variadic parts by compare
		template<typename Union_T, typename Type_T>
		void sort(const BasicMutRef<Union_T, Type_T> ref);

		//counts number of logical nodes of subtree
		template<typename Union_T, typename Type_T>
		std::size_t count(const BasicRef<Union_T, Type_T> ref);

		//copies subtree starting at src_ref into dst_store and returns its head
		template<typename Union_T, typename Type_T>
		[[nodiscard]] BasicTypedIdx<Type_T> copy(const BasicRef<Union_T, Type_T> src_ref, BasicStore<Union_T>& dst_store);

		//returns true iff subtree starting at ref contains to_contain (or is to_contain itself)
		template<typename Union_T, typename Type_T>
		bool contains(const BasicRef<Union_T, Type_T> ref, const BasicTypedIdx<Type_T> to_contain);

		bool contains_variables(const Ref ref);
		bool contains_variables(const pattern::PnRef ref);

		//returns TypedIdx() if unsuccsessfull
		TypedIdx search_variable(const Ref ref, const std::string_view name);

		//first combines layers, then combines values exact, then sorts
		//return value is new head
		template<typename Union_T, typename Type_T>
		[[nodiscard]] BasicTypedIdx<Type_T> establish_basic_order(BasicMutRef<Union_T, Type_T> ref);

		//returns pointer to field of parent of subtree, where subtree is held
		template<typename Union_T, typename TypedIdx_T>
		TypedIdx_T* find_subtree_owner(BasicStore<Union_T>& store, TypedIdx_T& head, const TypedIdx_T subtree);

		//compares term starting at head in store with pattern starting at pn_head in pn_store
		//if match is succsessfull, match_data stores what pattern's match variables matched and true is returned.
		//if match was not succsessfull, match_data is NOT reset and false is returned
		bool match(const pattern::PnRef pn_ref, const Ref ref, pattern::MatchData& match_data);

	} //namespace tree

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
		struct ReturnEarlyPossible <T, std::void_t<decltype(std::declval<T>().return_early())>	> :std::true_type {};

		static_assert(ReturnEarlyPossible<Find<TypedIdx>>::value);
		static_assert(ReturnEarlyPossible<FindBool>::value);
		static_assert(!ReturnEarlyPossible<bool>::value);


		struct Void {}; //used if there is nothing to be returned from simple_fold

		//calls apply with every node (postorder), parameter is (BasicRef<Union_T, Type_T, Const> ref), apply returns Res_T
		//Res_T might have nonstatic member return_early, to indicate if the fold may be stopped early, as the result is already known
		//not really a fold function in the classical sense, as there is no information accumulated. 
		//  eighter you have the final result or not. (or dont return a result at all)
		template<typename Res_T, typename Union_T, typename Type_T, Const is_const, typename Apply>
		Res_T simple_fold(const BasicRef<Union_T, Type_T, is_const> ref, Apply apply);

		//this fold differentiates between recursive nodes (Op's, Fn's and ValueMatchVariable) and Leafes (values and variables)
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
		Term(std::string& name);
		Term() = default;

		void combine_layers() noexcept;
		void combine_values_inexact() noexcept;
		void combine_values_exact() noexcept;
		void sort() noexcept;

		std::string to_memory_layout() const;
		std::string to_string() const;
		std::string to_pretty_string(); //will tidy up term first
		std::string to_pretty_string() const; //assumes sorted term

		intern::MutRef ref() noexcept;
		intern::Ref ref() const noexcept;
	};	//class Term

}	//namespace bmath



