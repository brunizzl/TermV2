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
		generic_function,
		COUNT
	};

	enum class Leaf
	{
		variable,
		complex,
		COUNT
	};

	//these are luped together, because they behave the same in most cases -> can be seperated easily from rest
	enum class Fn //short for Function (note that generic_function is not listed here, at it's behavior is more complicated)
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
	using TypedIdxSLC = TermSLC<std::uint32_t, TypedIdx, 3>;

	using Sum     = TypedIdxSLC;
	using Product = TypedIdxSLC;

	//if any buildin funtion exeeds a parameter count of 4, a more involved structure needs to replace this.
	template<typename TypedIdx_T>
	using FnParams = std::array<TypedIdx_T, 4>;

	struct GenericFunction
	{
		static constexpr std::size_t short_name_max = 8 + 3; //plus '\0' at end, supplied by name_size

		//if name_size == NameSize::small short_name is used, but as if would be of length 11
		//this is perhaps undefined behavior, but im feeling unsave today >:)	
		union
		{
			char short_name[8] = "";
			std::uint32_t long_name_idx;	//points to TermString128 containing name (if active)
		};
	private:
		char short_name_extension[3] = ""; //just implementation detail, no one needs to see this
	public:
		//if small is active, it doubles in purpose as '\0' character to end the name
		enum class NameSize :char { small = '\0', longer } name_size = NameSize::small;

		std::uint32_t params_idx = 0; //points to TypedIdxSLC containing the parameters
	};

	using  Variable = TermString128;
	using Complex = std::complex<double>;

	union TypesUnion
	{
		FnParams<TypedIdx> fn_params;
		GenericFunction generic_function;
		Complex complex;
		TypedIdxSLC index_slc; //representing GenericFunction's extra parameters, Sum or Product 
		TermString128 string;	//Variable is a string and GenericFunction may allocate additional string nodes

		TypesUnion(const FnParams<TypedIdx>& val) :fn_params(val)        {}
		TypesUnion(const GenericFunction&    val) :generic_function(val) {}
		TypesUnion(const Complex&            val) :complex(val)          {}
		TypesUnion(const TypedIdxSLC&        val) :index_slc(val)        {}
		TypesUnion(const TermString128&      val) :string(val)           {} 

		constexpr auto operator<=>(const TypesUnion&) const = default;

		template<typename T> constexpr const T& to() const noexcept;
		template<> constexpr const FnParams<TypedIdx> &to<FnParams<TypedIdx>>() const noexcept { return this->fn_params; }
		template<> constexpr const GenericFunction    &to<GenericFunction   >() const noexcept { return this->generic_function; }
		template<> constexpr const Complex            &to<Complex           >() const noexcept { return this->complex; }
		template<> constexpr const TypedIdxSLC        &to<TypedIdxSLC       >() const noexcept { return this->index_slc; }
		template<> constexpr const TermString128      &to<TermString128     >() const noexcept { return this->string; }
		template<typename T> constexpr T& to() noexcept;
		template<> constexpr FnParams<TypedIdx> &to<FnParams<TypedIdx>>() noexcept { return this->fn_params; }
		template<> constexpr GenericFunction    &to<GenericFunction   >() noexcept { return this->generic_function; }
		template<> constexpr Complex            &to<Complex           >() noexcept { return this->complex; }
		template<> constexpr TypedIdxSLC        &to<TypedIdxSLC       >() noexcept { return this->index_slc; }
		template<> constexpr TermString128      &to<TermString128     >() noexcept { return this->string; }
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);
	using Store = TermStore<TypesUnion>;

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
		using PnTypedIdxSLC = TermSLC<std::uint32_t, PnTypedIdx, 3>;

		using PnSum = PnTypedIdxSLC;
		using PnProduct = PnTypedIdxSLC;

		enum class Restr
		{
			function, //packs generic_function and any in Fn together
			any,    
			unknown, //used only as error value
			COUNT
		};

		//note: of Type, only sum, product, complex or variable may be used, as there is (currently)
		//no need to differentiate between any of the functions of Fn and unknown_function.
		using Restriction = SumEnum<Restr, Type>; 

		template<typename TypedIdx_T>
		bool meets_restriction(const TypedIdx_T ref, const Restriction restr);

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

		//only conciders the forms relevant for an actual number
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
			GenericFunction generic_function;
			Complex complex;
			PnTypedIdxSLC index_slc; //representing GenericFunction's extra parameters, Sum or Product 
			TermString128 string;	//PnVariable is a string and GenericFunction may allocate additional string nodes
			TreeMatchVariable tree_match;
			ValueMatchVariable value_match;

			PnTypesUnion(const FnParams<PnTypedIdx>& val) :fn_params(val)        {}
			PnTypesUnion(const GenericFunction&      val) :generic_function(val) {}
			PnTypesUnion(const Complex&              val) :complex(val)          {}
			PnTypesUnion(const PnTypedIdxSLC&        val) :index_slc(val)        {}
			PnTypesUnion(const TermString128&        val) :string(val)           {} 
			PnTypesUnion(const TreeMatchVariable&    val) :tree_match(val)       {} 
			PnTypesUnion(const ValueMatchVariable&   val) :value_match(val)      {} 

			constexpr auto operator<=>(const PnTypesUnion&) const = default;

			template<typename T> constexpr const T& to() const noexcept;
			template<> constexpr const FnParams<PnTypedIdx> &to<FnParams<PnTypedIdx>>() const noexcept { return this->fn_params; }
			template<> constexpr const GenericFunction      &to<GenericFunction     >() const noexcept { return this->generic_function; }
			template<> constexpr const Complex              &to<Complex             >() const noexcept { return this->complex; }
			template<> constexpr const PnTypedIdxSLC        &to<PnTypedIdxSLC       >() const noexcept { return this->index_slc; }
			template<> constexpr const TermString128        &to<TermString128       >() const noexcept { return this->string; }
			template<> constexpr const TreeMatchVariable    &to<TreeMatchVariable   >() const noexcept { return this->tree_match; }
			template<> constexpr const ValueMatchVariable   &to<ValueMatchVariable  >() const noexcept { return this->value_match; }
			template<typename T> constexpr T& to() noexcept;
			template<> constexpr FnParams<PnTypedIdx> &to<FnParams<PnTypedIdx>>() noexcept { return this->fn_params; }
			template<> constexpr GenericFunction      &to<GenericFunction     >() noexcept { return this->generic_function; }
			template<> constexpr Complex              &to<Complex             >() noexcept { return this->complex; }
			template<> constexpr PnTypedIdxSLC        &to<PnTypedIdxSLC       >() noexcept { return this->index_slc; }
			template<> constexpr TermString128        &to<TermString128       >() noexcept { return this->string; }
			template<> constexpr TreeMatchVariable    &to<TreeMatchVariable   >() noexcept { return this->tree_match; }
			template<> constexpr ValueMatchVariable   &to<ValueMatchVariable  >() noexcept { return this->value_match; }
		};
		static_assert(sizeof(PnTypesUnion) * 8 == 128);

		using PnStore = TermStore<PnTypesUnion>;

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
		};

		//these functions provide utitity to change value_match from its state holding two value_proxy directly to actually
		//having copy_idx and match_idx initialized (thus value_match also bubbles up a bit in term)
		namespace build_value_match {

			//returns pointer to position in parent of value_match, where value_match is to be held in future
			// (currently value_match is at 
			//assumes head to be passed at reference to its own storage position
			PnTypedIdx* find_value_match_subtree(PnStore& store, PnTypedIdx& head, const PnTypedIdx value_match);

			void rearrange(PnStore& store, PnTypedIdx& head, const PnTypedIdx value_match);

			struct Equation { PnTypedIdx lhs_head, rhs_head; };

			//reorders lhs and rhs until to_isolate is lhs_head, returns updated lhs_head and rhs_head
			//(possible other subtrees identical to to_isolate are not considered, thus the name prefix)
			[[nodiscard]] Equation stupid_solve_for(PnStore& store, Equation eq, const PnTypedIdx to_isolate);

		} //namespace build_value_match

	} //namespace pattern

	//utility for both Function and GenericFunction
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

		inline auto range(Store& store, GenericFunction& func) noexcept 
		{ return TypedIdxSLC::Range<Store>(store, func.params_idx); }

		inline auto range(const Store& store, const GenericFunction& func) noexcept 
		{ return TypedIdxSLC::Range<const Store>(store, func.params_idx); }

		inline auto range(pattern::PnStore& store, GenericFunction& func) noexcept 
		{ return pattern::PnTypedIdxSLC::Range<pattern::PnStore>(store, func.params_idx); }

		inline auto range(const pattern::PnStore& store, const GenericFunction& func) noexcept 
		{ return pattern::PnTypedIdxSLC::Range<const pattern::PnStore>(store, func.params_idx); }

	} //namespace fn

	//utility for variadic types (Sum and Product)
	namespace vc {

		inline auto range(Store& store, std::uint32_t vd_idx) noexcept
		{ return TypedIdxSLC::Range<Store>(store, vd_idx); }

		inline auto range(const Store& store, std::uint32_t vd_idx) noexcept 
		{ return TypedIdxSLC::Range<const Store>(store, vd_idx); }

		inline auto range(pattern::PnStore& store, std::uint32_t vd_idx) noexcept
		{ return pattern::PnTypedIdxSLC::Range<pattern::PnStore>(store, vd_idx); }

		inline auto range(const pattern::PnStore& store, std::uint32_t vd_idx) noexcept 
		{ return pattern::PnTypedIdxSLC::Range<const pattern::PnStore>(store, vd_idx); }

	} //namespace vc

	//recursive tree traversal (some of these traversal functions are also found in namespace print)
	namespace tree {

		//removes subtree starting at ref from store
		template<typename Store_T, typename TypedIdx_T>
		void free(Store_T& store, const TypedIdx_T ref);

		//flatten sums holding als summands and products holding products as factors
		template<typename Store_T, typename TypedIdx_T>
		void combine_layers(Store_T& store, const TypedIdx_T ref);

		//if a subtree can be fully evaluated, it will be, even if the result can not be stored exactly in 
		//floating point or the computation is unexact.
		//if a value is returned, the state of the subtree is unspecified but valid (from a storage perspective) 
		//and is expected to be deleted and replaced with the result.
		template<typename Store_T, typename TypedIdx_T>
		[[nodiscard]] OptComplex combine_values_inexact(Store_T& store, const TypedIdx_T ref);

		//if evaluation of subtree was inexact / impossible, returns Complex(NAN, undefined), else returns result.
		//if an exact value is returned, the state of the subtree is unspecified but valid (from a storage perspective) 
		//and is expected to be deleted and replaced with the result. (equivalent behavior to combine_values_inexact)
		template<typename Store_T, typename TypedIdx_T>
		[[nodiscard]] OptComplex combine_values_exact(Store_T& store, const TypedIdx_T ref);

		//compares two subterms of perhaps different stores, assumes both to have their variadic parts sorted
		template<typename Store_T1, typename Store_T2, typename TypedIdx_T1, typename TypedIdx_T2>
		[[nodiscard]] std::strong_ordering compare(const Store_T1& store_1, const Store_T2& store_2, 
			const TypedIdx_T1 ref_1, const TypedIdx_T2 ref_2);

		//sorts variadic parts by compare
		template<typename Store_T, typename TypedIdx_T>
		void sort(Store_T& store, const TypedIdx_T ref);

		//counts number of logical nodes of subtree
		template<typename Store_T, typename TypedIdx_T>
		std::size_t count(Store_T& store, const TypedIdx_T ref);

		//copies subtree starting at src_ref into dst_store and returns its head
		template<typename DstTypedIdx_T, typename SrcStore_T, typename DstStore_T, typename SrcTypedIdx_T>
		[[nodiscard]] DstTypedIdx_T copy(const SrcStore_T& src_store, DstStore_T& dst_store, const SrcTypedIdx_T src_ref);

		//returns true iff subtree starting at ref contains to_contain (or is to_contain itself)
		template<typename Store_T, typename TypedIdx_T>
		bool contains(const Store_T& store, const TypedIdx_T ref, const TypedIdx_T to_contain);

		bool contains_variables(const Store& store, const TypedIdx ref);
		bool contains_variables(const pattern::PnStore& store, const pattern::PnTypedIdx ref);

		//returns TypedIdx() if unsuccsessfull
		TypedIdx search_variable(const Store& store, const TypedIdx head, std::string_view name);

		//first combines layers, then combines values exact, then sorts
		//return value is new head
		template<typename Store_T, typename TypedIdx_T>
		[[nodiscard]] TypedIdx_T establish_basic_order(Store_T& store, TypedIdx_T head);

		//returns pointer to field of parent of subtree, where subtree is held
		template<typename Store_T, typename TypedIdx_T>
		TypedIdx_T* find_subtree_owner(Store_T& store, TypedIdx_T* const head, const TypedIdx_T subtree);

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


		struct Void {}; //used, if there is nothing to be returned

		//calls apply with every node (postorder), parameters are (std::uint32_t index, Type_T type), apply returns Res_T
		//Res_T might have nonstatic member return_early, to indicate if the fold may be stopped early, as the result is already known
		template<typename Res_T, typename Store_T, typename TypedIdx_T, typename Apply>
		Res_T simple_fold(Store_T& store, const TypedIdx_T ref, Apply apply);

		//this fold differentiates between recursive nodes (Op's, Fn's and ValueMatchVariable) and Leafes (values and variables)
		//OpAccumulator is constructed before a recursive call is made and consumes each recursive result. It thus needs to at least
		//  have a Constructor taking as arguments (TypedIdx_T& ref, AccInit... init) 
		//  and a consume method taking as single parameter (Res_T elem_res)
		//  a result method taking no parameters and returning Res_T
		//leaf_apply has parameters (TypedIdx_T& ref) and returns Res_T.		
		template<typename Res_T, typename OpAccumulator, typename Store_T, typename TypedIdx_T, typename LeafApply, 
			typename... AccInit>
		Res_T tree_fold(Store_T& store, TypedIdx_T* ref, LeafApply leaf_apply, const AccInit... init);

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

		std::pair<intern::Store*, intern::TypedIdx> data() noexcept;
	};	//class Term

}	//namespace bmath



