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
		named_fn,
		COUNT
	};

	enum class Leaf
	{
		variable,
		complex,
		COUNT
	};

	//these are luped together, because they behave the same in most cases -> can be seperated easily from rest
	//behavior for every specific element in Fn is defined at only two places:
	//  1. function fn::eval specifies how (and if at all) to evaluate
	//  2. array fn::props_table specifies name and parameter count
	enum class Fn //short for Function (note that named_fn is not listed here, at it's behavior is more complicated)
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
		force,  //params[0] := argument forces evaluation even if it is unexact
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

	struct NamedFn
	{
		static constexpr std::size_t max_name_size = 12u; 

		std::uint32_t params_idx = 0; //points to TypedIdxSLC containing the parameters
		char name[max_name_size] = {}; //short name seems sufficient for internal purposes

		constexpr std::size_t name_size() const noexcept { return std::min(std::strlen(this->name), max_name_size); }
		constexpr std::string_view name_view() const noexcept { return { this->name, this->name_size() }; }
	};

	using Variable = StringSLC;
	using Complex = std::complex<double>;

	union TypesUnion
	{
	private:
		char unused; //allows to constexpr default construct union without initialisation
	public:
		FnParams<TypedIdx> fn_params;
		NamedFn named_fn;
		Complex complex;
		TypedIdxSLC index_slc; //representing NamedFn's extra parameters and Sum and Product 
		StringSLC string;	//Variable is a string

		constexpr TypesUnion(const FnParams<TypedIdx>& val) noexcept :fn_params(val)        {}
		constexpr TypesUnion(const NamedFn&            val) noexcept :named_fn(val)         {}
		constexpr TypesUnion(const Complex&            val) noexcept :complex(val)          {}
		constexpr TypesUnion(const TypedIdxSLC&        val) noexcept :index_slc(val)        {}
		constexpr TypesUnion(const StringSLC&          val) noexcept :string(val)           {} 
		constexpr TypesUnion()                              noexcept :unused(0)             {}

		constexpr auto operator<=>(const TypesUnion&) const = default;

		constexpr operator const FnParams<TypedIdx> &() const noexcept { return this->fn_params; }
		constexpr operator const NamedFn            &() const noexcept { return this->named_fn; }
		constexpr operator const Complex            &() const noexcept { return this->complex; }
		constexpr operator const TypedIdxSLC        &() const noexcept { return this->index_slc; }
		constexpr operator const StringSLC          &() const noexcept { return this->string; }

		constexpr operator FnParams<TypedIdx> &() noexcept { return this->fn_params; }
		constexpr operator NamedFn            &() noexcept { return this->named_fn; }
		constexpr operator Complex            &() noexcept { return this->complex; }
		constexpr operator TypedIdxSLC        &() noexcept { return this->index_slc; }
		constexpr operator StringSLC          &() noexcept { return this->string; }
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);
	using Store = BasicStore<TypesUnion>;


	using MutRef = BasicMutRef<TypesUnion, Type>;
	using Ref = BasicRef<TypesUnion, Type>;


	namespace pattern {

		enum class PnVar 
		{ 
			tree_match, 
			value_match,
			value_proxy, //not actual node in tree, just "end" indicator
			COUNT 
		};

		//represent not actual node in pattern tree, as all match info is stored in MultiMatchDatum in MatchData		
		enum class MultiVar { summands, factors, params, COUNT };


		using PnType = SumEnum<MultiVar, PnVar, Type>; //dont list all enums making up Type directly, to allow converion to Type

		//as the usual Term does not know special pattern elements, these constants serve as placeholders for
		//  the actual PnType instances to also allow them beeing used in templates compiled to both pattern and normal term.
		static constexpr unsigned _tree_match  = unsigned(PnType(PnVar::tree_match));
		static constexpr unsigned _value_match = unsigned(PnType(PnVar::value_match));
		static constexpr unsigned _value_proxy = unsigned(PnType(PnVar::value_proxy));
		static constexpr unsigned _summands    = unsigned(PnType(MultiVar::summands));
		static constexpr unsigned _factors     = unsigned(PnType(MultiVar::factors));
		static constexpr unsigned _params      = unsigned(PnType(MultiVar::params));

		using PnTypedIdx = BasicTypedIdx<PnType>;
		using PnTypedIdxSLC = TermSLC<PnTypedIdx>;

		using PnSum = PnTypedIdxSLC;
		using PnProduct = PnTypedIdxSLC;

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
		using Restriction = SumEnum<Restr, Type>; 

		bool meets_restriction(const Ref ref, const Restriction restr);

		//in a valid pattern, all TreeMatchVariables of same name share the same restr and the same match_data_idx.
		//it is allowed to have multiple instances of the same TreeMatchVariable per side.
		//this variation of the match variable can match a subtree occuring in a term
		struct TreeMatchVariable
		{
			std::uint32_t match_data_idx; //indexes in MatchData::tree_match_data
			Restriction restr = Restr::any;
		};

		//although the type MultiMatchVariable does not exist as type of node in term, oder nodes may reference it
		// (now called PnVariable::summands or PnVariable::factors)
		//Meaning: if some PnTypedIdx has .get_type() == PnVariable::summands, all data of this node are stored in MatchData at
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

		bool has_form(const Complex& nr, const Form form);

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
		//in praxis a PnTypedIdx with .get_type() == PnVariable::value_proxy works similar to (imagined) MultiMatchVariable, as
		//  .get_index() also does not point in store of pattern, but in an array of MatchData.
		struct ValueMatchVariable
		{
			PnTypedIdx match_idx;
			PnTypedIdx copy_idx;
			std::uint32_t match_data_idx; //indexes in MatchData::value_match_data
			Form form = Form::real;

			ValueMatchVariable(std::uint32_t new_match_data_idx, Form new_form)
				:match_idx(PnTypedIdx(new_match_data_idx, PnVar::value_proxy)),
				copy_idx(PnTypedIdx(new_match_data_idx, PnVar::value_proxy)),
				match_data_idx(new_match_data_idx), form(new_form)
			{}
		};

		union PnTypesUnion
		{
		private:
			char unused; //allows to constexpr default construct union without initialisation
		public:
			FnParams<PnTypedIdx> fn_params;
			NamedFn named_fn;
			Complex complex;
			PnTypedIdxSLC index_slc; //representing NamedFn's extra parameters and Sum and Product 
			StringSLC string;	//PnVariable is a string
			TreeMatchVariable tree_match;
			ValueMatchVariable value_match;

			constexpr PnTypesUnion(const FnParams<PnTypedIdx>& val) noexcept :fn_params(val)        {}
			constexpr PnTypesUnion(const NamedFn&              val) noexcept :named_fn(val)         {}
			constexpr PnTypesUnion(const Complex&              val) noexcept :complex(val)          {}
			constexpr PnTypesUnion(const PnTypedIdxSLC&        val) noexcept :index_slc(val)        {}
			constexpr PnTypesUnion(const StringSLC&            val) noexcept :string(val)           {} 
			constexpr PnTypesUnion(const TreeMatchVariable&    val) noexcept :tree_match(val)       {} 
			constexpr PnTypesUnion(const ValueMatchVariable&   val) noexcept :value_match(val)      {} 
			constexpr PnTypesUnion()                                noexcept :unused(0)             {} 

			constexpr auto operator<=>(const PnTypesUnion&) const = default;

			constexpr operator const FnParams<PnTypedIdx> &() const noexcept { return this->fn_params; }
			constexpr operator const NamedFn              &() const noexcept { return this->named_fn; }
			constexpr operator const Complex              &() const noexcept { return this->complex; }
			constexpr operator const PnTypedIdxSLC        &() const noexcept { return this->index_slc; }
			constexpr operator const StringSLC            &() const noexcept { return this->string; }
			constexpr operator const TreeMatchVariable    &() const noexcept { return this->tree_match; }
			constexpr operator const ValueMatchVariable   &() const noexcept { return this->value_match; }

			constexpr operator FnParams<PnTypedIdx> &() noexcept { return this->fn_params; }
			constexpr operator NamedFn              &() noexcept { return this->named_fn; }
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

			constexpr bool is_set() const noexcept
			{
				assert(equivalent(this->responsible != PnTypedIdx{}, this->match_idx != TypedIdx{}));
				return  this->responsible != PnTypedIdx{};
			}
		};

		struct SharedMultiDatum
		{
			StupidBufferVector<TypedIdx, 8> match_indices; //indexes in Term to simplify
		};

		struct SharedValueDatum
		{
			Complex value = 0.0;
			TypedIdx match_idx = TypedIdx{}; //indexes in Term to simplify (only usefull during rematch to only match later elements)
			PnTypedIdx responsible = PnTypedIdx{}; //the instance of ValueMatchVariable that was setting value

			constexpr bool is_set() const noexcept
			{
				assert(equivalent(this->value != 0.0, this->responsible != PnTypedIdx{}, this->match_idx != TypedIdx{}));
				return  this->responsible != PnTypedIdx{};
			}
		};

		//to allow a constant PnTerm to be matched against, all match info is stored here
		struct MatchData
		{
			static constexpr std::size_t max_value_match_count = 2u; //maximal number of unrelated ValueMatchVariables allowed per pattern
			static constexpr std::size_t max_tree_match_count = 4u;	 //maximal number of unrelated TreeMatchVariables allowed per pattern
			static constexpr std::size_t max_multi_match_count = 2u; //maximal number of unrelated MultiMatchVariables allowed per pattern

			std::array<SharedValueDatum, max_value_match_count> value_match_data = {};
			std::array<SharedTreeDatum, max_tree_match_count> tree_match_data = {};
			std::array<SharedMultiDatum, max_multi_match_count> multi_match_data = {};

			constexpr auto& info(const TreeMatchVariable& var) noexcept { return this->tree_match_data[var.match_data_idx]; }
			constexpr auto& info(const ValueMatchVariable& var) noexcept { return this->value_match_data[var.match_data_idx]; }
			constexpr auto& info(const TreeMatchVariable& var) const noexcept { return this->tree_match_data[var.match_data_idx]; }
			constexpr auto& info(const ValueMatchVariable& var) const noexcept { return this->value_match_data[var.match_data_idx]; }

			constexpr auto& multi_info(const std::uint32_t idx) noexcept { return this->multi_match_data[idx]; }
			constexpr auto& multi_info(const std::uint32_t idx) const noexcept { return this->multi_match_data[idx]; }
		};

		struct PnTerm
		{
			PnTypedIdx lhs_head;
			PnTypedIdx rhs_head;
			PnStore lhs_store;
			PnStore rhs_store;

			PnTerm(std::string name);
			std::string to_string() const;
			std::string lhs_memory_layout() const;
			std::string rhs_memory_layout() const;
			std::string lhs_tree(const std::size_t offset = 0u) const;
			std::string rhs_tree(const std::size_t offset = 0u) const;

			PnMutRef lhs_mut_ref() noexcept { return PnMutRef(this->lhs_store, this->lhs_head); }
			PnMutRef rhs_mut_ref() noexcept { return PnMutRef(this->rhs_store, this->rhs_head); }
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

			//mostly stripped down version of tree::combine_values_exact to find calculate SharedValueDatum.value
			// from the start_val taken out of matched term
			OptComplex eval_value_match(const PnRef ref, const Complex& start_val);

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
			{ Fn::pow  , "pow"  , 2u },   
			{ Fn::log  , "log"  , 2u }, 	
			{ Fn::exp  , "exp"  , 1u },	
			{ Fn::sqrt , "sqrt" , 1u },
			{ Fn::asinh, "asinh", 1u },	
			{ Fn::acosh, "acosh", 1u },
			{ Fn::atanh, "atanh", 1u },	
			{ Fn::asin , "asin" , 1u },	
			{ Fn::acos , "acos" , 1u },	
			{ Fn::atan , "atan" , 1u },	
			{ Fn::sinh , "sinh" , 1u },	
			{ Fn::cosh , "cosh" , 1u },	
			{ Fn::tanh , "tanh" , 1u },	
			{ Fn::sin  , "sin"  , 1u },	
			{ Fn::cos  , "cos"  , 1u },	
			{ Fn::tan  , "tan"  , 1u },	
			{ Fn::abs  , "abs"  , 1u },	
			{ Fn::arg  , "arg"  , 1u },	
			{ Fn::ln   , "ln"   , 1u },	
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

		constexpr std::size_t param_count(const pattern::PnType type) noexcept 
		{ return props_table[static_cast<unsigned>(type.to<Fn>())].param_count; }



		template<typename TypedIdx_T, typename Type_T>
		constexpr std::span<TypedIdx_T> range(FnParams<TypedIdx_T>& params, const Type_T type) noexcept
		{ return { params.data(), param_count(type) }; }

		template<typename TypedIdx_T, typename Type_T>
		constexpr std::span<const TypedIdx_T> range(const FnParams<TypedIdx_T>& params, const Type_T type) noexcept
		{ return { params.data(), param_count(type) }; }


		template<Const is_const>
		constexpr auto range(const BasicRef<TypesUnion, Type, is_const> ref) noexcept 
		{ 
			assert(ref.type == Op::named_fn);
			return ref.new_as<TypedIdxSLC>(ref->named_fn.params_idx); 
		}

		template<Const is_const>
		constexpr auto range(const BasicRef<pattern::PnTypesUnion, pattern::PnType, is_const> ref) noexcept 
		{ 
			assert(ref.type == Op::named_fn);
			return ref.new_as<pattern::PnTypedIdxSLC>(ref->named_fn.params_idx); 
		}

	} //namespace fn

	//utility for variadic types (Sum and Product)
	namespace vc {

		inline auto range(const MutRef ref) noexcept { return ref.cast<TypedIdxSLC>(); }
		inline auto range(const    Ref ref) noexcept { return ref.cast<TypedIdxSLC>(); }

		inline auto range(const pattern::PnMutRef ref) noexcept { return ref.cast<pattern::PnTypedIdxSLC>(); }
		inline auto range(const    pattern::PnRef ref) noexcept { return ref.cast<pattern::PnTypedIdxSLC>(); }

	} //namespace vc

	//general purpose recursive tree traversal
	namespace tree {

		//removes subtree starting at ref from store
		template<typename Union_T, typename Type_T>
		void free(const BasicMutRef<Union_T, Type_T> ref);

		//flatten sums holding sums as summands and products holding products as factors
		//also eliminates unessecary indirections like sums with a single summand or products with a single factor
		//  to enable this second behavior, the index and type of ref may change, thus that is returned.
		template<typename Union_T, typename Type_T>
		[[nodiscard]] BasicTypedIdx<Type_T> combine_layers(const BasicMutRef<Union_T, Type_T> ref);

		//if a subtree can be fully evaluated, it will be, even if the result can not be stored exactly in 
		//floating point or the computation is unexact.
		//if a value is returned, the state of the subtree is unspecified but valid (from a storage perspective) 
		//and is expected to be deleted and replaced with the result.
		template<typename Union_T, typename Type_T>
		[[nodiscard]] OptComplex combine_values_inexact(const BasicMutRef<Union_T, Type_T> ref);

		//if evaluation of subtree was inexact / impossible, returns Nothing, else returns Just result.
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

		//counts number of logical nodes of subtree (note: logical nodes might be fewer than used slots in store)
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
		//only the non-const return value requires this function to not take a const store, 
		//  else handing in a reference as head might not be desired.
		template<typename Union_T, typename TypedIdx_T>
		TypedIdx_T* find_subtree_owner(BasicStore<Union_T>& store, TypedIdx_T& head, const TypedIdx_T subtree);

	} //namespace tree

	//algorithms to compare pattern to usual term and find match
	namespace match {

		//compares term starting at ref.index in ref.store with pattern starting at pn_ref.index in pn_ref.store
		//if match is succsessfull, match_data stores what pattern's match variables matched and true is returned.
		//if match was not succsessfull, match_data is NOT reset and false is returned
		bool equals(const pattern::PnRef pn_ref, const Ref ref, pattern::MatchData& match_data);

		struct PermutationEqualsRes
		{
			StupidBufferVector<TypedIdx, 12> matched;
			StupidBufferVector<TypedIdx, 12> not_matched;
		};

		//allows to match a sum / product pn_ref in a sum / product ref regardless of order
		//if no match was found, both PermutationEqualsRes.matched and PermutationEqualsRes.not_matched will be empty, 
		//  else PermutationEqualsRes.matched will contain the elements in term where a corrensponding part in pattern was found and
		//  PermutationEqualsRes.not_matched will contain the leftovers (-> empty if MultiVar paticipated).
		//it is assumed, that pn_ref and ref are both the same variadic type (eighter sum and sum or product and product)
		PermutationEqualsRes permutation_equals(const pattern::PnRef pn_ref, const Ref ref, pattern::MatchData& match_data);

		//copies pn_ref with match_data into store, returns head of copied result.
		[[nodiscard]] TypedIdx copy(const pattern::PnRef pn_ref, const pattern::MatchData& match_data, 
			const Store& src_store, Store& dst_store);

		//this function is the primary function designed to be called from outside of this namespace.
		//the function will try to match the head of in with the head of ref and if so, replace the matched part with out.
		//if a transformation happened, Just the new subterm is returned to replace the TypedIdx of the old subterm in its parent,
		//  else nothing.
		//if the result contains something, ref is no longer valid.
		[[nodiscard]] std::optional<TypedIdx> match_and_replace(const pattern::PnRef in, const pattern::PnRef out, const MutRef ref);

		//tries to match in (in postoreder) against every subterm of ref and finally ref itself. if a deeper match was found, 
		// snd of return value is true. 
		//if fst of return value is valid, ref could be matched and got replaced by *fst of return value, meaning ref is no longer valid
		// and caller needs to replace ref with *return_value.first  
		[[nodiscard]] std::pair<std::optional<TypedIdx>, bool> recursive_match_and_replace(
			const pattern::PnRef in, const pattern::PnRef out, const MutRef ref);

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
		struct ReturnEarlyPossible <T, std::void_t<decltype(std::declval<T>().return_early())>	> :std::true_type {};

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
		Term(std::string& name); //allows whitespace and implicit product
		Term(const std::string_view simple_name); //simple_name may not contain any whitespace

		void combine_layers() noexcept;
		void combine_values_inexact() noexcept;
		void combine_values_exact() noexcept;
		void sort() noexcept;
		void standardize() noexcept; 

		std::string to_memory_layout() const noexcept;
		std::string to_string() const noexcept;
		std::string to_pretty_string() noexcept; //will call standardize first
		std::string to_pretty_string() const noexcept; //assumes sorted term
		std::string to_tree() const noexcept;

		intern::MutRef mut_ref() noexcept;
		intern::Ref ref() const noexcept;
		bool match_and_replace(const intern::pattern::PnTerm& p) noexcept; //returns true if match was found 
	};	//class Term

}	//namespace bmath



