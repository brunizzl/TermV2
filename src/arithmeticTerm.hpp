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

	enum class Type 
	{
		sum,
		product,
		known_function,
		generic_function,
		variable,
		complex,
		COUNT	//has to be last element
	};

	using TypedIdx = BasicTypedIdx<Type>;
	using TypedIdxSLC = TermSLC<std::uint32_t, TypedIdx, 3>;

	using Sum            = TypedIdxSLC;
	using Product        = TypedIdxSLC;

	enum class FnType :std::uint32_t
	{
		asinh,	//params[0] := argument
		acosh,	//params[0] := argument
		atanh,	//params[0] := argument
		asin,	//params[0] := argument
		acos,	//params[0] := argument
		atan,	//params[0] := argument
		sinh,	//params[0] := argument
		cosh,	//params[0] := argument
		tanh,	//params[0] := argument
		sqrt,	//params[0] := argument
		pow,    //params[0] := base      params[1] := expo    
		log,	//params[0] := base      params[1] := argument
		exp,	//params[0] := argument
		sin,	//params[0] := argument
		cos,	//params[0] := argument
		tan,	//params[0] := argument
		abs,	//params[0] := argument
		arg,	//params[0] := argument
		ln,		//params[0] := argument
		re,		//params[0] := argument
		im,		//params[0] := argument
		UNKNOWN //has to be last element, doubles as count
	};

	template<typename TypedIdx_T>
	struct BasicKnownFunction
	{
		FnType type;

		//if any buildin funtion exeeds a parameter count of 3, a more involved structure needs to replace this.
		TypedIdx_T params[3];
	};
	using KnownFunction = BasicKnownFunction<TypedIdx>;

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
		KnownFunction known_function;
		GenericFunction generic_function;
		Complex complex;
		TypedIdxSLC index_slc; //representing GenericFunction's extra parameters, Sum or Product 
		TermString128 string;	//Variable is a string and GenericFunction may allocate additional string nodes

		TypesUnion(const KnownFunction&   val) :known_function(val)   {}
		TypesUnion(const GenericFunction& val) :generic_function(val) {}
		TypesUnion(const Complex&         val) :complex(val)          {}
		TypesUnion(const TypedIdxSLC&     val) :index_slc(val)        {}
		TypesUnion(const TermString128&   val) :string(val)           {} 

		constexpr auto operator<=>(const TypesUnion&) const = default;

		template<typename T> constexpr const T& to() const noexcept;
		template<> constexpr const KnownFunction  &to<KnownFunction  >() const noexcept { return this->known_function; }
		template<> constexpr const GenericFunction&to<GenericFunction>() const noexcept { return this->generic_function; }
		template<> constexpr const Complex        &to<Complex        >() const noexcept { return this->complex; }
		template<> constexpr const TypedIdxSLC    &to<TypedIdxSLC    >() const noexcept { return this->index_slc; }
		template<> constexpr const TermString128  &to<TermString128  >() const noexcept { return this->string; }
		template<typename T> constexpr T& to() noexcept;
		template<> constexpr KnownFunction  &to<KnownFunction  >() noexcept { return this->known_function; }
		template<> constexpr GenericFunction&to<GenericFunction>() noexcept { return this->generic_function; }
		template<> constexpr Complex        &to<Complex        >() noexcept { return this->complex; }
		template<> constexpr TypedIdxSLC    &to<TypedIdxSLC    >() noexcept { return this->index_slc; }
		template<> constexpr TermString128  &to<TermString128  >() noexcept { return this->string; }
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);
	using Store = TermStore<TypesUnion>;

	namespace pattern {

		enum class PnVariable 
		{ 
			tree_match, 
			value_match,
			value_proxy, //doesn'd index in store of PnTerm, but in shared_value_match_data of PnTerm
			COUNT 
		};

		using PnType = SumEnum<PnVariable, Type>;
		static constexpr unsigned _tree_match  = unsigned(PnType(PnVariable::tree_match));
		static constexpr unsigned _value_match = unsigned(PnType(PnVariable::value_match));
		static constexpr unsigned _value_proxy = unsigned(PnType(PnVariable::value_proxy));

		using PnTypedIdx = BasicTypedIdx<PnType>;
		using PnTypedIdxSLC = TermSLC<std::uint32_t, PnTypedIdx, 3>;

		using PnSum = PnTypedIdxSLC;
		using PnProduct = PnTypedIdxSLC;
		using PnKnownFunction = BasicKnownFunction<PnTypedIdx>;

		enum class Restr //the rest
		{
			function, //packs both known and generic together
			any,    
			unknown, //used only as error value
			COUNT
		};

		//note: of Type, only sum, product, complex or variable may be used, as there is (currently)
		//no need to differentiate between known_function and unknown_function.
		using Restriction = SumEnum<Restr, Type>; 

		template<typename TypedIdx_T>
		bool meets_restriction(const TypedIdx_T ref, const Restriction restr);

		//in a valid pattern, all TreeMatchVariables of same name share the same restr and the same shared_data_idx.
		//it is allowed to have multiple instances of the same TreeMatchVariable per side.
		//this variation of the match variable can match a subtree occuring in a term
		struct TreeMatchVariable
		{
			std::uint32_t shared_data_idx; //points not in pattern tree, but extra vector called shared_tree_match_data.
			Restriction restr = Restr::any;
			std::array<char, 4u> name; //just convinience for debugging, not actually needed, thus this small and crappy
		};

		//specifies more constraints on a value
		enum class Form
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

		//in a valid pattern, all ValueMatchVariables of same name share the same form and the same shared_data_idx.
		//it is allowed to have multiple instances of the same ValueMatchVariable per side.
		//this variation of the match variable can match a value of specific form occuring in a term
		struct ValueMatchVariable
		{
			PnTypedIdx match_idx;
			PnTypedIdx copy_idx;
			Form form = Form::real;
			std::array<char, 4u> name; //just convinience for debugging, not actually needed, thus this small and crappy
		};

		union PnTypesUnion
		{
			PnKnownFunction known_function;
			GenericFunction generic_function;
			Complex complex;
			PnTypedIdxSLC index_slc; //representing GenericFunction's extra parameters, Sum or Product 
			TermString128 string;	//PnVariable is a string and GenericFunction may allocate additional string nodes
			TreeMatchVariable tree_match;
			ValueMatchVariable value_match;

			PnTypesUnion(const PnKnownFunction&    val) :known_function(val)   {}
			PnTypesUnion(const GenericFunction&    val) :generic_function(val) {}
			PnTypesUnion(const Complex&            val) :complex(val)          {}
			PnTypesUnion(const PnTypedIdxSLC&      val) :index_slc(val)        {}
			PnTypesUnion(const TermString128&      val) :string(val)           {} 
			PnTypesUnion(const TreeMatchVariable&  val) :tree_match(val)       {} 
			PnTypesUnion(const ValueMatchVariable& val) :value_match(val)      {} 

			constexpr auto operator<=>(const PnTypesUnion&) const = default;

			template<typename T> constexpr const T& to() const noexcept;
			template<> constexpr const PnKnownFunction   &to<PnKnownFunction   >() const noexcept { return this->known_function; }
			template<> constexpr const GenericFunction   &to<GenericFunction   >() const noexcept { return this->generic_function; }
			template<> constexpr const Complex           &to<Complex           >() const noexcept { return this->complex; }
			template<> constexpr const PnTypedIdxSLC     &to<PnTypedIdxSLC     >() const noexcept { return this->index_slc; }
			template<> constexpr const TermString128     &to<TermString128     >() const noexcept { return this->string; }
			template<> constexpr const TreeMatchVariable &to<TreeMatchVariable >() const noexcept { return this->tree_match; }
			template<> constexpr const ValueMatchVariable&to<ValueMatchVariable>() const noexcept { return this->value_match; }
			template<typename T> constexpr T& to() noexcept;
			template<> constexpr PnKnownFunction   &to<PnKnownFunction   >() noexcept { return this->known_function; }
			template<> constexpr GenericFunction   &to<GenericFunction   >() noexcept { return this->generic_function; }
			template<> constexpr Complex           &to<Complex           >() noexcept { return this->complex; }
			template<> constexpr PnTypedIdxSLC     &to<PnTypedIdxSLC     >() noexcept { return this->index_slc; }
			template<> constexpr TermString128     &to<TermString128     >() noexcept { return this->string; }
			template<> constexpr TreeMatchVariable &to<TreeMatchVariable >() noexcept { return this->tree_match; }
			template<> constexpr ValueMatchVariable&to<ValueMatchVariable>() noexcept { return this->value_match; }
		};
		static_assert(sizeof(PnTypesUnion) * 8 == 128);

		using PnStore = TermStore<PnTypesUnion>;

		//all MatchVariables of same name in pattern (e.g. "a" in pattern "a*b+a" share the same SharedTreeDatum to know 
		//whitch actually matched, and if the name "a" is already matched, even if the current instance is not.
		struct SharedTreeDatum
		{
			TypedIdx match_idx = TypedIdx{}; //indexes in ArithmeticTerm to simplify
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

	} //namespace pattern

	//utility for both KnownFunction and GenericFunction
	namespace fn {

		constexpr auto param_count_table = std::to_array<std::pair<FnType, std::size_t>>({
			{ FnType::asinh, 1 },	
			{ FnType::acosh, 1 },
			{ FnType::atanh, 1 },	
			{ FnType::asin , 1 },	
			{ FnType::acos , 1 },	
			{ FnType::atan , 1 },	
			{ FnType::sinh , 1 },	
			{ FnType::cosh , 1 },	
			{ FnType::tanh , 1 },	
			{ FnType::sqrt , 1 },	
			{ FnType::pow  , 2 }, //<- only for these two fuckers >:(  
			{ FnType::log  , 2 }, //<- 
			{ FnType::exp  , 1 },	
			{ FnType::sin  , 1 },	
			{ FnType::cos  , 1 },	
			{ FnType::tan  , 1 },	
			{ FnType::abs  , 1 },	
			{ FnType::arg  , 1 },	
			{ FnType::ln   , 1 },	
			{ FnType::re   , 1 },	
			{ FnType::im   , 1 },	
		});
		constexpr std::size_t param_count(FnType type) noexcept 
		{ return find_snd(param_count_table, type); }

		template<typename TypedIdx_T>
		std::span<TypedIdx_T> range(BasicKnownFunction<TypedIdx_T>& func) noexcept
		{ return { func.params, param_count(func.type) }; }

		template<typename TypedIdx_T>
		std::span<const TypedIdx_T> range(const BasicKnownFunction<TypedIdx_T>& func) noexcept
		{ return { func.params, param_count(func.type) }; }

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
	namespace vdc {

		inline auto range(Store& store, std::uint32_t vd_idx) noexcept
		{ return TypedIdxSLC::Range<Store>(store, vd_idx); }

		inline auto range(const Store& store, std::uint32_t vd_idx) noexcept 
		{ return TypedIdxSLC::Range<const Store>(store, vd_idx); }

		inline auto range(pattern::PnStore& store, std::uint32_t vd_idx) noexcept
		{ return pattern::PnTypedIdxSLC::Range<pattern::PnStore>(store, vd_idx); }

		inline auto range(const pattern::PnStore& store, std::uint32_t vd_idx) noexcept 
		{ return pattern::PnTypedIdxSLC::Range<const pattern::PnStore>(store, vd_idx); }

	} //namespace vdc

	//recursive tree traversal (some of these traversal functions are also found in namespace print)
	namespace tree {

		//removes subtree starting at ref from store
		template<typename Store_T, typename TypedIdx_T>
		void free(Store_T& store, const TypedIdx_T ref);

		//flatten sums holding als summands and products holding products as factors
		template<typename Store_T, typename TypedIdx_T>
		void combine_layers(Store_T& store, const TypedIdx_T ref);

		//if a subtree can be fully evaluated, it will be, even if the result can not be stored exactly in 
		//floating point/ the computation is unexact
		//the evaluated subtree also deletes itself, meaning the caller needs to reinsert the value,
		//  if a value was returned
		[[nodiscard]] std::optional<Complex> combine_values_inexact(Store& store, const TypedIdx ref);

		//if evaluation of subtree was inexact / impossible, returns Complex(NAN, undefined), else returns result.
		//the subtree starting at ref still remains. if deletion is desired, this has to be done by the caller.
		template<typename Store_T, typename TypedIdx_T>
		[[nodiscard]] Complex combine_values_exact(Store_T& store, const TypedIdx_T ref);

		//helper for combine_values_exact
		inline bool is_valid(const Complex c) { return !std::isnan(c.real()); };

		//compares two subterms of perhaps different stores, assumes both to have their variadic parts sorted
		template<typename Store_T1, typename Store_T2, typename TypedIdx_T1, typename TypedIdx_T2>
		[[nodiscard]] std::strong_ordering compare(const Store_T1& store_1, const Store_T2& store_2, 
			const TypedIdx_T1 ref_1, const TypedIdx_T2 ref_2);

		//sorts variadic parts by compare
		template<typename Store_T, typename TypedIdx_T>
		void sort(Store_T& store, const TypedIdx_T ref);

		template<typename TypedIdx_T>
		struct Equation
		{
			TypedIdx_T lhs_head;
			TypedIdx_T rhs_head;
		};

		//reorders lhs and rhs until to_isolate is lhs_head, 
		//(possible other subtrees identical to to_isolate are not considered, thus the name prefix)
		template<typename Store_T, typename TypedIdx_T>
		void stupid_solve_for(Store_T& store, Equation<TypedIdx_T>& equation, const TypedIdx_T to_isolate);

		//returns true iff subtree starting at ref contains to_contain
		template<typename Store_T, typename TypedIdx_T>
		bool contains(const Store_T& store, const TypedIdx_T ref, const TypedIdx_T to_contain);

		//if only a few types need actual attention and the rest only does the recursive call anyway,
		//this function handles the recursive calls
		enum class Order { pre, post };
		template<Order order, typename Store_T, typename TypedIdx_T, typename Apply>
		void for_each(Store_T& store, const TypedIdx_T ref, Apply apply);

		//returns TypedIdx() if unsuccsessfull
		TypedIdx search_variable(const Store& store, const TypedIdx head, std::string_view name);

	} //namespace tree

}	//namespace bmath::intern

namespace bmath {

	class ArithmeticTerm
	{
		intern::Store store;
		intern::TypedIdx head;

	public:
		ArithmeticTerm(std::string& name);
		ArithmeticTerm() = default;

		void combine_layers() noexcept;
		void combine_values_inexact() noexcept;
		void combine_values_exact() noexcept;
		void sort() noexcept;

		std::string to_memory_layout() const;
		std::string to_string() const;
		std::string to_pretty_string(); //will tidy up term first
		std::string to_pretty_string() const; //assumes sorted term
	};	//class ArithmeticTerm

}	//namespace bmath



