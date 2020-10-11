#pragma once

#include <complex>
#include <span>
#include <optional>
#include <compare>

#include "typedIndex.hpp"
#include "termStore.hpp"
#include "termColony.hpp"
#include "parseTerm.hpp"

namespace bmath::intern {

	enum class Type :unsigned {
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

	enum class FnType : std::uint32_t
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
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);
	using Store = TermStore<TypesUnion>;

	namespace pattern {

		enum class PnSpecial { match_variable, COUNT };
		using PnType = SumEnum<PnSpecial, Type>;
		static constexpr unsigned _match_variable = unsigned(PnType(PnSpecial::match_variable));

		using PnTypedIdx = BasicTypedIdx<PnType>;
		using PnTypedIdxSLC = TermSLC<std::uint32_t, PnTypedIdx, 3>;

		using PnSum = PnTypedIdxSLC;
		using PnProduct = PnTypedIdxSLC;
		using PnKnownFunction = BasicKnownFunction<PnTypedIdx>;

		//used to restrict match_variable to only match terms complying with restr
		enum class Form
		{
			any,
			function,
			natural, //includes 0
			integer,
			real,
			negative,     //implies real   
			positive,     //implies real     	
			not_negative, //implies real  
			not_positive, //implies real 
			not_minus_one,       
			UNKNOWN	//has to be last element
		};

		//note: of Type, only sum, product, complex or variable may be used, as there is (currently)
		//no need to differentiate between known_function and unknown_function.
		using Restriction = SumEnum<Pair<Form, Form::UNKNOWN>, Type>;

		template<typename Store_T, typename TypedIdx_T>
		bool has_form(const Store_T& store, const TypedIdx_T ref, const Restriction restr);

		//in a valid pattern, all MatchVariables of same name share the same restr and the same shared_data_idx.
		struct MatchVariable
		{
			TypedIdx match_idx; //remembers what is is matched with (if so)
			std::uint32_t shared_data_idx; //points not in pattern tree, but extra vector called shared_match_data.
			Restriction restr = Form::any;
			std::array<char, 4u> name;
		};

		union PnTypesUnion
		{
			PnKnownFunction known_function;
			GenericFunction generic_function;
			Complex complex;
			PnTypedIdxSLC index_slc; //representing GenericFunction's extra parameters, Sum or Product 
			TermString128 string;	//PnVariable is a string and GenericFunction may allocate additional string nodes
			MatchVariable match_variable;

			PnTypesUnion(const PnKnownFunction& val) :known_function(val)   {}
			PnTypesUnion(const GenericFunction& val) :generic_function(val) {}
			PnTypesUnion(const Complex&         val) :complex(val)          {}
			PnTypesUnion(const PnTypedIdxSLC&   val) :index_slc(val)        {}
			PnTypesUnion(const TermString128&   val) :string(val)           {} 
			PnTypesUnion(const MatchVariable&   val) :match_variable(val)   {} 
		};
		static_assert(sizeof(PnTypesUnion) * 8 == 128);

		using PnStore = TermStore<PnTypesUnion>;

		//all MatchVariables of same name in pattern (e.g. "a" in pattern "a*b+a" share the same SharedMatchData to know 
		//whitch actually matched, and if the name "a" is already matched, even if the current instance is not.
		struct SharedMatchData
		{
			TypedIdx match_idx = TypedIdx(); //default initialized to type == Type::COUNT and index == 0
		};

		struct PnTerm
		{
			static constexpr std::size_t max_var_count = 6u;

			std::array<SharedMatchData, max_var_count> shared_match_data;
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
		[[nodiscard]] std::optional<Complex> combine_values_unexact(Store& store, const TypedIdx ref);

		//same as combine_values_unexact, but is quite conservative in what computation is allowed to do.
		template<typename Store_T, typename TypedIdx_T>
		[[nodiscard]] std::optional<Complex> combine_values_exact(Store_T& store, const TypedIdx_T ref);

		//compares two subterms of perhaps different stores, assumes both to have their variadic parts sorted
		template<typename Store_T1, typename Store_T2, typename TypedIdx_T1, typename TypedIdx_T2>
		[[nodiscard]] std::strong_ordering compare(const Store_T1& store_1, const Store_T2& store_2, 
			const TypedIdx_T1 ref_1, const TypedIdx_T2 ref_2);

		//sorts variadic parts by compare
		template<typename Store_T, typename TypedIdx_T>
		void sort(Store_T& store, const TypedIdx_T ref);

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
		void combine_values_unexact() noexcept;
		void combine_values_exact() noexcept;
		void sort() noexcept;

		std::string show_memory_layout() const;
		std::string to_string() const;
		std::string to_pretty_string() const;
	};	//class ArithmeticTerm

}	//namespace bmath



