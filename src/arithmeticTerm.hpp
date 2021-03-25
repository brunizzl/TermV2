#pragma once

#include <complex>
#include <optional>
#include <compare>
#include <string_view>
#include <array>
#include <variant>

#include "utility/sumEnum.hpp"
#include "utility/misc.hpp"
#include "utility/vector.hpp"

#include "typedIndex.hpp"
#include "termStore.hpp"
#include "reference.hpp"
#include "parseTerm.hpp"
#include "termVector.hpp"

namespace bmath::intern {

	//matching algorithm will try to also match permutations
	enum class Comm //short for Commutative
	{
		sum,
		product,
		multiset,
		set,
		union_,
		intersection,
		COUNT
	};

	enum class NonComm
	{
		list,
		ordered_sum,
		ordered_product,
		COUNT
	};

	using Variadic = SumEnum<Comm, NonComm>;

	//all functions known at compile time will not store their name with every instance in the store. this does.
	//the memory is sectioned in two parts: 
	//the index points at the IndexVector containing the parameters.
	//the function name follwos as CharVector in direct succession
	//(by starting with the parameters, most functions need no extra case to handle NamedFn)
	struct NamedFn :SingleSumEnumEntry {};

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
	using FixedArity = SumEnum<NamedFn, Fn>;

	using Function = SumEnum<Variadic, FixedArity>;

	//the only leaves in MathType
	enum class Literal
	{
		variable,
		complex,
		COUNT
	};

	using MathType = SumEnum<Literal, Function>;

	using MathIdx = BasicTypedIdx<MathType>;

	using IndexVector = StoredVector<MathIdx>;
	using CharVector = StoredVector<char>;
	using Complex = std::complex<double>;



	union MathUnion
	{
		Complex complex;
		IndexVector parameters; //all in Variadic and all in Fn
		CharVector characters;

		constexpr MathUnion(const Complex    & val) noexcept :complex(val)     {}
		constexpr MathUnion(const IndexVector& val) noexcept :parameters(val)  {}
		constexpr MathUnion(const CharVector & val) noexcept :characters(val)  {} 
		constexpr MathUnion()                       noexcept :complex(0.0)     {} 

		constexpr auto operator<=>(const MathUnion&) const = default;

		constexpr operator const Complex    &() const noexcept { return this->complex;     }
		constexpr operator const IndexVector&() const noexcept { return this->parameters;  }
		constexpr operator const CharVector &() const noexcept { return this->characters;  }

		constexpr operator Complex    &() noexcept { return this->complex;     }
		constexpr operator IndexVector&() noexcept { return this->parameters;  }
		constexpr operator CharVector &() noexcept { return this->characters;  }
	};

	static_assert(sizeof(MathUnion) * 8 == 128);
	using MathStore = BasicStore<MathUnion>;


	template<typename T>
	concept MathReference = Reference<T> && T::is_const && std::is_same_v<typename T::value_type, MathUnion>;

	template<typename T>
	concept MutMathReference = Reference<T> && !T::is_const && std::is_same_v<typename T::value_type, MathUnion>;


	using Ref = BasicSaveRef<MathType, const MathStore>;
	using UnsaveRef = BasicUnsaveRef<MathType, MathUnion>;
	using MutRef = BasicSaveRef<MathType, MathStore>;

	static_assert(MathReference<Ref>);
	static_assert(MathReference<UnsaveRef>);
	static_assert(MutMathReference<MutRef>);


	//utility for NamedFn, types in Fn and types in Variadic
	namespace fn {
		OptionalComplex eval(Fn type, const std::array<OptionalComplex, 4>& param_vals);

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

		constexpr std::size_t arity(const MathType type) noexcept 
		{ return fn_props_table[static_cast<unsigned>(type.to<Fn>())].arity; }

		struct VariadicProperties
		{
			Variadic type = Variadic::COUNT;
			std::string_view name = "";
			bool associative = false; //allows to flatten nested instances if true
		};

		//every item enumerated in Variadic (except COUNT, duh) may be listed here in order of apperance in Variadic
		constexpr auto variadic_props_table = std::to_array<VariadicProperties>({
			{ NonComm::list           , "list"        , false },
			{ NonComm::ordered_sum    , "sum'"        , true  },
			{ NonComm::ordered_product, "product'"    , true  },
			{ Comm::sum               , "sum"         , true  },
			{ Comm::product           , "product"     , true  },
			{ Comm::multiset          , "multiset"    , false },
			{ Comm::set               , "set"         , false },
			{ Comm::union_            , "union"       , true  },
			{ Comm::intersection      , "intersection", true  },
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

		constexpr bool is_associative(const Variadic type) noexcept 
		{ return variadic_props_table[static_cast<unsigned>(type)].associative; }

		constexpr bool is_associative(const MathType type) noexcept 
		{ return is_associative(type.to<Variadic>()); }



		constexpr auto&        range(const UnsaveRef ref) noexcept { return ref->parameters; }
		constexpr auto&        range(const       Ref ref) noexcept { return ref->parameters; }
		constexpr auto    save_range(const       Ref ref) noexcept { return ref.cast<IndexVector>(); }
		constexpr auto         range(const    MutRef ref) noexcept { return ref.cast<IndexVector>(); }
		constexpr auto& unsave_range(const    MutRef ref) noexcept { return ref->parameters; }


		template<Reference R>
		constexpr std::uint32_t named_fn_name_index(const R named_fn) noexcept
		{
			return named_fn.index + named_fn->parameters.node_count();
		}

		template<Reference R>
		constexpr const CharVector& named_fn_name(const R named_fn) noexcept
		{
			return static_cast<const CharVector&>(named_fn.store->at(fn::named_fn_name_index(named_fn)));
		}

		constexpr const CharVector& named_fn_name(const UnsaveRef named_fn) noexcept
		{
			return static_cast<const CharVector&>(*named_fn.raw_at(fn::named_fn_name_index(named_fn)));
		}

		template<detail_enum::Enumeratable Type_T, StoreLike Store_T, typename Parameters_T>
		inline auto build_named_fn(Store_T& store, const std::string_view name, const Parameters_T& params)
		{
			using TypedIdx = BasicTypedIdx<Type_T>;
			using TypedIdxVec = StoredVector<TypedIdx>;

			const std::size_t parameter_capacity = TypedIdxVec::smallest_fit_capacity(params.size());
			const std::size_t nr_parameter_nodes = TypedIdxVec::_node_count(parameter_capacity);

			const std::size_t name_capacity = CharVector::smallest_fit_capacity(name.size());
			const std::size_t nr_name_nodes = CharVector::_node_count(name_capacity);

			const std::size_t result_index = store.allocate_n(nr_parameter_nodes + nr_name_nodes);
			TypedIdxVec::emplace(store.at(result_index), params, parameter_capacity);
			CharVector::emplace(store.at(result_index + nr_parameter_nodes), name, name_capacity);
			return TypedIdx(result_index, NamedFn{});
		}


		enum class Commutative :bool { no, ye };
		enum class Associative :bool { no, ye };

		struct FunctionProperties
		{
			static constexpr std::size_t variadic = -1ull;

			std::string_view name; //assumed to be pointing at c-string literal (e.g. "sin" written as such in code)			
			std::size_t arity;
			Commutative commutative; //"ye" assumes arity == variadic
			Associative associative; //"ye" assumes arity == variadic

			constexpr FunctionProperties(std::string_view const n, Commutative const c, Associative const a) noexcept
				:name(n), arity(variadic), commutative(c), associative(a) {}

			constexpr FunctionProperties(std::string_view const n, std::size_t const a) noexcept
				:name(n), arity(a), associative(Associative::no), commutative(Commutative::no) {}

			enum class Category 
			{
				commutative,    //implies variadic
				noncommutative, //implies variadic
				nonvariadic,
			};

			constexpr Category category() const noexcept 
			{
				if (this->arity == variadic) {
					return (bool)this->commutative ? Category::commutative : Category::noncommutative;
				}
				else {
					assert(!(bool)this->commutative && !(bool)this->associative);
					return Category::nonvariadic;
				}
			}
		};

		constexpr auto functions = []() {
			auto functions = std::to_array<FunctionProperties>({
				{ "sum"         , Commutative::ye, Associative::ye },
				{ "product"     , Commutative::ye, Associative::ye },
				{ "sum'"        , Commutative::no, Associative::ye },
				{ "product'"    , Commutative::no, Associative::ye },
				{ "list"        , Commutative::no, Associative::no },
				{ "set"         , Commutative::ye, Associative::no },
				{ "union"       , Commutative::ye, Associative::ye },
				{ "intersection", Commutative::ye, Associative::ye },
				{ "multiset"    , Commutative::ye, Associative::no },
				{ "pow"         , 2u },
				{ "log"         , 2u },
				{ "sqrt"        , 1u },
				{ "exp"         , 1u },
				{ "ln"          , 1u },
				{ "sin"         , 1u },
				{ "cos"         , 1u },
				{ "tan"         , 1u },
				{ "sinh"        , 1u },
				{ "cosh"        , 1u },
				{ "tanh"        , 1u },
				{ "asin"        , 1u },
				{ "acos"        , 1u },
				{ "atan"        , 1u },
				{ "asinh"       , 1u },
				{ "acosh"       , 1u },
				{ "atanh"       , 1u },
				{ "abs"         , 1u },
				{ "arg"         , 1u },
				{ "re"          , 1u },
				{ "im"          , 1u },
				{ "force"       , 1u },
				{ "diff"        , 2u },
			});

			std::sort(functions.begin(), functions.end(), 
				[](FunctionProperties const& fst, FunctionProperties const& snd) {
					return fst.category() != snd.category() ?
						fst.category() < snd.category() :
						fst.name < snd.name;
			});
			return functions;
		}();

		constexpr std::size_t commutative_count = std::count_if(functions.begin(), functions.end(),
			[](const FunctionProperties& fp) { return fp.category() == FunctionProperties::Category::commutative; });
		constexpr std::size_t noncommutative_count = std::count_if(functions.begin(), functions.end(),
			[](const FunctionProperties& fp) { return fp.category() == FunctionProperties::Category::noncommutative; });
		constexpr std::size_t nonvariadic_count = std::count_if(functions.begin(), functions.end(),
			[](const FunctionProperties& fp) { return fp.category() == FunctionProperties::Category::nonvariadic; });

	} //namespace fn

	using Commutative_   = FiniteUnsignedInt<fn::commutative_count>;
	using NonCommutative = FiniteUnsignedInt<fn::noncommutative_count>;
	using FixedArity_    = FiniteUnsignedInt<fn::nonvariadic_count>;

	using Variadic_ = SumEnum<NonCommutative, Commutative_>;
	using Function_ = SumEnum<NamedFn, FixedArity_, Variadic_>;

	static_assert((unsigned)Function_::COUNT == fn::functions.size() + 1u, 
		"a value of type Function_ encodes an element in fn::functions table or NamedFn");

	namespace fn {

		template<Reference R>
		constexpr std::string_view name_of(R const ref) noexcept
		{
			assert(ref.type.is<Function_>());
			if (ref.type.is<NamedFn>()) {
				return fn::named_fn_name(ref);
			}
			else {
				return functions[static_cast<unsigned>(ref.type.to<Function_>())].name;
			}
		}

		constexpr Function_ type_of(std::string_view const name_) noexcept 
		{
			auto const iter = std::find_if(functions.begin(), functions.end(), 
				[name_](FunctionProperties const& f) { return f.name == name_; });
			if (iter == functions.end()) {
				return Function_::COUNT;
			}
			else {
				return Function_(iter - functions.begin());
			}
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
		[[nodiscard]] MathIdx combine(const MutRef ref, const bool exact);

		//compares two subterms of perhaps different stores, assumes both to have their parameters parts sorted
		[[nodiscard]] std::strong_ordering compare(const UnsaveRef ref_1, const UnsaveRef ref_2);

		//sorts parameters parts by compare
		void sort(const MutRef ref);

		//counts number of logical nodes of subtree (note: logical nodes might be fewer than used slots in store)
		std::size_t count(const UnsaveRef ref);

		//copies subtree starting at src_ref into dst_store and returns its head
		[[nodiscard]] MathIdx copy(const Ref src_ref, MathStore& dst_store);

		//returns true iff subtree starting at ref contains to_contain (or is to_contain itself)
		bool contains(const UnsaveRef ref, const MathIdx to_contain);

		bool contains_variables(const UnsaveRef ref);

		//returns TypedIdx() if unsuccsessfull
		MathIdx search_variable(const UnsaveRef ref, const std::string_view name);

		//calls first tree::combine, then tree::sort
		[[nodiscard]] MathIdx establish_basic_order(const MutRef ref);

		//(true -> every position in store eighter free or used by (children of) heads exactly once)
		bool valid_storage(const MathStore& store, const std::initializer_list<MathIdx> heads);

	} //namespace tree

}	//namespace bmath::intern

namespace bmath {

	class Term
	{
	public:
		intern::MathStore store;
		intern::MathIdx head;

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
	};	//class Term

}	//namespace bmath



