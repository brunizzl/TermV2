#pragma once

#include <complex>
#include <cassert>
#include <array>
#include <algorithm>
#include <string>

#include "utility/sumEnum.hpp"

#include "typedIndex.hpp"
#include "termVector.hpp"
#include "termStore.hpp"

namespace simp {

	using bmath::intern::SumEnum;
	using bmath::intern::StoredVector;
	using bmath::intern::BasicTypedIdx;

	enum struct MathType
	{
		complex, 
		boolean, //can act as function of two parameters: true returns first, false second (can not be curried)
		symbol, 
		call,
		buildin,
		lambda,
		lambda_param,
		COUNT
	};

	enum struct SingleMatch
	{ 
		restricted, 
		unrestricted,
		weak, 
		COUNT 
	};

	enum struct ValueMatch
	{
		strong,
		weak,
		COUNT
	};

	enum class MultiMatch { fst, snd, COUNT };

	using MatchType = SumEnum<SingleMatch, ValueMatch, MultiMatch>;	
	using Type = SumEnum<MathType, MatchType>;

	using TypedIdx = BasicTypedIdx<Type>;

	//only some types correspond to actual nodes in the term tree.
	//other types can represent all information defining a value of this type in a single unsigned integer,
	//thus TypedIdx can represent them locally without any need for indirection.
	//(also: the union TermNode is more managable with fewer types contained.)
	constexpr bool is_node(const Type type) noexcept
	{
		switch (type) {
		case Type(MathType::complex):         return true;
		case Type(MathType::boolean):         return false;
		case Type(MathType::symbol):          return true;
		case Type(MathType::call):            return true;
		case Type(MathType::buildin):         return false;
		case Type(MathType::lambda):          return true;
		case Type(MathType::lambda_param):    return false;
		case Type(SingleMatch::restricted):   return true;
		case Type(SingleMatch::unrestricted): return false;
		case Type(SingleMatch::weak):         return false;
		case Type(ValueMatch::strong):        return true;
		case Type(ValueMatch::weak):          return false;
		case Type(MultiMatch::fst):           return false;
		case Type(MultiMatch::snd):           return false;
		default:
			assert(false);
			return false;
		}
	}

	namespace fn {
		//matching algorithm will try to also match permutations
		enum class Comm //short for Commutative
		{
			sum,
			product,
			and_,
			or_,
			multiset,
			set,
			union_,
			intersection,
			min,
			max,
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

		//these are lumped together, because they behave the same in most cases -> can be seperated easily from rest
		//behavior for every specific element in FixedArity is (at least) defined at array fn::fn_props_table specifying name and arity
		// if the element in Fn is of order one (no functions as arguments / results), 
		//  function fn::eval specifies how to evaluate
		enum class FixedArity
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
			pair,   //two parameters, no evaluation
			triple, //three parameters, no evaluation
			not_,   //params[0] := argument
			eq,
			neq,
			greater,
			smaller,
			greater_eq,
			smaller_eq,
			COUNT
		};

		using Buildin = SumEnum<Variadic, FixedArity>;

		constexpr TypedIdx to_typed_idx(const Buildin type) noexcept { return TypedIdx(static_cast<unsigned>(type), MathType::buildin); }
		constexpr Buildin from_typed_idx(const TypedIdx idx) noexcept {
			assert(idx.get_type() == MathType::buildin);
			return Buildin(idx.get_index());
		}

		struct FixedArityProps
		{
			FixedArity type;
			std::string_view name;
			std::size_t arity;
		};

		constexpr auto fixed_arity_table = std::to_array<FixedArityProps>({
			{ FixedArity::pow       , "pow"       , 2u },
			{ FixedArity::log       , "log"       , 2u },
			{ FixedArity::sqrt      , "sqrt"      , 1u },
			{ FixedArity::exp       , "exp"       , 1u },
			{ FixedArity::ln        , "ln"        , 1u },
			{ FixedArity::sin       , "sin"       , 1u },
			{ FixedArity::cos       , "cos"       , 1u },
			{ FixedArity::tan       , "tan"       , 1u },
			{ FixedArity::sinh      , "sinh"      , 1u },
			{ FixedArity::cosh      , "cosh"      , 1u },
			{ FixedArity::tanh      , "tanh"      , 1u },
			{ FixedArity::asin      , "asin"      , 1u },
			{ FixedArity::acos      , "acos"      , 1u },
			{ FixedArity::atan      , "atan"      , 1u },
			{ FixedArity::asinh     , "asinh"     , 1u },
			{ FixedArity::acosh     , "acosh"     , 1u },
			{ FixedArity::atanh     , "atanh"     , 1u },
			{ FixedArity::abs       , "abs"       , 1u },
			{ FixedArity::arg       , "arg"       , 1u },
			{ FixedArity::re        , "re"        , 1u },
			{ FixedArity::im        , "im"        , 1u },
			{ FixedArity::force     , "force"     , 1u },
			{ FixedArity::diff      , "diff"      , 2u },
			{ FixedArity::pair      , "pair"      , 2u },
			{ FixedArity::triple    , "triple"    , 3u },
			{ FixedArity::not_      , "not"       , 1u },
			{ FixedArity::eq        , "eq"        , 2u },
			{ FixedArity::neq       , "neq"       , 2u },
			{ FixedArity::greater   , "greater"   , 2u },
			{ FixedArity::smaller   , "smaller"   , 2u },
			{ FixedArity::greater_eq, "greater_eq", 2u },
			{ FixedArity::smaller_eq, "smaller_eq", 2u },
		});
		static_assert(static_cast<unsigned>(fixed_arity_table.front().type) == 0u);
		static_assert(std::is_sorted(fixed_arity_table.begin(), fixed_arity_table.end(),
			[](auto lhs, auto rhs) { return lhs.type < rhs.type; }));
		static_assert(fixed_arity_table.size() == static_cast<unsigned>(FixedArity::COUNT));

		struct VariadicProps
		{
			Variadic type;
			std::string_view name;
			bool associative; //allows to flatten nested instances if true
		};

		constexpr auto variadic_table = std::to_array<VariadicProps>({
			{ NonComm::list           , "list"        , false },
			{ NonComm::ordered_sum    , "sum'"        , true  },
			{ NonComm::ordered_product, "product'"    , true  },
			{ Comm::sum               , "sum"         , true  },
			{ Comm::product           , "product"     , true  },
			{ Comm::and_              , "and"         , true  },
			{ Comm::or_               , "or"          , true  },
			{ Comm::multiset          , "multiset"    , false },
			{ Comm::set               , "set"         , false },
			{ Comm::union_            , "union"       , true  },
			{ Comm::intersection      , "intersection", true  },
			{ Comm::min               , "min"         , true  },
			{ Comm::max               , "max"         , true  },
		});
		static_assert(static_cast<unsigned>(variadic_table.front().type) == 0u);
		static_assert(std::is_sorted(variadic_table.begin(), variadic_table.end(),
			[](auto lhs, auto rhs) { return lhs.type < rhs.type; }));
		static_assert(variadic_table.size() == static_cast<unsigned>(Variadic::COUNT));

		//returns Buildin::COUNT if no name was found
		constexpr Buildin type_of(std::string_view name_) noexcept
		{
			if (const auto iter = std::find_if(fixed_arity_table.begin(), fixed_arity_table.end(),
				[name_](const FixedArityProps& p) { return p.name == name_; });
				iter != fixed_arity_table.end()) 
			{
				return iter->type;
			}
			if (const auto iter = std::find_if(variadic_table.begin(), variadic_table.end(),
				[name_](const VariadicProps& p) { return p.name == name_; });
				iter != variadic_table.end())
			{
				return iter->type;
			}
			return Buildin::COUNT;
		}

		constexpr std::string_view name_of(const Buildin type)
		{
			if (type.is<Variadic>()) {
				return variadic_table[static_cast<unsigned>(type.to<Variadic>())].name;
			}
			assert(type.is<FixedArity>());
			return fixed_arity_table[static_cast<unsigned>(type.to<FixedArity>())].name;
		}

	} //namespace fn



	using Complex = std::complex<double>;
	using Symbol = StoredVector<char>;

	struct IdxRange { TypedIdx* start; TypedIdx* stop; };
	struct ConstIdxRange { const TypedIdx* start; const TypedIdx *stop; };
	constexpr TypedIdx* begin(const IdxRange& r) noexcept { return r.start; }
	constexpr TypedIdx* end(const IdxRange& r) noexcept { return r.stop; }
	constexpr const TypedIdx* begin(const ConstIdxRange& r) noexcept { return r.start; }
	constexpr const TypedIdx* end(const ConstIdxRange& r) noexcept { return r.stop; }

	struct Call :StoredVector<TypedIdx>
	{
		using StoredVector<TypedIdx>::StoredVector;

		//first in array is assumed to be callable
		constexpr TypedIdx& function() noexcept { return this->front(); }
		constexpr const TypedIdx& function() const noexcept { return this->front(); }

		//iterate over all but function
		constexpr IdxRange parameters() noexcept { return { this->begin() + 1u, this->end() }; }
		constexpr ConstIdxRange parameters() const noexcept { return { this->begin() + 1u, this->end() }; }
	};

	struct Lambda
	{
		TypedIdx definition;
		std::uint32_t param_count;
	}; 	

	enum class Restriction
	{
		condition, //restricted by RestrictedSingleMatch::condition
		nn1, //compact for "not negative one" (basically any, but the exact term "-1" will not be accepted)
		no_val, //any, but Complex is forbidden    
		symbol, //only Symbol is accepted
		COUNT
	};

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

	struct RestrictedSingleMatch
	{
		Restriction restriction;
		TypedIdx condition; //only pointig to something if this->restriction == Restriction::condition 
		std::uint32_t match_data_index;
	};

	struct StrongValueMatch
	{
		TypedIdx match_index;
		Domain domain;
		std::uint32_t match_data_index;
	};

	//in a valid pattern every Variadic is preceeded by an element of this
	struct VariadicMetaData
	{
		std::uint32_t match_data_idx = -1u; //indexes in MatchData::variadic_match_data

		//bit i dertermines whether parameter i is rematchable
		bmath::intern::BitSet32 rematchable = -1u;
		//bit i determines whether parameter i is only matched with term directly succeding match of parameter i-1
		bmath::intern::BitSet32 directly_after_prev = 0u;
		//bit i determines whether parameter i is only matched with terms succeding match of parameter i-1
		bmath::intern::BitSet32 always_after_prev = 0u;
	};

	union TermNode
	{
		Complex complex;
		Symbol symbol;
		Call call;
		Lambda lambda;
		RestrictedSingleMatch single_match;
		StrongValueMatch value_match;
		VariadicMetaData variadic_data;

		constexpr TermNode(const Complex              & val) noexcept :complex(val)       {}
		constexpr TermNode(const Symbol               & val) noexcept :symbol(val)        {}
		constexpr TermNode(const Call                 & val) noexcept :call(val)          {}
		constexpr TermNode(const Lambda               & val) noexcept :lambda(val)        {}
		constexpr TermNode(const RestrictedSingleMatch& val) noexcept :single_match(val)  {}
		constexpr TermNode(const StrongValueMatch     & val) noexcept :value_match(val)   {}
		constexpr TermNode(const VariadicMetaData&      val) noexcept :variadic_data(val) {}

		constexpr operator const Complex              & () const noexcept { return this->complex;       }
		constexpr operator const Symbol               & () const noexcept { return this->symbol;        }
		constexpr operator const Call                 & () const noexcept { return this->call;          }
		constexpr operator const Lambda               & () const noexcept { return this->lambda;        }
		constexpr operator const RestrictedSingleMatch& () const noexcept { return this->single_match;  }
		constexpr operator const StrongValueMatch     & () const noexcept { return this->value_match;   }
		constexpr operator const VariadicMetaData     & () const noexcept { return this->variadic_data; }

		constexpr operator Complex              & () noexcept { return this->complex;       }
		constexpr operator Symbol               & () noexcept { return this->symbol;        }
		constexpr operator Call                 & () noexcept { return this->call;          }
		constexpr operator Lambda               & () noexcept { return this->lambda;        }
		constexpr operator RestrictedSingleMatch& () noexcept { return this->single_match;  }
		constexpr operator StrongValueMatch     & () noexcept { return this->value_match;   }
		constexpr operator VariadicMetaData     & () noexcept { return this->variadic_data; }
	};

	static_assert(sizeof(TermNode) * 8 == 128);
	using Store = bmath::intern::BasicStore<TermNode>;

	using Ref = bmath::intern::BasicSaveRef<Type, const Store>;
	using UnsaveRef = bmath::intern::BasicUnsaveRef<Type, TermNode>;
	using MutRef = bmath::intern::BasicSaveRef<Type, Store>;

	//term without any pattern shenaniganz
	struct Literal 
	{
		Store store;
		TypedIdx head;

		Literal(std::string name);

		std::string to_string() const noexcept;

		constexpr Ref ref() const noexcept { return Ref(this->store, this->head); }
	};

} //namespace simp
