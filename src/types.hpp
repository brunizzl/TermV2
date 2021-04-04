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

	enum class MathType
	{
		complex, //only of MathType never expected as function in call
		boolean, //can act as function of two parameters: true returns first, false second (can not be curried)
		symbol, 
		call,
		buildin,
		lambda,
		lambda_param,
		COUNT
	};

	//is present instead of MathType::buildin in a match side pattern node representing a Variadic
	//acts exactly like MathType::buildin 
	//  -> .get_index() of TypedIdx with .get_type() == PatternBuildin{} yields fn::Buildin value
	//difference: if PatternBuildin is used, there is a VariadicMetaData allocated directly 
	//  in front of call calling  PatternBuildin
	//layout in store: [...][...][VariadicMetaData][Call][...][...] 
	//  if the [Call] node has a .function() of type PatternBuildin
	struct PatternBuildin :bmath::intern::SingleSumEnumEntry {}; 

	enum class SingleMatch
	{ 
		restricted, 
		unrestricted,
		weak, 
		COUNT 
	};
	enum class ValueMatch
	{
		strong,
		weak,
		COUNT
	};
	enum class MultiMatch 
	{ 
		fst, 
		snd, 
		COUNT 
	};
	using MatchType = SumEnum<MultiMatch, ValueMatch, SingleMatch>;

	using PatternType = SumEnum<MatchType, PatternBuildin>;
	using Type = SumEnum<PatternType, MathType>;

	constexpr auto type_count = Type::COUNT;

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
		case Type(PatternBuildin{}):          return true;
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
			id, //unary identity function
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
			fmap, //params[0] := unary lambda, params[1] := function call evaluates "fmap(f, g(xs...)) -> g(f(xs)...)"
			COUNT
		};

		using Buildin = SumEnum<Variadic, FixedArity>;

		inline constexpr TypedIdx to_typed_idx(const Buildin type) noexcept 
		{ 
			return TypedIdx(static_cast<unsigned>(type), MathType::buildin); 
		}

		inline constexpr Buildin from_typed_idx(const TypedIdx idx) noexcept 
		{
			assert(idx.get_type() == MathType::buildin || idx.get_type() == PatternBuildin{});
			return Buildin(idx.get_index());
		}
	} //namespace fn



	using Complex = std::complex<double>;
	using Symbol = StoredVector<char>;

	template<typename T>
	struct Range
	{ 
		T* start = nullptr; 
		T* stop = nullptr;
		constexpr T& operator[](const std::size_t offset) const noexcept { return *(start + offset); }
		constexpr void remove_prefix(const std::size_t length) noexcept { start += std::min(length, stop - start); }
		constexpr void remove_suffix(const std::size_t length) noexcept { stop -= std::min(length, stop - start); }
		constexpr T* begin() const noexcept { return this->start; }
		constexpr T* end() const noexcept { return this->stop; }
	};

	struct Call :StoredVector<TypedIdx>
	{
		using StoredVector<TypedIdx>::StoredVector;

		//first in array is assumed to be callable
		constexpr TypedIdx& function() noexcept { return this->front(); }
		constexpr const TypedIdx& function() const noexcept { return this->front(); }

		//iterate over all but function
		constexpr Range<TypedIdx> parameters() noexcept { return { this->begin() + 1u, this->end() }; }
		constexpr Range<const TypedIdx> parameters() const noexcept { return { this->begin() + 1u, this->end() }; }
	};

	struct Lambda
	{
		TypedIdx definition;
		std::uint32_t param_count;
		//if a lambda is transparent, there is the possibility, that lambdas in the ancestry
		//  own variables in this definition.
		//the outermost lambda is never transparent, otherwise matching in such lambdas is undefined behavior.
		//in the normal form, all lambdas but the outermost one are transparent, otherwise matching in such lambdas
		//  is undefined behavior.
		bool transparent; //note: an intransparent lambda is a combinator
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

	struct Unrestricted :bmath::intern::SingleSumEnumEntry {}; //used if SingleMatch is restricted only by a condition
	using Restriction = SumEnum<Type, fn::Buildin, Domain, Unrestricted>; //fn::BUildin is more precise MathType::call

	struct RestrictedSingleMatch
	{
		Restriction restriction;
		TypedIdx condition; //may be TypedIdx(), then there is no extra condition (else indexes in pattern)
		std::uint32_t match_data_index; //indexes in MatchData::value_match_data
	};

	struct StrongValueMatch
	{
		Domain domain;
		TypedIdx match_index; //indexes in pattern
		std::uint32_t match_data_index; //indexes in MatchData::value_match_data
	};

	//in a valid pattern every Variadic is preceeded by an element of this
	struct VariadicMetaData
	{
		std::uint32_t match_data_idx = -1u; //indexes in MatchData::variadic_match_data
		//bit i dertermines whether parameter i is rematchable
		bmath::intern::BitSet32 rematchable = -1u;
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

	static_assert(sizeof(TermNode) == sizeof(Complex));
	using Store = bmath::intern::BasicStore<TermNode>;

	using Ref = bmath::intern::BasicSaveRef<Type, const Store>;
	using UnsaveRef = bmath::intern::BasicUnsaveRef<Type, TermNode>;
	using MutRef = bmath::intern::BasicSaveRef<Type, Store>;

	//caution: can only be used as a range-based-for-loop, if there is no reallocation possible inside the loop body!
	template<bmath::intern::Reference R> requires (requires { R::store; })
	constexpr auto begin(const R& ref) noexcept
	{
		assert(ref.type == MathType::call);
		using TypedIdx_T = std::conditional_t<R::is_const, const TypedIdx, TypedIdx>;
		using Store_T = std::remove_reference_t<decltype(*ref.store)>;
		using Iter = bmath::intern::detail_vector::SaveIterator<TypedIdx_T, sizeof(TermNode), Store_T>;
		return Iter{ *ref.store, ref.index, 0u };
	}

	//caution: can only be used as a range-based-for-loop, if there is no reallocation possible inside the loop body!
	template<bmath::intern::Reference R> requires (requires { R::store; })
	constexpr auto end(const R& ref) noexcept
	{
		assert(ref.type == MathType::call);
		return bmath::intern::detail_vector::SaveEndIndicator{ (std::uint32_t)ref->call.size() };
	}


	constexpr VariadicMetaData variadic_meta_data(const UnsaveRef ref)
	{
		assert(ref.type == MathType::call);
		assert(ref->call.function().get_type().is<PatternBuildin>());
		return *(ref.ptr - 1);
	}



	namespace fn {
		struct FixedArityProps
		{
			FixedArity type;
			std::string_view name;
			std::size_t arity;
			Restriction input_space = Unrestricted{}; //assumes all parameters to have the same restriction
			Restriction result_space = Unrestricted{};
		};

		constexpr auto fixed_arity_table = std::to_array<FixedArityProps>({
			{ FixedArity::id        , "id"        , 1u, Unrestricted{}   , Unrestricted{}     },
			{ FixedArity::pow       , "pow"       , 2u, MathType::complex, MathType::complex  },
			{ FixedArity::log       , "log"       , 2u, MathType::complex, MathType::complex  },
			{ FixedArity::sqrt      , "sqrt"      , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::exp       , "exp"       , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::ln        , "ln"        , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::sin       , "sin"       , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::cos       , "cos"       , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::tan       , "tan"       , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::sinh      , "sinh"      , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::cosh      , "cosh"      , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::tanh      , "tanh"      , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::asin      , "asin"      , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::acos      , "acos"      , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::atan      , "atan"      , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::asinh     , "asinh"     , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::acosh     , "acosh"     , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::atanh     , "atanh"     , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::abs       , "abs"       , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::arg       , "arg"       , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::re        , "re"        , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::im        , "im"        , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::force     , "force"     , 1u, MathType::complex, MathType::complex  },
			{ FixedArity::diff      , "diff"      , 2u, Unrestricted{}   , Unrestricted{}     },
			{ FixedArity::pair      , "pair"      , 2u, Unrestricted{}   , FixedArity::pair   },
			{ FixedArity::triple    , "triple"    , 3u, Unrestricted{}   , FixedArity::triple },
			{ FixedArity::not_      , "not"       , 1u, MathType::boolean, MathType::boolean  },
			{ FixedArity::eq        , "eq"        , 2u, Unrestricted{}   , MathType::boolean  },
			{ FixedArity::neq       , "neq"       , 2u, Unrestricted{}   , MathType::boolean  },
			{ FixedArity::greater   , "greater"   , 2u, Domain::real     , MathType::boolean  },
			{ FixedArity::smaller   , "smaller"   , 2u, Domain::real     , MathType::boolean  },
			{ FixedArity::greater_eq, "greater_eq", 2u, Domain::real     , MathType::boolean  },
			{ FixedArity::smaller_eq, "smaller_eq", 2u, Domain::real     , MathType::boolean  },
			{ FixedArity::fmap      , "fmap"      , 2u, Unrestricted{}   , Unrestricted{}     },
		});
		static_assert(static_cast<unsigned>(fixed_arity_table.front().type) == 0u);
		static_assert(std::is_sorted(fixed_arity_table.begin(), fixed_arity_table.end(),
			[](auto lhs, auto rhs) { return lhs.type < rhs.type; }));
		static_assert(fixed_arity_table.size() == static_cast<unsigned>(FixedArity::COUNT));

		constexpr std::size_t arity(const FixedArity f) noexcept
		{
			assert(f < FixedArity::COUNT);
			return fixed_arity_table[static_cast<unsigned>(f)].arity;
		}

		struct VariadicProps
		{
			Variadic type;
			std::string_view name;
			bool associative; //allows to flatten nested instances if true
			Restriction input_space = Unrestricted{}; //assumes all parameters to have the same restriction
			Restriction result_space = Unrestricted{};
		};

		constexpr auto variadic_table = std::to_array<VariadicProps>({
			{ NonComm::list           , "list"        , false, Unrestricted{}   , NonComm::list     },
			{ NonComm::ordered_sum    , "sum'"        , true , Unrestricted{}   , Unrestricted{}    },
			{ NonComm::ordered_product, "product'"    , true , Unrestricted{}   , Unrestricted{}    },
			{ Comm::sum               , "sum"         , true , MathType::complex, MathType::complex },
			{ Comm::product           , "product"     , true , MathType::complex, MathType::complex },
			{ Comm::and_              , "and"         , true , MathType::boolean, MathType::boolean },
			{ Comm::or_               , "or"          , true , MathType::boolean, MathType::boolean },
			{ Comm::multiset          , "multiset"    , false, Unrestricted{}   , Comm::multiset    },
			{ Comm::set               , "set"         , false, Unrestricted{}   , Comm::set         },
			{ Comm::union_            , "union"       , true , Comm::set        , Comm::set         },
			{ Comm::intersection      , "intersection", true , Comm::set        , Comm::set         },
			{ Comm::min               , "min"         , true , Domain::real     , Domain::real      },
			{ Comm::max               , "max"         , true , Domain::real     , Domain::real      },
		});
		static_assert(static_cast<unsigned>(variadic_table.front().type) == 0u);
		static_assert(std::is_sorted(variadic_table.begin(), variadic_table.end(),
			[](auto lhs, auto rhs) { return lhs.type < rhs.type; }));
		static_assert(variadic_table.size() == static_cast<unsigned>(Variadic::COUNT));

		constexpr bool is_associative(const Variadic v) noexcept
		{
			return variadic_table[static_cast<unsigned>(v)].associative;
		}

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

		constexpr std::string_view name_of(const Buildin f)
		{
			if (f.is<Variadic>()) {
				return variadic_table[static_cast<unsigned>(f.to<Variadic>())].name;
			}
			assert(f.is<FixedArity>());
			return fixed_arity_table[static_cast<unsigned>(f.to<FixedArity>())].name;
		}

		constexpr Restriction result_space(const Buildin f)
		{
			if (f.is<Variadic>()) {
				return variadic_table[static_cast<unsigned>(f.to<Variadic>())].result_space;
			}
			assert(f.is<FixedArity>());
			return fixed_arity_table[static_cast<unsigned>(f.to<FixedArity>())].result_space;
		}

		constexpr Restriction input_space(const Buildin f)
		{
			if (f.is<Variadic>()) {
				return variadic_table[static_cast<unsigned>(f.to<Variadic>())].input_space;
			}
			assert(f.is<FixedArity>());
			return fixed_arity_table[static_cast<unsigned>(f.to<FixedArity>())].input_space;
		}
	} //namespace fn



} //namespace simp
