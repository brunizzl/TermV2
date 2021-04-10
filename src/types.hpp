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

	//everything that is not exclusively affiliated with patterns
	enum class Literal
	{
		complex, //(besides call itself) only of Literal never expected as .function() in call
		symbol, 
		buildin,
		lambda,
		lambda_param,
		call,
		COUNT
	};

	//is present instead of Literal::buildin in a match side pattern node representing a Variadic
	//acts exactly like Literal::buildin 
	//  -> .get_index() of TypedIdx with .get_type() == PatternVariadic{} yields fn::Buildin value
	//difference: if PatternVariadic is used, there is a VariadicMetaData allocated directly 
	//  after call calling PatternVariadic
	//layout in store: [...][...][first call node][more call nodes...][last call node][VariadicMetaData][...] 
	//  if the [first call node] node has a .function() of .get_type() beeing PatternVariadic
	struct PatternVariadic :bmath::intern::SingleSumEnumEntry {}; 

	//acts as unsigned integer type for pattern construction and as auxilliary pattern type
	struct PatternUnsigned :bmath::intern::SingleSumEnumEntry {};

	//these act as different placeholders in a pattern
	enum class Match 
	{
		//these two own their entry in MatchData::single_match, meaning they set a new subterm to match the given index.
		single_restricted, //.get_index() points at RestrictedSingleMatch node in store
		single_unrestricted, //shallow TypedIdx, .get_index() specifies position in MatchData::single_match
		//does not own MatchData entry
		single_weak, //(shallow) is expected to only be encountered after the .get_index() in MatchData::single_match has already been set
		value, //ownership is decided in ValueMatch
		multi, //(shallow), can match an arbitrary amount of subtrees, every instance may only be encountered once -> always owning
		COUNT
	};
	using PatternType = SumEnum<PatternUnsigned, PatternVariadic, Match>;

	using Type = SumEnum<PatternType, Literal>;

	using TypedIdx = BasicTypedIdx<Type>;

	//only some types correspond to actual allocated storage in the term tree.
	//other types can represent all information defining a value of this type in a single unsigned integer,
	//thus TypedIdx can represent them locally without any need for indirection.
	//(also: the union TermNode is more managable with fewer types contained.)
	constexpr bool is_stored_node(const Type type) noexcept
	{
		switch (type) {
		case Type(Literal::complex):           return true;
		case Type(Literal::symbol):            return true;
		case Type(Literal::call):              return true;
		case Type(Literal::buildin):           return false;
		case Type(Literal::lambda):            return true;
		case Type(Literal::lambda_param):      return false;
		case Type(Match::single_restricted):   return true;
		case Type(Match::single_unrestricted): return false;
		case Type(Match::single_weak):         return false;
		case Type(Match::value):               return true;
		case Type(Match::multi):               return false;
		case Type(PatternUnsigned{}):          return false;
		case Type(PatternVariadic{}):          return true;
		case Type(Type::COUNT):                return false; //.get_type() of literal_nullptr
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

		//these functions take complex values as arguments (the arity may be greater than one) and return complex values
		enum class CtoC
		{
			negate, //x -> -x
			invert, //x -> 1/x
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
			COUNT
		};

		//no functions in the classical sense, but can be called as in lambda calculus, acting like a ternary (true returns fist, false second
		enum class Bool
		{
			false_,
			true_,
			COUNT
		};

		//the input space may differ, but the result space is always Bool
		enum class ToBool
		{
			not_,
			eq,
			neq,
			greater,
			smaller,
			greater_eq,
			smaller_eq,
			COUNT
		};

		//various functions only sharing their property of not beeing variadic
		enum class Misc
		{
			id, //unary identity function
			force,  //params[0] := argument   forces evaluation even if it is unexact
			diff,   //params[0] := function  params[1] := variable the derivation is done in respect to
			pair,   //two parameters, no evaluation
			triple, //three parameters, no evaluation
			fmap, //params[0] := unary lambda, params[1] := function call evaluates "fmap(f, g(xs...)) -> g(f(xs)...)"
			COUNT
		};

		//helpers for pattern construction and less important pattern parts
		enum class PatternAux
		{
			single_match,
			value_match, //used to represent all of ValueMatch during build process and final ValueMatch in rhs
			value_proxy, //not used in function call, but to indicate end of subtree in StrongValueMatch::match_index
			multi_match, //differentiates between multiple MultiMatch instances in a single NonComm call
			COUNT
		};

		//these are lumped together, because they behave the same in most cases -> can be seperated easily from rest
		//behavior for every specific element in FixedArity is (at least) defined at array fn::fn_props_table specifying name and arity
		// if the element in Fn is of order one (no functions as arguments / results), 
		//  function fn::eval specifies how to evaluate
		using FixedArity = SumEnum<PatternAux, Misc, CtoC, ToBool, Bool>;

		using Buildin = SumEnum<Variadic, FixedArity>;

		inline constexpr TypedIdx to_typed_idx(const Buildin type) noexcept 
		{ 
			return TypedIdx(static_cast<unsigned>(type), Literal::buildin); 
		}

		inline constexpr Buildin from_typed_idx(const TypedIdx idx) noexcept 
		{
			assert(idx.get_type() == Literal::buildin || idx.get_type() == PatternVariadic{});
			return Buildin(idx.get_index());
		}
	} //namespace fn

	constexpr TypedIdx literal_nullptr = TypedIdx();
	constexpr TypedIdx literal_false   = fn::to_typed_idx(fn::Bool::false_);
	constexpr TypedIdx literal_true    = fn::to_typed_idx(fn::Bool::true_);
	constexpr TypedIdx value_proxy     = fn::to_typed_idx(fn::PatternAux::value_proxy);

	constexpr TypedIdx bool_to_typed_idx(const bool b) { return b ? literal_true : literal_false; }


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
		constexpr std::size_t size() const noexcept { return this->stop - this->start; }
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

	enum class ComplexSubset
	{
		natural,   //{ 1, 2, 3, ... }
		natural_0, //{ 0, 1, 2, ... }
		integer,
		real,
		complex,
		negative,     //implies real   
		positive,     //implies real     	
		not_negative, //implies real  
		not_positive, //implies real 
		COUNT
	};

	enum class Restr
	{
		none, //everything is possible
		callable, 
		boolean,
		COUNT
	};

	using Restriction = SumEnum<Type, fn::Buildin, ComplexSubset, Restr>; //fn::Buildin is more precise Literal::call

	struct RestrictedSingleMatch
	{
		std::uint32_t match_data_index; //indexes in MatchData::single_match_data
		Restriction restriction = Restr::none;
		TypedIdx condition = literal_true;
	};

	struct StrongValueMatch
	{
		std::uint32_t match_data_index; //indexes in MatchData::value_match_data
		ComplexSubset domain; //ComplexSubset::COUNT acts as no restriction
		TypedIdx match_index;
		bool owner;
	};

	//in a valid pattern every commutative Variadic and every 
	//  non-commutative Variadic containing at least one MultiMatch is suceeded by an element of this
	struct VariadicMetaData
	{
		std::uint32_t match_data_index = -1u; //indexes in MatchData::variadic_match_data
		//bit i dertermines whether parameter i is rematchable
		bmath::intern::BitSet32 rematchable = -1u;
		//bit i determines whether parameter i is only matched with terms succeding match of parameter i-1
		bmath::intern::BitSet32 always_after_prev = 0u;
	};

	union TermNode
	{
		//nodes valid everywhere:
		Complex complex;
		Symbol symbol;
		Call call;
		Lambda lambda;

		//nodes only expected in a pattern:
		RestrictedSingleMatch single_match;
		StrongValueMatch value_match;
		VariadicMetaData variadic_data;

		constexpr TermNode(const Complex              & val) noexcept :complex(val)       {}
		constexpr TermNode(const Symbol               & val) noexcept :symbol(val)        {}
		constexpr TermNode(const Call                 & val) noexcept :call(val)          {}
		constexpr TermNode(const Lambda               & val) noexcept :lambda(val)        {}
		constexpr TermNode(const RestrictedSingleMatch& val) noexcept :single_match(val)  {}
		constexpr TermNode(const StrongValueMatch     & val) noexcept :value_match(val)   {}
		constexpr TermNode(const VariadicMetaData     & val) noexcept :variadic_data(val) {}

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
		assert(ref.type == Literal::call);
		using TypedIdx_T = std::conditional_t<R::is_const, const TypedIdx, TypedIdx>;
		using Store_T = std::remove_reference_t<decltype(*ref.store)>;
		using Iter = bmath::intern::detail_vector::SaveIterator<TypedIdx_T, sizeof(TermNode), Store_T>;
		return Iter{ *ref.store, ref.index, 0u };
	}

	//caution: can only be used as a range-based-for-loop, if there is no reallocation possible inside the loop body!
	template<bmath::intern::Reference R> requires (requires { R::store; })
	constexpr auto end(const R& ref) noexcept
	{
		assert(ref.type == Literal::call);
		return bmath::intern::detail_vector::SaveEndIndicator{ (std::uint32_t)ref->call.size() };
	}


	constexpr const VariadicMetaData& variadic_meta_data(const UnsaveRef ref)
	{
		assert(ref.type == Literal::call);
		assert(ref->call.function().get_type().is<PatternVariadic>());
		return *(ref.ptr + ref->call.node_count());
	}

	constexpr VariadicMetaData& variadic_meta_data(const MutRef ref)
	{
		assert(ref.type == Literal::call);
		assert(ref->call.function().get_type().is<PatternVariadic>());
		return *(&ref.store->at(ref.index) + ref->call.node_count());
	}



	namespace fn {
		// a function of fixed arity may restrict the different parameters differently. 
		//if more than four parameters are expected, all parameters beyond the third share the forth restriction
		struct FixedInputSpace
		{
			static constexpr std::size_t max_different_count = 4;
			Restriction spaces[max_different_count];

			constexpr FixedInputSpace() noexcept :spaces{ Restr::none, Restr::none, Restr::none, Restr::none } {}

			constexpr FixedInputSpace(Restriction a)                                              noexcept : spaces{ a, a, a, a } {}
			constexpr FixedInputSpace(Restriction a, Restriction b)                               noexcept : spaces{ a, b, b, b } {}
			constexpr FixedInputSpace(Restriction a, Restriction b, Restriction c)                noexcept : spaces{ a, b, c, c } {}
			constexpr FixedInputSpace(Restriction a, Restriction b, Restriction c, Restriction d) noexcept : spaces{ a, b, c, d } {}

			constexpr Restriction operator[](const std::size_t idx) const noexcept 
			{ 
				return idx < max_different_count ? 
					this->spaces[idx] :
					this->spaces[max_different_count - 1u];
			}
		}; //struct FixedInputSpace

		struct FixedArityProps
		{
			FixedArity type;
			std::string_view name;
			std::size_t arity;
			FixedInputSpace input_space = {}; //assumes all parameters to have the same restriction
			Restriction result_space = Restr::none;
		};

		constexpr auto fixed_arity_table = std::to_array<FixedArityProps>({
			{ Bool::false_            , "false"     , 2u, { Restr::none         }, Restr::none                 },
			{ Bool::true_             , "true"      , 2u, { Restr::none         }, Restr::none                 },
			{ ToBool::not_            , "not"       , 1u, { Restr::boolean      }, Restr::boolean              },
			{ ToBool::eq              , "eq"        , 2u, { Restr::none         }, Restr::boolean              },
			{ ToBool::neq             , "neq"       , 2u, { Restr::none         }, Restr::boolean              },
			{ ToBool::greater         , "greater"   , 2u, { ComplexSubset::real }, Restr::boolean              },
			{ ToBool::smaller         , "smaller"   , 2u, { ComplexSubset::real }, Restr::boolean              },
			{ ToBool::greater_eq      , "greater_eq", 2u, { ComplexSubset::real }, Restr::boolean              },
			{ ToBool::smaller_eq      , "smaller_eq", 2u, { ComplexSubset::real }, Restr::boolean              },
			{ CtoC::negate            , "negate"    , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::invert            , "invert"    , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::pow               , "pow"       , 2u, { Literal::complex    }, Literal::complex            },
			{ CtoC::log               , "log"       , 2u, { Literal::complex    }, Literal::complex            },
			{ CtoC::sqrt              , "sqrt"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::exp               , "exp"       , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::ln                , "ln"        , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::sin               , "sin"       , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::cos               , "cos"       , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::tan               , "tan"       , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::sinh              , "sinh"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::cosh              , "cosh"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::tanh              , "tanh"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::asin              , "asin"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::acos              , "acos"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::atan              , "atan"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::asinh             , "asinh"     , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::acosh             , "acosh"     , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::atanh             , "atanh"     , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::abs               , "abs"       , 1u, { Literal::complex    }, ComplexSubset::not_negative },
			{ CtoC::arg               , "arg"       , 1u, { Literal::complex    }, ComplexSubset::not_negative },
			{ CtoC::re                , "re"        , 1u, { Literal::complex    }, ComplexSubset::real         },
			{ CtoC::im                , "im"        , 1u, { Literal::complex    }, ComplexSubset::real         },
			{ Misc::id                , "id"        , 1u, { Restr::none         }, Restr::none                 },
			{ Misc::force             , "force"     , 1u, { Literal::complex    }, Literal::complex            },
			{ Misc::diff              , "diff"      , 2u, { Restr::none, Literal::symbol }, Restr::none },
			{ Misc::pair              , "pair"      , 2u, {}, Misc::pair },
			{ Misc::triple            , "triple"    , 3u, {}, Misc::triple },
			{ Misc::fmap              , "fmap"      , 2u, { Restr::callable, Literal::call }, Literal::call },
			{ PatternAux::single_match, "_SM"       , 2u, { Restr::none         }, Restr::none                 },
			{ PatternAux::value_match , "_VM"       , 2u, { Restr::none         }, Restr::none                 },
			{ PatternAux::value_proxy , "_VP"       , 0u, { Restr::none         }, PatternAux::value_proxy     },
			{ PatternAux::multi_match , "_MM"       , 2u, { Restr::none         }, Restr::none                 },
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
			Restriction input_space = Restr::none; //assumes all parameters to have the same restriction
			Restriction result_space = Restr::none;
		};

		constexpr auto variadic_table = std::to_array<VariadicProps>({
			{ NonComm::list           , "list"        , false, Restr::none        , NonComm::list       },
			{ NonComm::ordered_sum    , "sum'"        , true , Restr::none        , Restr::none         },
			{ NonComm::ordered_product, "product'"    , true , Restr::none        , Restr::none         },
			{ Comm::sum               , "sum"         , true , Literal::complex   , Literal::complex    },
			{ Comm::product           , "product"     , true , Literal::complex   , Literal::complex    },
			{ Comm::and_              , "and"         , true , Restr::boolean     , Restr::boolean      },
			{ Comm::or_               , "or"          , true , Restr::boolean     , Restr::boolean      },
			{ Comm::multiset          , "multiset"    , false, Restr::none        , Comm::multiset      },
			{ Comm::set               , "set"         , false, Restr::none        , Comm::set           },
			{ Comm::union_            , "union"       , true , Comm::set          , Comm::set           },
			{ Comm::intersection      , "intersection", true , Comm::set          , Comm::set           },
			{ Comm::min               , "min"         , true , ComplexSubset::real, ComplexSubset::real },
			{ Comm::max               , "max"         , true , ComplexSubset::real, ComplexSubset::real },
		});
		static_assert(static_cast<unsigned>(variadic_table.front().type) == 0u);
		static_assert(std::is_sorted(variadic_table.begin(), variadic_table.end(),
			[](auto lhs, auto rhs) { return lhs.type < rhs.type; }));
		static_assert(variadic_table.size() == static_cast<unsigned>(Variadic::COUNT));

		constexpr bool is_associative(const Variadic v) noexcept
		{
			assert(f < Variadic::COUNT);
			return variadic_table[static_cast<unsigned>(v)].associative;
		}

		//properties shared by variadic and FixedArity are stored in a single table below for convinience
		struct CommonProps 
		{ 
			Buildin type = Buildin::COUNT; 
			std::string_view name = ""; 
			Restriction result_space = Restr::none; 
		};

		constexpr auto common_table = [] {
			std::array<CommonProps, (unsigned)Buildin::COUNT> res;
			std::size_t i = 0;
			for (const auto& elem : fixed_arity_table) { res[i++] = { elem.type, elem.name, elem.result_space }; }
			for (const auto& elem : variadic_table)    { res[i++] = { elem.type, elem.name, elem.result_space }; }
			assert(i == res.size());
			return res;
		}();
		static_assert(std::is_sorted(common_table.begin(), common_table.end(), 
			[](auto& a, auto& b) { return a.type < b.type; }), "order elements in table the same way as in buildin");

		//returns Buildin::COUNT if no name was found
		constexpr Buildin type_of(std::string_view name_) noexcept
		{
			return bmath::intern::search(common_table, &CommonProps::name, name_).type;
		}

		constexpr std::string_view name_of(const Buildin f)
		{
			assert(f < Buildin::COUNT);
			return common_table[static_cast<unsigned>(f)].name;
		}

		constexpr Restriction result_space(const Buildin f)
		{
			assert(f < Buildin::COUNT);
			return common_table[static_cast<unsigned>(f)].result_space;
		}

		constexpr Restriction input_space(const Variadic f) 
		{ 
			assert(f < Variadic::COUNT);
			return variadic_table[static_cast<unsigned>(f)].input_space; 
		}

		constexpr const FixedInputSpace& input_space(const FixedArity f) 
		{
			assert(f < FixedArity::COUNT);
			return fixed_arity_table[static_cast<unsigned>(f)].input_space; 
		}
	} //namespace fn



} //namespace simp
