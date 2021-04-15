#pragma once

#include <complex>
#include <cassert>
#include <array>
#include <algorithm>
#include <string>
#include <span>

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
		native,
		lambda,
		lambda_param,
		call,
		COUNT
	};

	//is present instead of Literal::call in a match side pattern node representing a call to a Variadic function
	//acts exactly like Literal::call, only that at .get_index() - 1u an extra VariadicMetaData node is allocated in term
	//layout in store: [...][...][VariadicMetaData][first call node][more call nodes...][last call node][...] 
	//                                             ^.get_index() points here
	//note 1: the layout is chosen to enable handling of PattenCall and Literal::call as one in most cases 
	//  and to enable access to VariadicMetaData without first determining how many nodes the call owns.
	//note 2: a PatternCall may only occur once the appearance of a pattern is finalized, e.g. it should have already been combined...
	//  and it may only occur on the match side of a rule
	struct PatternCall :bmath::intern::SingleSumEnumEntry {}; 

	//acts as unsigned integer type for pattern construction and as auxilliary type
	struct PatternUnsigned :bmath::intern::SingleSumEnumEntry {};

	//these act as different placeholders in a pattern
	enum class Match 
	{
		//these two own their entry in MatchData::single_match, meaning they set a new subterm to match the given index.
		single_restricted, //.get_index() points at RestrictedSingleMatch node in store
		single_unrestricted, //shallow NodeIdx, .get_index() specifies position in MatchData::single_match
		//does not own MatchData entry
		single_weak, //(shallow) is expected to only be encountered after the .get_index() in MatchData::single_match has already been set
		value, //ownership is decided in ValueMatch
		multi, //(shallow), can match an arbitrary amount of subtrees, every instance may only be encountered once -> always owning
		COUNT
	};
	using PatternNodeType = SumEnum<Match, PatternUnsigned, PatternCall>; //(made so that PatternCall directly follows Literal::call

	using NodeType = SumEnum<PatternNodeType, Literal>;

	using NodeIdx = BasicTypedIdx<NodeType>;

	//only some types correspond to actual allocated storage in the term tree.
	//other types can represent all information defining a value of this type in a single unsigned integer,
	//thus NodeIdx can represent them locally without any need for indirection.
	//(also: the union TermNode is more managable with fewer types contained.)
	constexpr bool is_stored_node(const NodeType type) noexcept
	{
		switch (type) {
		case NodeType(Literal::complex):           return true;
		case NodeType(Literal::symbol):            return true;
		case NodeType(Literal::native):            return false;
		case NodeType(Literal::lambda):            return true;
		case NodeType(Literal::lambda_param):      return false;
		case NodeType(Literal::call):              return true;
		case NodeType(PatternCall{}):	           return true;
		case NodeType(PatternUnsigned{}):          return false;
		case NodeType(Match::single_restricted):   return true;
		case NodeType(Match::single_unrestricted): return false;
		case NodeType(Match::single_weak):         return false;
		case NodeType(Match::value):               return true;
		case NodeType(Match::multi):               return false;
		case NodeType(NodeType::COUNT):            return false; //.get_type() of literal_nullptr
		default:
			assert(false);
			BMATH_UNREACHABLE;
			return false;
		}
	}



	//short for native
	namespace nv {
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
			floor,  //round down
			ceil,   //round up
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
		enum class MiscFn
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
		enum class PatternAuxFn
		{
			single_match,
			value_match, //used to represent all of ValueMatch during build process and final ValueMatch in rhs
			value_proxy, //not used in function call, but to indicate end of subtree in ValueMatch::match_index
			multi_match, //differentiates between multiple MultiMatch instances in a single NonComm call
			COUNT
		};

		//these are lumped together, because they behave the same in most cases -> can be seperated easily from rest
		//behavior for every specific element in FixedArity is (at least) defined at array nv::fn_props_table specifying name and arity
		// if the element in Fn is of order one (no functions as arguments / results), 
		//  function nv::eval specifies how to evaluate
		using FixedArity = SumEnum<PatternAuxFn, MiscFn, CtoC, ToBool, Bool>;

		//everything callable
		using Function_ = SumEnum<Variadic, FixedArity>;

		//intended to be used as restriction in a single match variable, or narrow the return possibilities of a function
		enum class Restr
		{
			any, //everything is possible
			callable, //eighter Literal::lambda or Literal::native with .is<Function_>()
			boolean, //maybe add "types" section to native, handle bool there?
			COUNT
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

		//mostly used to denote restrictions on allowed types 
		//  note: NodeType also appears here, but only as restriction, not as indicator of possible indirection
		using Constant = SumEnum<NodeType, ComplexSubset, Restr>;

		//values of this type are stored in .get_index() of a NodeIdx if .get_type() is Literal::native
		using Native = SumEnum<Constant, Function_>;

		inline constexpr NodeIdx to_typed_idx(const Native type) noexcept 
		{ 
			return NodeIdx(static_cast<unsigned>(type), Literal::native); 
		}

		inline constexpr Native from_typed_idx(const NodeIdx idx) noexcept 
		{
			assert(idx.get_type() == Literal::native);
			return Native(idx.get_index());
		}
	} //namespace nv

	constexpr NodeIdx literal_nullptr = NodeIdx();
	constexpr NodeIdx literal_false   = nv::to_typed_idx(nv::Bool::false_);
	constexpr NodeIdx literal_true    = nv::to_typed_idx(nv::Bool::true_);
	constexpr NodeIdx value_proxy     = nv::to_typed_idx(nv::PatternAuxFn::value_proxy);

	constexpr NodeIdx bool_to_typed_idx(const bool b) { return b ? literal_true : literal_false; }


	using Complex = std::complex<double>;
	using Symbol = StoredVector<char>;

	struct Call :StoredVector<NodeIdx>
	{
		using StoredVector<NodeIdx>::StoredVector;

		//first in array is assumed to be callable
		constexpr NodeIdx& function() noexcept { return this->front(); }
		constexpr const NodeIdx& function() const noexcept { return this->front(); }

		//iterate over all but function
		constexpr std::span<NodeIdx> parameters() noexcept { return { this->begin() + 1u, this->end() }; }
		constexpr std::span<const NodeIdx> parameters() const noexcept { return { this->begin() + 1u, this->end() }; }
	};

	struct Lambda
	{
		NodeIdx definition;
		std::uint32_t param_count;
		//if a lambda is transparent, there is the possibility, that lambdas in the ancestry
		//  own variables in this definition.
		//the outermost lambda is never transparent, otherwise matching in such lambdas is undefined behavior.
		//in the normal form, all lambdas but the outermost one are transparent, otherwise matching in such lambdas
		//  is undefined behavior.
		bool transparent; //note: an intransparent lambda is a combinator
		bool lazy = false; //currently unused
	}; 	

	struct RestrictedSingleMatch
	{
		std::uint32_t match_data_index; //indexes in MatchData::single_match_data
		nv::Native restriction = nv::Restr::any;
		NodeIdx condition = literal_true;
	};

	struct ValueMatch
	{
		std::uint32_t match_data_index; //indexes in MatchData::value_match_data
		nv::ComplexSubset domain = nv::ComplexSubset::complex; //ComplexSubset::complex acts as no restriction
		NodeIdx match_index;
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
		ValueMatch value_match;
		VariadicMetaData variadic_data;

		constexpr TermNode(const Complex              & val) noexcept :complex(val)       {}
		constexpr TermNode(const Symbol               & val) noexcept :symbol(val)        {}
		constexpr TermNode(const Call                 & val) noexcept :call(val)          {}
		constexpr TermNode(const Lambda               & val) noexcept :lambda(val)        {}
		constexpr TermNode(const RestrictedSingleMatch& val) noexcept :single_match(val)  {}
		constexpr TermNode(const ValueMatch           & val) noexcept :value_match(val)   {}
		constexpr TermNode(const VariadicMetaData     & val) noexcept :variadic_data(val) {}

		constexpr operator const Complex              & () const noexcept { return this->complex;       }
		constexpr operator const Symbol               & () const noexcept { return this->symbol;        }
		constexpr operator const Call                 & () const noexcept { return this->call;          }
		constexpr operator const Lambda               & () const noexcept { return this->lambda;        }
		constexpr operator const RestrictedSingleMatch& () const noexcept { return this->single_match;  }
		constexpr operator const ValueMatch           & () const noexcept { return this->value_match;   }
		constexpr operator const VariadicMetaData     & () const noexcept { return this->variadic_data; }

		constexpr operator Complex              & () noexcept { return this->complex;       }
		constexpr operator Symbol               & () noexcept { return this->symbol;        }
		constexpr operator Call                 & () noexcept { return this->call;          }
		constexpr operator Lambda               & () noexcept { return this->lambda;        }
		constexpr operator RestrictedSingleMatch& () noexcept { return this->single_match;  }
		constexpr operator ValueMatch           & () noexcept { return this->value_match;   }
		constexpr operator VariadicMetaData     & () noexcept { return this->variadic_data; }
	}; //TermNode
	static_assert(sizeof(TermNode) == sizeof(Complex));

	using Store = bmath::intern::BasicStore<TermNode>;

	using Ref = bmath::intern::BasicSaveRef<NodeType, const Store>;
	using UnsaveRef = bmath::intern::BasicUnsaveRef<NodeType, TermNode>;
	using MutRef = bmath::intern::BasicSaveRef<NodeType, Store>;

	//caution: can only be used as a range-based-for-loop, if there is no reallocation possible inside the loop body!
	template<bmath::intern::Reference R> requires (requires { R::store; })
	constexpr auto begin(const R& ref) noexcept
	{
		assert(ref.type == Literal::call || ref.type == PatternCall{});
		using TypedIdx_T = std::conditional_t<R::is_const, const NodeIdx, NodeIdx>;
		using Store_T = std::remove_reference_t<decltype(*ref.store)>;
		using Iter = bmath::intern::detail_vector::SaveIterator<TypedIdx_T, sizeof(TermNode), Store_T>;
		return Iter{ *ref.store, ref.index, 0u };
	}

	//caution: can only be used as a range-based-for-loop, if there is no reallocation possible inside the loop body!
	template<bmath::intern::Reference R> requires (requires { R::store; })
	constexpr auto end(const R& ref) noexcept
	{
		assert(ref.type == Literal::call || ref.type == PatternCall{});
		return bmath::intern::detail_vector::SaveEndIndicator{ (std::uint32_t)ref->call.size() };
	}


	constexpr const VariadicMetaData& variadic_meta_data(const UnsaveRef ref)
	{
		assert(ref.type == PatternCall{});
		return *(ref.ptr - 1u);
	}

	constexpr VariadicMetaData& variadic_meta_data(const MutRef ref)
	{
		assert(ref.type == PatternCall{});
		return *(&ref.store->at(ref.index) - 1u);
	}



	namespace nv {
		// a function of fixed arity may restrict the different parameters differently. 
		//if more than four parameters are expected, all parameters beyond the third share the forth restriction
		struct FixedInputSpace
		{
			static constexpr std::size_t max_different_count = 4;
			Native spaces[max_different_count];

			constexpr FixedInputSpace() noexcept :spaces{ Restr::any, Restr::any, Restr::any, Restr::any } {}

			constexpr FixedInputSpace(Native a)                               noexcept : spaces{ a, a, a, a } {}
			constexpr FixedInputSpace(Native a, Native b)                     noexcept : spaces{ a, b, b, b } {}
			constexpr FixedInputSpace(Native a, Native b, Native c)           noexcept : spaces{ a, b, c, c } {}
			constexpr FixedInputSpace(Native a, Native b, Native c, Native d) noexcept : spaces{ a, b, c, d } {}

			constexpr Native operator[](const std::size_t idx) const noexcept
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
			Native result_space = Restr::any;
		};

		constexpr auto fixed_arity_table = std::to_array<FixedArityProps>({
			{ Bool::false_              , "false"     , 2u, { Restr::any          }, Restr::any                  },
			{ Bool::true_               , "true"      , 2u, { Restr::any          }, Restr::any                  },
			{ ToBool::not_              , "not"       , 1u, { Restr::boolean      }, Restr::boolean              },
			{ ToBool::eq                , "eq"        , 2u, { Restr::any          }, Restr::boolean              },
			{ ToBool::neq               , "neq"       , 2u, { Restr::any          }, Restr::boolean              },
			{ ToBool::greater           , "greater"   , 2u, { ComplexSubset::real }, Restr::boolean              },
			{ ToBool::smaller           , "smaller"   , 2u, { ComplexSubset::real }, Restr::boolean              },
			{ ToBool::greater_eq        , "greater_eq", 2u, { ComplexSubset::real }, Restr::boolean              },
			{ ToBool::smaller_eq        , "smaller_eq", 2u, { ComplexSubset::real }, Restr::boolean              },
			{ CtoC::negate              , "negate"    , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::invert              , "invert"    , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::pow                 , "pow"       , 2u, { Literal::complex    }, Literal::complex            },
			{ CtoC::log                 , "log"       , 2u, { Literal::complex    }, Literal::complex            },
			{ CtoC::sqrt                , "sqrt"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::exp                 , "exp"       , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::ln                  , "ln"        , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::sin                 , "sin"       , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::cos                 , "cos"       , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::tan                 , "tan"       , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::sinh                , "sinh"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::cosh                , "cosh"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::tanh                , "tanh"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::asin                , "asin"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::acos                , "acos"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::atan                , "atan"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::asinh               , "asinh"     , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::acosh               , "acosh"     , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::atanh               , "atanh"     , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::abs                 , "abs"       , 1u, { Literal::complex    }, ComplexSubset::not_negative },
			{ CtoC::arg                 , "arg"       , 1u, { Literal::complex    }, ComplexSubset::not_negative },
			{ CtoC::re                  , "re"        , 1u, { Literal::complex    }, ComplexSubset::real         },
			{ CtoC::im                  , "im"        , 1u, { Literal::complex    }, ComplexSubset::real         },
			{ CtoC::floor               , "floor"     , 1u, { ComplexSubset::real }, ComplexSubset::integer      },
			{ CtoC::ceil                , "ceil"      , 1u, { ComplexSubset::real }, ComplexSubset::integer      },
			{ MiscFn::id                , "id"        , 1u, { Restr::any          }, Restr::any                  },
			{ MiscFn::force             , "force"     , 1u, { Literal::complex    }, Literal::complex            },
			{ MiscFn::diff              , "diff"      , 2u, { Restr::any, Literal::symbol }, Restr::any },
			{ MiscFn::pair              , "pair"      , 2u, {}, MiscFn::pair },
			{ MiscFn::triple            , "triple"    , 3u, {}, MiscFn::triple },
			{ MiscFn::fmap              , "fmap"      , 2u, { Restr::callable, Literal::call }, Literal::call },
			{ PatternAuxFn::single_match, "_SM"       , 2u, { Restr::any         }, Restr::any                 },
			{ PatternAuxFn::value_match , "_VM"       , 2u, { Restr::any         }, Restr::any                 },
			{ PatternAuxFn::value_proxy , "\\"        , 0u, { Restr::any         }, PatternAuxFn::value_proxy     },//can not be constructed from a string
			{ PatternAuxFn::multi_match , "_MM"       , 2u, { Restr::any         }, Restr::any                 },
		});
		static_assert(static_cast<unsigned>(fixed_arity_table.front().type) == 0u);
		static_assert(bmath::intern::is_sorted_by(fixed_arity_table, &FixedArityProps::type));
		static_assert(fixed_arity_table.size() == static_cast<unsigned>(FixedArity::COUNT));

		constexpr std::size_t arity(const FixedArity f) noexcept
		{
			assert(f < FixedArity(FixedArity::COUNT));
			return fixed_arity_table[static_cast<unsigned>(f)].arity;
		}

		constexpr const FixedInputSpace& input_space(const FixedArity f)
		{
			assert(f < FixedArity(FixedArity::COUNT));
			return fixed_arity_table[static_cast<unsigned>(f)].input_space;
		}

		struct VariadicProps
		{
			Variadic type;
			std::string_view name;
			bool associative; //allows to flatten nested instances if true
			Native input_space = Restr::any; //assumes all parameters to have the same restriction
			Native result_space = Restr::any;
		};

		constexpr auto variadic_table = std::to_array<VariadicProps>({
			{ NonComm::list           , "list"        , false, Restr::any         , NonComm::list       },
			{ NonComm::ordered_sum    , "sum'"        , true , Restr::any         , Restr::any          },
			{ NonComm::ordered_product, "product'"    , true , Restr::any         , Restr::any          },
			{ Comm::sum               , "sum"         , true , Literal::complex   , Literal::complex    },
			{ Comm::product           , "product"     , true , Literal::complex   , Literal::complex    },
			{ Comm::and_              , "and"         , true , Restr::boolean     , Restr::boolean      },
			{ Comm::or_               , "or"          , true , Restr::boolean     , Restr::boolean      },
			{ Comm::multiset          , "multiset"    , false, Restr::any         , Comm::multiset      },
			{ Comm::set               , "set"         , false, Restr::any         , Comm::set           },
			{ Comm::union_            , "union"       , true , Comm::set          , Comm::set           },
			{ Comm::intersection      , "intersection", true , Comm::set          , Comm::set           },
			{ Comm::min               , "min"         , true , ComplexSubset::real, ComplexSubset::real },
			{ Comm::max               , "max"         , true , ComplexSubset::real, ComplexSubset::real },
		});
		static_assert(static_cast<unsigned>(variadic_table.front().type) == 0u);
		static_assert(bmath::intern::is_sorted_by(variadic_table, &VariadicProps::type));
		static_assert(variadic_table.size() == static_cast<unsigned>(Variadic::COUNT));

		constexpr bool is_associative(const Variadic f) noexcept
		{
			assert(f < Variadic(Variadic::COUNT));
			return variadic_table[static_cast<unsigned>(f)].associative;
		}

		constexpr nv::Native input_space(const Variadic f)
		{
			assert(f < Variadic(Variadic::COUNT));
			return variadic_table[static_cast<unsigned>(f)].input_space;
		}

		//properties shared by variadic and FixedArity are stored in a single table below for convinience
		struct CommonProps 
		{ 
			Native type = Native::COUNT; 
			std::string_view name = ""; 
			Native result_space = Restr::any;
		};

		constexpr auto constant_table = std::to_array<CommonProps>({
			{ Restr::any                 , "any"         , Literal::native },
			{ Restr::callable            , "callable"    , Literal::native },
			{ Restr::boolean             , "bool"        , Literal::native },
			{ ComplexSubset::natural     , "nat"         , Literal::native },
			{ ComplexSubset::natural_0   , "nat_0"       , Literal::native },
			{ ComplexSubset::integer     , "int"         , Literal::native },
			{ ComplexSubset::real        , "real"        , Literal::native },
			{ ComplexSubset::complex     , "\\"          , Literal::native }, //can not be constructed from a string
			{ ComplexSubset::negative    , "\\"          , Literal::native }, //can not be constructed from a string
			{ ComplexSubset::positive    , "\\"          , Literal::native }, //can not be constructed from a string
			{ ComplexSubset::not_negative, "\\"          , Literal::native }, //can not be constructed from a string
			{ ComplexSubset::not_positive, "\\"          , Literal::native }, //can not be constructed from a string
			{ Literal::complex           , "complex"     , Literal::native },
			{ Literal::symbol            , "symbol"      , Literal::native },
			{ Literal::native            , "native"      , Literal::native },
			{ Literal::lambda            , "lambda"      , Literal::native },
			{ Literal::lambda_param      , "lambda_param", Literal::native },
			{ Literal::call              , "call"        , Literal::native },
			{ PatternCall{}              , "\\"          , Literal::native }, //can not be constructed from a string
			{ PatternUnsigned{}          , "_UInt"       , Literal::native },
			{ Match::single_restricted   , "\\"          , Literal::native }, //can not be constructed from a string
			{ Match::single_unrestricted , "\\"          , Literal::native }, //can not be constructed from a string
			{ Match::single_weak         , "\\"          , Literal::native }, //can not be constructed from a string
			{ Match::value               , "\\"          , Literal::native }, //can not be constructed from a string
			{ Match::multi               , "\\"          , Literal::native }, //can not be constructed from a string
		});
		static_assert(constant_table.size() == (unsigned)Constant::COUNT);
		static_assert(bmath::intern::is_sorted_by(constant_table, &CommonProps::type));

		constexpr auto common_table = [] {
			std::array<CommonProps, (unsigned)Native::COUNT> res;
			std::size_t i = 0;
			for (const auto& elem : fixed_arity_table) { res[i++] = { elem.type, elem.name, elem.result_space }; }
			for (const auto& elem : variadic_table)    { res[i++] = { elem.type, elem.name, elem.result_space }; }
			for (const auto& elem : constant_table)    { res[i++] = { elem.type, elem.name, elem.result_space }; }
			return res;
		}();
		static_assert(bmath::intern::is_sorted_by(common_table, &CommonProps::type)
			, "order elements in table the same way as in native");
		static_assert(common_table.size() == (unsigned)Native::COUNT);

		//returns Native::COUNT if no name was found
		constexpr Native type_of(std::string_view name_) noexcept
		{
			return bmath::intern::search(common_table, &CommonProps::name, name_).type;
		}

		constexpr std::string_view name_of(const Native f)
		{
			assert(f < Native(Native::COUNT));
			return common_table[static_cast<unsigned>(f)].name;
		}

		constexpr nv::Native result_space(const Native f)
		{
			assert(f < Native(Native::COUNT));
			return common_table[static_cast<unsigned>(f)].result_space;
		}
	} //namespace nv





	//all MatchVariables of same name in pattern (e.g. "a" in pattern "a*b+a" share the same SharedTreeDatum to know 
	//whitch actually matched, and if the name "a" is already matched, even if the current instance is not.
	struct SharedSingleMatchEntry
	{
		NodeIdx match_idx = NodeIdx{}; //indexes in Term to simplify
		bool is_set() const noexcept { return this->match_idx != NodeIdx(); } //debugging
	};

	struct SharedValueMatchEntry
	{
		Complex value = std::numeric_limits<double>::quiet_NaN();
		bool is_set() const noexcept { return !std::isnan(this->value.real()); } //debugging
	};

	struct SharedVariadicEntry
	{
		//no call to nv::Variadic in a pattern may have more parameters than max_pn_variadic_params_count many
		static constexpr std::size_t max_pn_variadic_params_count = 10u;

		using MatchPos_T = decltype(Call::Info::size);

		//every element in pattern (except all multi match) has own entry which logs, 
		//  with which element in term to match it currently is associated with.
		std::array<MatchPos_T, max_pn_variadic_params_count> match_positions = {};

		NodeIdx match_idx = NodeIdx(); //indexes in Term to simplify (the haystack)

		constexpr SharedVariadicEntry() noexcept { this->match_positions.fill(-1u); }

		//checks this->match_positions if needle is contained
		//use with care: might check more than actually contained in specific pattern!
		constexpr bool index_matched(const MatchPos_T needle) const noexcept
		{
			const auto stop = this->match_positions.end();
			return std::find(this->match_positions.begin(), stop, needle) != stop;
		}
	}; //SharedVariadicEntry

	//to allow a constant RewriteRule to be matched against, all match info is stored here
	struct MatchData
	{
		//maximal number of unrelated TreeMatchVariables allowed per pattern
		static constexpr std::size_t max_single_match_count = 8u;
		//maximal number of variadics allowed per pattern
		static constexpr std::size_t max_variadic_count = 4u;
		//maximal number of unrelated ValueMatchVariables allowed per pattern
		static constexpr std::size_t max_value_match_count = 2u;

		std::array<SharedValueMatchEntry, max_single_match_count> single_match_data = {};
		std::array<SharedVariadicEntry, max_variadic_count> variadic_data = {};
		std::array<SharedSingleMatchEntry, max_value_match_count> value_match_data = {};

		constexpr auto& value_info(const ValueMatch& var) noexcept
		{	return this->value_match_data[var.match_data_index];
		}

		constexpr auto& value_info(const ValueMatch& var) const noexcept
		{	return this->value_match_data[var.match_data_index];
		}

		constexpr auto& single_info(const RestrictedSingleMatch& var) noexcept
		{	return this->single_match_data[var.match_data_index];
		}

		constexpr auto& single_info(const RestrictedSingleMatch& var) const noexcept
		{	return this->single_match_data[var.match_data_index];
		}
	}; //MatchData

} //namespace simp
