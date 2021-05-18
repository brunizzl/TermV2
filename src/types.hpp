#pragma once

#include <complex>
#include <cassert>
#include <array>
#include <algorithm>
#include <string>
#include <span>
#include <variant>

#include "utility/sumEnum.hpp"
#include "utility/misc.hpp"

#include "typedIndex.hpp"
#include "termVector.hpp"
#include "countingStore.hpp"

namespace simp {

	using bmath::intern::SumEnum;
	using bmath::intern::SingleSumEnumEntry;
	using bmath::intern::StoredVector;
	using bmath::intern::BasicTypedIdx;

	//everything that is not exclusively affiliated with patterns
	enum class Literal
	{
		complex, //only of Literal never expected as .function() in f_app
		symbol,
		lambda,
		lambda_param,
		f_app,
		COUNT
	};

	//is present instead of Literal::f_app in a match side pattern node representing an application of a Variadic function
	//acts exactly like Literal::f_app, only that at (.get_index() - 1u) an extra FAppInfo node is allocated in term
	//layout in store: [...][...][FAppInfo][first f_app node][more f_app nodes...][last f_app node][...] 
	//                                     ^.get_index() points here
	//note 1: the layout is chosen to enable handling of PatternFApp and Literal::f_app as one in most cases 
	//  and to enable access to FAppInfo without first determining how many nodes the function application holds.
	//note 2: a PatternFApp may only occur once the appearance of a pattern is finalized, e.g. it should have already been combined
	//  and it may only occur on the match side of a rule
	struct PatternFApp :SingleSumEnumEntry {}; 

	//acts as unsigned integer type for pattern construction, meaning .get_index() is interpreted as unsigned integer
	struct PatternUnsigned :SingleSumEnumEntry {};

	enum class SingleMatch
	{
		//these two own their entry in match::State::single_match, meaning they set a new subterm to match the given index.
		restricted,   //.get_index() points at RestrictedSingleMatch node in store
		unrestricted, //shallow NodeIndex, .get_index() specifies position in match::State::single_match
		//does not own match::State entry
		weak,  //(shallow) is expected to only be encountered after the .get_index() in match::State::single_match has already been set
		COUNT
	};

	enum class SpecialMatch
	{
		multi, //only expected in rhs, lhs holds multi info in PatternCallData
		value, //ownership is decided in ValueMatch
		COUNT
	};

	using MatchVariableType = SumEnum<SpecialMatch, SingleMatch>;

	using PatternNodeType = SumEnum<MatchVariableType, PatternUnsigned, PatternFApp>; //(ordered so that PatternFApp directly follows Literal::f_app)

	using NodeType = SumEnum<PatternNodeType, Literal>;

	using NodeIndex = BasicTypedIdx<NodeType>;

	//only some types correspond to actual allocated storage in the term tree.
	//other types can represent all information defining a value of this type in a single unsigned integer,
	//thus NodeIndex can represent them locally without any need for indirection.
	//(also: the union TermNode is more managable with fewer types contained.)
	constexpr bool is_stored_node(const NodeType type) noexcept
	{
		switch (type) {
		case NodeType(Literal::complex):           return true;
		case NodeType(Literal::symbol):            return false;
		case NodeType(Literal::lambda):            return true;
		case NodeType(Literal::lambda_param):      return false;
		case NodeType(Literal::f_app):             return true;
		case NodeType(PatternFApp{}):	           return true;
		case NodeType(PatternUnsigned{}):          return false;
		case NodeType(SingleMatch::restricted):    return true;
		case NodeType(SingleMatch::unrestricted):  return false;
		case NodeType(SingleMatch::weak):          return false;
		case NodeType(SpecialMatch::multi):        return true;
		case NodeType(SpecialMatch::value):        return true;
		case NodeType(NodeType::COUNT):            return false; //.get_type() of literal_nullptr
		default:
			assert(false);
			BMATH_UNREACHABLE;
			return false;
		}
	}



	//short for native_symbol
	namespace nv {
		//matching algorithm will try to also match permutations
		enum class Comm //short for Commutative
		{
			sum,
			prod,
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
			ordered_prod,
			COUNT
		};

		using Variadic = SumEnum<Comm, NonComm>;

		//these functions take complex values as arguments (the arity may be greater than one) and return complex values
		enum class CtoC
		{
			divide, //usually only encountered in a condition
			pow,    //params[0] := base      params[1] := expo    
			log,	//params[0] := base      params[1] := argument
			sqrt,	
			exp,	
			ln,		
			sin,	
			cos,	
			tan,	
			sinh,	
			cosh,	
			tanh,	
			asin,	
			acos,	
			atan,	
			asinh,	
			acosh,	
			atanh,	
			abs,	
			arg,	
			re,		
			im,		
			conj, 
			floor,  //round down
			ceil,   //round up
			COUNT
		};

		//no functions in the classical sense, but can be applied as in lambda calculus, acting like a ternary 
		//  (true returns fist, false second)
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
			contains, //true if fst contains snd
			COUNT
		};

		//various functions only sharing their property of not beeing variadic
		enum class MiscFn
		{
			id, //identity function
			diff,   //params[0] := function application params[1] := variable the derivation is done in respect to
			fdiff, //params[0] := function (e.g. "fdiff(sin) -> cos" or "fdiff(tan) -> \x .1/cos(x)^2" or "fdiff(\x .x^2) -> \x .2 x")
			pair,   //two parameters, no evaluation
			fst, //access pair elements
			snd, //access pair elements
			COUNT
		};

		//as in haskell, but applicable to any function application, not just lists:
		//the first parameter (params[0]) is always an applicative, defining on an application of which function the transformation should be performed
		enum class HaskellFn
		{
			map, //params[1] := unary lambda, params[2] := function application (evaluates "map(g, f, g(xs...)) -> g(f(xs)...)")
			filter, //params[1] := unary lambda returning bool, params[2] := function application (leaves only parameters of params[2] where params[1] returns true)
			split, //as ffilter, but returns both subsets (predicate true and else) as pair
			foldl, //folds application from left
			foldr, //folds application from right
			COUNT
		};

		//helpers for pattern construction
		enum class PatternFn
		{
			value_match, //used to represent all of ValueMatch during build process and final ValueMatch in rhs
			of_type, //returns true if fst is element of snd
			COUNT
		};

		//these are lumped together, because they behave the same in most cases -> can be seperated easily from rest
		//behavior for every specific element in FixedArity is (at least) defined at array nv::fn_props_table specifying name and arity
		// if the element in Fn is of order one (no functions as arguments / results), 
		//  function nv::eval specifies how to evaluate
		using FixedArity = SumEnum<PatternFn, HaskellFn, MiscFn, CtoC, ToBool, Bool>;

		//everything applicable
		using Function_ = SumEnum<Variadic, FixedArity>;

		//intended to be used as restriction in a single match variable, or narrow the return possibilities of a function
		enum class Restr
		{
			any, //everything is possible
			applicable, //eighter Literal::lambda or Literal::symbol with .is<Function_>() or with not .is<Native>()
			boolean, //maybe add "types" section to native_symbol, handle bool there?
			no_value, //all except Literal::complex
			not_neg_1, //all except the exact value of -1.0 for Literal::complex
			not_0, //all except the exact value of 0.0 (or differently signed variants) for Literal::complex
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

		enum class PatternConst
		{
			value_proxy, //found as offspring of .inverse of ValueMatch
			COUNT
		};

		//mostly used to denote restrictions on allowed types 
		//  note: NodeType also appears here, but only as restriction, not as indicator of possible indirection
		//this SumEnum contains everything in Native not applicable, although also everything applicable is a constant.
		//in particular: Bool is not part of Constant, as it can be applied
		using Constant = SumEnum<NodeType, ComplexSubset, Restr, PatternConst>;

		//values of this type are stored in .get_index() of a NodeIndex if .get_type() is Literal::native_symbol
		using Native = SumEnum<Constant, Function_>;
	} //namespace nv

	//if .value is larger than Native::COUNT, the Symbol was added at runtime
	using Symbol = bmath::intern::FinalSumEnum<nv::Native>;

	inline constexpr NodeIndex from_native(const nv::Native n) noexcept
	{
		return NodeIndex(static_cast<unsigned>(n), Literal::symbol);
	}

	inline constexpr Symbol to_symbol(const NodeIndex idx) noexcept
	{
		assert(idx.get_type() == Literal::symbol);
		return Symbol(idx.get_index());
	}

	constexpr NodeIndex literal_nullptr = NodeIndex();
	constexpr NodeIndex literal_false   = from_native(nv::Bool::false_);
	constexpr NodeIndex literal_true    = from_native(nv::Bool::true_);
	constexpr NodeIndex value_proxy     = from_native(nv::PatternConst::value_proxy);

	constexpr NodeIndex bool_to_typed_idx(const bool b) { return b ? literal_true : literal_false; }


	using Complex = std::complex<double>;

	struct FApp :StoredVector<NodeIndex>
	{
		using StoredVector<NodeIndex>::StoredVector;

		//first in array is assumed to be applicable
		constexpr NodeIndex& function() noexcept { return this->front(); }
		constexpr const NodeIndex& function() const noexcept { return this->front(); }

		//iterate over all but function
		constexpr std::span<NodeIndex> parameters() noexcept { return { this->begin() + 1u, this->end() }; }
		constexpr std::span<const NodeIndex> parameters() const noexcept { return { this->begin() + 1u, this->end() }; }
	};

	struct Lambda
	{
		std::uint32_t param_count;
		NodeIndex definition;
		//if a lambda is transparent, there is the possibility, that lambdas in the ancestry
		//  own variables in this definition.
		//the outermost lambda is never transparent, otherwise matching in such lambdas is undefined behavior.
		//in the normal form, all lambdas but the outermost one are transparent, otherwise matching in such lambdas
		//  is undefined behavior.
		bool transparent; //note: an intransparent lambda is a combinator
	}; 	

	struct RestrictedSingleMatch
	{
		std::uint32_t match_state_index; //indexes in match::State::single_vars
		NodeIndex condition; //eighter Literal::symbol, then assumed to be native, else some testable expression
	};

	//only expected in rhs (lhs stores multis only as bool /bits in PatternCallData)
	struct MultiMatch
	{
		std::uint32_t match_state_index; //indexes in match::State::f_app_entries
		//imaginary (as multi match variables are not explicitly in lhs) index in application in lhs, minus the other multis
		//e.g. in "list(xs..., 1, ys..., list(as..., 2, bs..., 3, cs...)) = list(cs...)" 
		//  has rhs instance of "cs..." .index_in_params = 2 (preceeded by "2" and "3")
		std::uint32_t index_in_params; 
	};

	struct ValueMatch
	{
		std::uint32_t match_state_index; //indexes in match::State::value_vars
		NodeIndex inverse; //in pattern "'sin'(2 $k 'pi')" is "_VP / 2" the inverse needed to compute $k, with _VP representing value_proxy
		nv::ComplexSubset domain = nv::ComplexSubset::complex;
		bool owner;
	};

	//determines which function is called to match a function application in a pattern with a literal
	enum class MatchStrategy :char
	{
		permutation,
		dilation,
		backtracking,
		linear,
	};

	//in a pattern every function application is preceeded by an instance of this
	//i have tried writing this as std::variant, but failed to keep the boilerplate acceptable in size :(
	struct FAppInfo
	{
		//determines what algorithm is choosen to match this application
		//  permutation:  uses all later members, only preceeded_by_multi is degraded to a bool
		//  dilation:     uses all later members except always_preceeding_next (because that is a tautology here)
		//  backtracking: uses ONLY rematchable_params (not even uses match_state_index)
		//  linear:       uses nothing
		MatchStrategy strategy = MatchStrategy::linear; //default required as linear in build_rume::build_lhs_multis_and_pattern_f_apps
		//indexes in match::State::f_app_entries (used in both commutative and non-commutative)
		std::uint32_t match_state_index = -1u;
		//bit i dertermines whether parameter i is rematchable (used in both commutative and non-commutative)
		bmath::intern::BitSet16 rematchable_params = (std::uint16_t)-1;
		//bit i determines whether parameter i is guaranteed to never have a 
		//  match at a higher haystack index than parameter i + 1 (used in commutative)
		bmath::intern::BitSet16 always_preceeding_next = (std::uint16_t)0;
		//bit i determines, wether pattern parameter i comes after a multi match variable
		// (because the multi match variables are not present in lhs as actual parameters)
		//note: as a multi is also valid as last parameter, a pattern function application may only hold up to 15 non-multi parameters!
		//(used in non-commutative as bitset and in commutative as bool)
		bmath::intern::BitSet16 preceeded_by_multi = (std::uint16_t)0;
	}; //struct FAppInfo

	union TermNode
	{
		//nodes valid everywhere:
		Complex complex;
		FApp f_app;
		Lambda lambda;

		//nodes only expected in a pattern:
		RestrictedSingleMatch single_match;
		MultiMatch multi_match;
		ValueMatch value_match;
		FAppInfo f_app_info;

		constexpr TermNode(const Complex              & val) noexcept :complex(val)      {}
		constexpr TermNode(const FApp                 & val) noexcept :f_app(val)        {}
		constexpr TermNode(const Lambda               & val) noexcept :lambda(val)       {}
		constexpr TermNode(const RestrictedSingleMatch& val) noexcept :single_match(val) {}
		constexpr TermNode(const MultiMatch           & val) noexcept :multi_match(val)  {}
		constexpr TermNode(const ValueMatch           & val) noexcept :value_match(val)  {}
		constexpr TermNode(const FAppInfo             & val) noexcept :f_app_info(val)   {}

		constexpr operator const Complex              & () const noexcept { return this->complex;      }
		constexpr operator const FApp                 & () const noexcept { return this->f_app;        }
		constexpr operator const Lambda               & () const noexcept { return this->lambda;       }
		constexpr operator const RestrictedSingleMatch& () const noexcept { return this->single_match; }
		constexpr operator const MultiMatch           & () const noexcept { return this->multi_match;  }
		constexpr operator const ValueMatch           & () const noexcept { return this->value_match;  }
		constexpr operator const FAppInfo             & () const noexcept { return this->f_app_info;   }

		constexpr operator Complex              & () noexcept { return this->complex;      }
		constexpr operator FApp                 & () noexcept { return this->f_app;        }
		constexpr operator Lambda               & () noexcept { return this->lambda;       }
		constexpr operator RestrictedSingleMatch& () noexcept { return this->single_match; }
		constexpr operator MultiMatch           & () noexcept { return this->multi_match;  }
		constexpr operator ValueMatch           & () noexcept { return this->value_match;  }
		constexpr operator FAppInfo             & () noexcept { return this->f_app_info;   }
	}; //TermNode
	static_assert(sizeof(TermNode) == sizeof(Complex));

	using Store = BasicCountingStore<TermNode, NodeIndex>;
	using MonotonicStore = bmath::intern::BasicMonotonicStore<TermNode>;

	using Ref = bmath::intern::BasicSaveRef<NodeType, const Store>;
	using UnsaveRef = bmath::intern::BasicUnsaveRef<NodeType, TermNode>;
	using MutRef = bmath::intern::BasicSaveRef<NodeType, Store>;

	//caution: can only be used as a range-based-for-loop, if there is no reallocation possible inside the loop body!
	template<bmath::intern::Reference R> requires (requires { R::store; })
	constexpr auto begin(const R& ref) noexcept
	{
		assert(ref.type == Literal::f_app || ref.type == PatternFApp{});
		using TypedIdx_T = std::conditional_t<R::is_const, const NodeIndex, NodeIndex>;
		using Store_T = std::remove_reference_t<decltype(*ref.store)>;
		using namespace bmath::intern::detail_vector;
		return SaveIterator<TypedIdx_T, sizeof(TermNode), Store_T>{ *ref.store, ref.index, 0u };
	}

	//caution: can only be used as a range-based-for-loop, if there is no reallocation possible inside the loop body!
	template<bmath::intern::Reference R> requires (requires { R::store; })
	constexpr auto end(const R& ref) noexcept
	{
		assert(ref.type == Literal::f_app || ref.type == PatternFApp{});
		return bmath::intern::detail_vector::SaveEndIndicator{ (std::uint32_t)ref->f_app.size() };
	}

	template<bmath::intern::Reference R> requires (!requires { R::store; })
	constexpr auto begin(const R& ref) noexcept
	{
		assert(ref.type == Literal::f_app || ref.type == PatternFApp{});
		return ref->f_app.begin();
	}

	template<bmath::intern::Reference R> requires (!requires { R::store; })
	constexpr auto end(const R& ref) noexcept
	{
		assert(ref.type == Literal::f_app || ref.type == PatternFApp{});
		return ref->f_app.end();
	}


	constexpr inline const FAppInfo& f_app_info(const UnsaveRef ref)
	{
		assert(ref.type == PatternFApp{});
		return *(ref.ptr - 1u);
	}

	constexpr inline FAppInfo& f_app_info(const MutRef ref)
	{
		assert(ref.type == PatternFApp{});
		return *(&ref.store->at(ref.index) - 1u);
	}


	struct TypeError
	{
		const char* what;
		UnsaveRef occurence; 
	};



	namespace nv {
		// a function of fixed arity may restrict the different parameters differently. 
		//if more than max_different_count parameters are expected, all parameters beyond the third share the forth restriction
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
			{ Bool::false_           , "false"     , 2u, { Restr::any          }, Restr::any                  },
			{ Bool::true_            , "true"      , 2u, { Restr::any          }, Restr::any                  },
			{ ToBool::not_           , "not"       , 1u, { Restr::boolean      }, Restr::boolean              },
			{ ToBool::eq             , "eq"        , 2u, { Restr::any          }, Restr::boolean              },
			{ ToBool::neq            , "neq"       , 2u, { Restr::any          }, Restr::boolean              },
			{ ToBool::greater        , "greater"   , 2u, { ComplexSubset::real }, Restr::boolean              },
			{ ToBool::smaller        , "smaller"   , 2u, { ComplexSubset::real }, Restr::boolean              },
			{ ToBool::greater_eq     , "greater_eq", 2u, { ComplexSubset::real }, Restr::boolean              },
			{ ToBool::smaller_eq     , "smaller_eq", 2u, { ComplexSubset::real }, Restr::boolean              },
			{ ToBool::contains       , "contains"  , 2u, { Restr::any          }, Restr::boolean              },
			{ CtoC::divide           , "_Divide"   , 2u, { Literal::complex    }, Literal::complex            },
			{ CtoC::pow              , "pow"       , 2u, { Literal::complex    }, Literal::complex            },
			{ CtoC::log              , "log"       , 2u, { Literal::complex    }, Literal::complex            },
			{ CtoC::sqrt             , "sqrt"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::exp              , "exp"       , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::ln               , "ln"        , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::sin              , "sin"       , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::cos              , "cos"       , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::tan              , "tan"       , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::sinh             , "sinh"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::cosh             , "cosh"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::tanh             , "tanh"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::asin             , "asin"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::acos             , "acos"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::atan             , "atan"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::asinh            , "asinh"     , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::acosh            , "acosh"     , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::atanh            , "atanh"     , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::abs              , "abs"       , 1u, { Literal::complex    }, ComplexSubset::not_negative },
			{ CtoC::arg              , "arg"       , 1u, { Literal::complex    }, ComplexSubset::not_negative },
			{ CtoC::re               , "re"        , 1u, { Literal::complex    }, ComplexSubset::real         },
			{ CtoC::im               , "im"        , 1u, { Literal::complex    }, ComplexSubset::real         },
			{ CtoC::conj             , "conj"      , 1u, { Literal::complex    }, Literal::complex            },
			{ CtoC::floor            , "floor"     , 1u, { ComplexSubset::real }, ComplexSubset::integer      },
			{ CtoC::ceil             , "ceil"      , 1u, { ComplexSubset::real }, ComplexSubset::integer      },
			{ MiscFn::id             , "id"        , 1u, { Restr::any          }, Restr::any                  },
			{ MiscFn::diff           , "diff"      , 2u, { Restr::any, Literal::symbol }, Restr::any          },
			{ MiscFn::fdiff          , "fdiff"     , 1u, { Restr::applicable   }, Restr::applicable           },
			{ MiscFn::pair           , "pair"      , 2u, {}                     , MiscFn::pair                },
			{ MiscFn::fst            , "fst"       , 1u, { MiscFn::pair        }, Restr::any                  },
			{ MiscFn::snd            , "snd"       , 1u, { MiscFn::pair        }, Restr::any                  },
			{ HaskellFn::map         , "map"       , 3u, { Restr::applicable, Restr::applicable, Literal::f_app             }, Literal::f_app  },
			{ HaskellFn::filter      , "filter"    , 3u, { Restr::applicable, Restr::applicable, Literal::f_app             }, Literal::f_app  },
			{ HaskellFn::split       , "split"     , 3u, { Restr::applicable, Restr::applicable, Literal::f_app             }, MiscFn::pair    },
			{ HaskellFn::foldl       , "ffoldl"    , 4u, { Restr::applicable, Restr::applicable, Restr::any, Literal::f_app }, Restr::any      }, //foldl f z (x:xs) = foldl f (f z x) xs
			{ HaskellFn::foldr       , "ffoldr"    , 4u, { Restr::applicable, Restr::applicable, Restr::any, Literal::f_app }, Restr::any      }, //foldr f z (x:xs) = f x (foldr f z xs) 
			{ PatternFn::value_match , "_VM"       , 3u, { PatternUnsigned{}, Restr::any, Restr::any                        }, Restr::any      }, //layout as in ValueMatch (minus .owner)
			{ PatternFn::of_type     , "_Of_T"     , 2u, { Restr::any, Literal::symbol                                      }, Restr::boolean  },
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
			{ NonComm::ordered_prod   , "prod'"       , true , Restr::any         , Restr::any          },
			{ Comm::sum               , "sum"         , true , Literal::complex   , Literal::complex    },
			{ Comm::prod              , "prod"        , true , Literal::complex   , Literal::complex    },
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
			{ PatternConst::value_proxy  , "_VP"         , PatternConst::value_proxy },
			{ Restr::any                 , "\\"          , Literal::symbol }, //can not be constructed from a string
			{ Restr::applicable          , "applicable"  , Literal::symbol },
			{ Restr::boolean             , "bool"        , Literal::symbol },
			{ Restr::no_value            , "_NoValue"    , Literal::symbol },
			{ Restr::not_neg_1           , "_NotNeg1"    , Literal::symbol },
			{ Restr::not_0               , "_Not0"       , Literal::symbol },
			{ ComplexSubset::natural     , "nat"         , Literal::symbol },
			{ ComplexSubset::natural_0   , "nat_0"       , Literal::symbol },
			{ ComplexSubset::integer     , "int"         , Literal::symbol },
			{ ComplexSubset::real        , "real"        , Literal::symbol },
			{ ComplexSubset::complex     , "_Complex"    , Literal::symbol }, 
			{ ComplexSubset::negative    , "_Negative"   , Literal::symbol }, //can not be constructed from a string
			{ ComplexSubset::positive    , "_Positive"   , Literal::symbol }, //can not be constructed from a string
			{ ComplexSubset::not_negative, "_NotNegative", Literal::symbol }, //can not be constructed from a string
			{ ComplexSubset::not_positive, "_NotPositive", Literal::symbol }, //can not be constructed from a string
			{ Literal::complex           , "complex"     , Literal::symbol },
			{ Literal::symbol            , "symbol"      , Literal::symbol },
			{ Literal::lambda            , "lambda"      , Literal::symbol },
			{ Literal::lambda_param      , "lambda_param", Literal::symbol },
			{ Literal::f_app             , "f_app"       , Literal::symbol },
			{ PatternFApp{}              , "\\"          , Literal::symbol }, //can not be constructed from a string
			{ PatternUnsigned{}          , "_UInt"       , Literal::symbol },
			{ SingleMatch::restricted    , "\\"          , Literal::symbol }, //can not be constructed from a string
			{ SingleMatch::unrestricted  , "\\"          , Literal::symbol }, //can not be constructed from a string
			{ SingleMatch::weak          , "_SingleMatch", Literal::symbol }, //can not be constructed from a string
			{ SpecialMatch::multi        , "_MultiMatch" , Literal::symbol }, //can not be constructed from a string
			{ SpecialMatch::value        , "_ValueMatch" , Literal::symbol }, //can not be constructed from a string
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

		constexpr bool is_lazy(const NodeIndex f)
		{
			return 
				f == literal_true || 
				f == literal_false;
		} //is_lazy
	} //namespace nv



	namespace match {

		//all MatchVariables of same name in pattern (e.g. "a" in pattern "a*b+a" share the same SharedTreeDatum to know 
		//whitch actually matched, and if the name "a" is already matched, even if the current instance is not.
		struct SharedSingleMatchEntry
		{
			NodeIndex match_idx = literal_nullptr; //indexes in Term to simplify
			bool is_set() const noexcept { return this->match_idx != literal_nullptr; } //debugging
		};

		struct SharedValueMatchEntry
		{
			Complex value = std::numeric_limits<double>::quiet_NaN();
			bool is_set() const noexcept { return !std::isnan(this->value.real()); } //debugging
		};

		struct SharedFAppEntry
		{
			//no PatternFApp may have more parameters than max_params_count many
			static constexpr std::size_t max_params_count = 10u;
			static_assert(max_params_count < 16u, "else larger bitsets are needed for PatternCallData");

			using MatchPos_T = decltype(FApp::Info::size);

			//every element in pattern (except all multi match) has own entry which logs, 
			//  with which element in term to match it currently is associated with.
			std::array<MatchPos_T, max_params_count> match_positions;

			NodeIndex match_idx = literal_nullptr; //indexes in Term to simplify (the haystack)

			constexpr SharedFAppEntry() noexcept { this->match_positions.fill(-1u); }

			//checks this->match_positions if needle is contained
			//use with care: might check more than actually contained in specific pattern!
			constexpr bool index_matched(const MatchPos_T needle) const noexcept
			{
				const auto stop = this->match_positions.end();
				return std::find(this->match_positions.begin(), stop, needle) != stop;
			}
		}; //SharedCallEntry

		//range of values matched to a multi mtch variable
		struct MultiRange
		{
			decltype(begin(std::declval<Ref>())) start;
			decltype(end(std::declval<Ref>())) stop;

			constexpr auto begin() const noexcept { return this->start; }
			constexpr auto end() const noexcept { return this->stop; }
		};

		//to allow a constant RewriteRule to be matched against, all match info is stored here
		struct State
		{
			const Store* haystack;

			constexpr State(const Store& haystack_) noexcept :haystack(&haystack_) {}

			constexpr Ref make_ref(const NodeIndex n) const noexcept 
			{	return Ref(*this->haystack, n); 
			}

			//maximal number of unrelated single match variables allowed per pattern
			static constexpr std::size_t max_single_match_count = 8u;
			//maximal number of PatternFApp with Strategy permutation oder dilation allowed per pattern
			static constexpr std::size_t max_pattern_f_app_count = 4u;
			//maximal number of unrelated value match variables allowed per pattern
			static constexpr std::size_t max_value_match_count = 2u;

			std::array<SharedFAppEntry, max_pattern_f_app_count> f_app_entries = {};
			std::array<SharedSingleMatchEntry, max_single_match_count> single_vars = {};
			std::array<SharedValueMatchEntry, max_value_match_count> value_vars = {};

			constexpr auto& value_entry(const ValueMatch& var) noexcept
			{	return this->value_vars[var.match_state_index];
			}

			constexpr auto& value_entry(const ValueMatch& var) const noexcept
			{	return this->value_vars[var.match_state_index];
			}

			constexpr auto& f_app_entry(const UnsaveRef ref) noexcept
			{	assert(ref.type == PatternFApp{});
				return this->f_app_entries[f_app_info(ref).match_state_index];
			}

			constexpr auto& f_app_entry(const UnsaveRef ref) const noexcept
			{	assert(ref.type == PatternFApp{});
				return this->f_app_entries[f_app_info(ref).match_state_index];
			}

			constexpr MultiRange multi_range(const MultiMatch& multi) const noexcept
			{
				assert(multi.index_in_params != -1u); 
				const SharedFAppEntry& owning_entry = this->f_app_entries[multi.match_state_index];
				const Ref matched_ref = this->make_ref(owning_entry.match_idx);
				assert(matched_ref.type == Literal::f_app);
				const FApp& matched_app = *matched_ref;

				const std::uint32_t begin_params_index =
					multi.index_in_params > 0 ?
					//+1u because we only want the thingies after the match listed here
					owning_entry.match_positions[multi.index_in_params - 1u] + 1u :
					0u;
				const std::uint32_t end_params_index =
					owning_entry.match_positions[multi.index_in_params] != SharedFAppEntry::MatchPos_T(-1) ?
					owning_entry.match_positions[multi.index_in_params] :
					matched_app.parameters().size();
				assert(begin_params_index < 10000 && end_params_index < 10000);
				//+ 1u in both cases, because the function itself resides at index 0
				return MultiRange{ { *matched_ref.store, matched_ref.index, begin_params_index + 1u }, { end_params_index + 1u } };
			}
		}; //State

	} //namespace match


	struct RuleHead
	{
		NodeIndex lhs; //match side
		NodeIndex rhs; //replace side
	};

} //namespace simp
