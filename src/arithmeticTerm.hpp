#pragma once

#include <complex>
#include <optional>
#include <compare>
#include <string_view>
#include <array>

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
	//-> advised to add entry to function generality() found in .cpp for these
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

	using Variadic = SumEnum<NonComm, Comm>;

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

	using Function = SumEnum<Fn, NamedFn, Variadic>;

	//the only leaves in MathType
	enum class Literal
	{
		variable,
		complex,
		COUNT
	};

	using MathType = SumEnum<Literal, Function>;


	//not expeced to be present in normal Term, only in Patterns
	enum class PnNode 
	{ 
		value_match, //real nodes appearing as elements in the store, just as everything of MathType
		value_proxy, //not actual node in tree, just "end" indicator for value subtrees
		tree_match,  //real nodes appearing as elements in the store, just as everything of MathType
		COUNT 
	};

	//not expeced to be present in normal Term, only in Patterns
	//represent not actual nodes in pattern tree, as all match info is stored in VariadicMatchDatum in MatchData	
	//thus all info required in the tree is given in the typed_idx, where the index is repurposed to point elsewhere
	//(elsewhere means that VariadicMatchDatum array in MatchData)
	enum class MultiPn 
	{ 
		params, //only of MultiPn allowed in valid pattern on lhs (exchanged in RewriteRule's constructor)
		summands, //only expected at rhs of valid pattern, to allow a sum (of all summands) as factor
		factors,  //only expected at rhs of valid pattern, to allow a product (of all factors) as summand
		COUNT 
	};

	using MatchType = SumEnum<MultiPn, PnNode>;


	using Type = SumEnum<MatchType, MathType>;

	using TypedIdx = BasicTypedIdx<Type>;

	using IndexVector = StoredVector<TypedIdx>;
	using CharVector = StoredVector<char>;
	using Complex = std::complex<double>;

	namespace pattern {

		enum class Restr
		{
			any,
			nn1, //compact for "not negative one" (basically any, but the exact term "-1" will not be accepted)
			no_val, //basically any, but Literal::complex is forbidden    
			function, //packs Variadic, NamedFn and Fn together
			COUNT
		};

		//note: of Type, only sum, product, complex or variable may be used, as there is (currently)
		//  no need to differentiate between any of the functions of Fn and unknown_function.
		using Restriction = SumEnum<Restr, MathType>; 

		//in a valid pattern, all TreeMatchVariables of same name share the same restr and the same match_data_idx.
		//it is allowed to have multiple instances of the same TreeMatchVariable per side.
		//this variation of the match variable can match a subtree occuring in a term
		struct TreeMatchVariable
		{
			std::uint32_t match_data_idx; //indexes in MatchData::tree_match_data
			Restriction restr = pattern::Restr::any;
		};

		//although the type MultiMatchVariable does not exist as type of node in term, other nodes may reference it
		// (now called PnVariable::summands or PnVariable::factors)
		//Meaning: if some TypedIdx has .get_type() == PnVariable::summands, all data of this node are stored in MatchData at
		//  .get_index(), not in the pattern tree (same goes for .get_type() == PnVariable::factors).
		//The concept of a (imagined) MultiMatchVariable is quite similar to TreeMatchVariable with Restr::any, 
		//but it can match multiple summands / factors in same sum / product, not only a single one.
		//This behavor can kinda be simulated by TreeMatchVariable, if all matched elements of MultiMatchVariable are packaged 
		//  together in an extra sum / product.
		//as match::permutation_equals takes a Ref, not a MutRef however, this approach can not be realized here (and is stupid anyway)
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
		//in praxis a TypedIdx with .get_type() == PnVariable::value_proxy works similar to (imagined) MultiMatchVariable, as
		//  .get_index() also does not point in store of pattern, but in an array of MatchData.
		struct ValueMatchVariable
		{
			TypedIdx mtch_idx;
			TypedIdx copy_idx;
			std::uint32_t match_data_idx; //indexes in MatchData::value_match_data
			Form form = Form::real;

			[[deprecated]] constexpr ValueMatchVariable(std::uint32_t new_match_data_idx, Form new_form) noexcept
				:mtch_idx(TypedIdx(new_match_data_idx, PnNode::value_proxy)),
				copy_idx(TypedIdx(new_match_data_idx, PnNode::value_proxy)),
				match_data_idx(new_match_data_idx), form(new_form)
			{}

			constexpr ValueMatchVariable(TypedIdx new_match, TypedIdx new_copy, std::uint32_t new_match_data_idx, Form new_form) noexcept
				:mtch_idx(new_match), copy_idx(new_copy), match_data_idx(new_match_data_idx), form(new_form)
			{}
		};



		struct UnknownPnVar :SingleSumEnumEntry {};

		//intermediary used in build process
		using PnVariablesType = SumEnum<Restriction, Form, MultiPn, UnknownPnVar>;

		struct TypeProps
		{
			PnVariablesType type = PnVariablesType(UnknownPnVar{});
			std::string_view name = "";
		};

		constexpr auto type_table = std::to_array<TypeProps>({
			{ Comm::sum           , "sum"           },
			{ Comm::product       , "product"       },
			{ Literal::variable   , "variable"      },
			{ Literal::complex    , "value"         }, //not to be mistaken for Form::complex
			{ Restr::function     , "fn"            },
			{ Form::natural       , "nat"           },
			{ Form::natural_0     , "nat0"          },
			{ Form::integer       , "int"           },
			{ Form::real          , "real"          },
			{ Form::complex       , "complex"       }, //not to be mistaken for Literal::complex
			{ Form::negative      , "negative"      },
			{ Form::not_negative  , "not_negative"  },
			{ Form::positive      , "positive"      },
			{ Form::not_positive  , "not_positive"  },
			{ Restr::any          , "any"           },
			{ Restr::nn1          , "nn1"           },
			{ Restr::no_val       , "no_val"        },
			{ MultiPn::summands   , "summands"      },
			{ MultiPn::factors    , "factors"       },
			{ MultiPn::params     , "params"        },
			});

		constexpr std::string_view name_of(const PnVariablesType r) noexcept { return find(type_table, &TypeProps::type, r).name; }
		constexpr PnVariablesType type_of(const std::string_view s) noexcept { return search(type_table, &TypeProps::name, s).type; }

	} //namespace pattern
	
	//if one pattern may compare equal to a term multiple ways (e.g. sum or product), it has high generality.
	//there are 3 main levels of generality:
	//  - unique (value 1xxx): pattern only matches one exact term e.g. pattern "2 'pi'" 
	//                         (note: everything without pattern variables falls tecnically in this category)
	//  - low    (value 2xxx): pattern is recursive, but has strong operands order (e.g. all in Fn), 
	//                         thus matches unique on outhermost level, but may hold general operands
	//  - high   (value 3xxx): sums / products containing pattern variables can match not only multiple terms, 
	//                         but may also match specific terms in more than one way (also tree variables, duh)
	//the table only differentiates between types, however (as described above) the real generality of a given term may be lower, 
	//  than that of its outermost node listed here.
	//as the goal of this endavour is (mostly) to sort the most general summands / factors in a pattern to the end, 
	//  the sorting required for efficiently matching patterns may use this table, but has to check more.
	constexpr int generality(Type type) noexcept 
	{ 
		constexpr auto type_generality_table = std::to_array<std::pair<Type, int>>({
			{ Type(Literal::complex     ), 1000 }, 
			{ Type(Literal::variable    ), 1001 },
			{ Type(PnNode::value_match  ), 1002 }, //may match different subsets of complex numbers, but always requires an actual value to match against
			{ Type(PnNode::value_proxy  ), 1003 }, //dont care really where this sits, as it never ist used in matching anyway
			//values 2xxx are not present, as that would require every item in Fn to be listed here (instead default_generality kicks in here)
			{ Type(Comm::multiset       ), 3002 },  
			{ Type(Comm::set            ), 3003 },  
			{ Type(Comm::sum            ), 3004 },  
			{ Type(Comm::product        ), 3005 }, 
			{ Type(PnNode::tree_match   ), 3006 }, 
			{ Type(MultiPn::params      ), 3007 }, //kinda special, as they always succeed in matching -> need to be matched last 
			{ Type(MultiPn::summands    ), 3008 }, //kinda special, as they always succeed in matching -> need to be matched last 
			{ Type(MultiPn::factors     ), 3009 }, //kinda special, as they always succeed in matching -> need to be matched last 
			{ Type(NamedFn{})            , 3010 },
		});
		static_assert(std::is_sorted(type_generality_table.begin(), type_generality_table.end(), 
			[](auto a, auto b) { return a.second < b.second; }));
		static_assert(static_cast<unsigned>(Type::COUNT) < 1000u, 
			"else the 2xxx generalities may leak into the 3xxx ones in table");

		const std::pair<Type, int> default_generality = { Type(0u), static_cast<unsigned>(type) + 2000 };
		return search(type_generality_table, &std::pair<Type, int>::first, type, default_generality).second; 
	}



	union MathUnion
	{
		Complex complex;
		IndexVector parameters; //all in Variadic and all in Fn
		CharVector char_vec;
		pattern::TreeMatchVariable tree_match;    //only expected as part of pattern
		pattern::ValueMatchVariable value_match;  //only expected as part of pattern

		constexpr MathUnion(const Complex                    & val) noexcept :complex(val)     {}
		constexpr MathUnion(const IndexVector                & val) noexcept :parameters(val)  {}
		constexpr MathUnion(const CharVector                 & val) noexcept :char_vec(val)    {} 
		constexpr MathUnion(const pattern::TreeMatchVariable & val) noexcept :tree_match(val)  {} 
		constexpr MathUnion(const pattern::ValueMatchVariable& val) noexcept :value_match(val) {} 
		constexpr MathUnion()                                       noexcept :complex(0.0)     {} 

		constexpr auto operator<=>(const MathUnion&) const = default;

		constexpr operator const Complex                     &() const noexcept { return this->complex;     }
		constexpr operator const IndexVector                 &() const noexcept { return this->parameters;  }
		constexpr operator const CharVector                  &() const noexcept { return this->char_vec;    }
		constexpr operator const pattern::TreeMatchVariable  &() const noexcept { return this->tree_match;  }
		constexpr operator const pattern::ValueMatchVariable &() const noexcept { return this->value_match; }

		constexpr operator Complex                     &() noexcept { return this->complex;     }
		constexpr operator IndexVector                 &() noexcept { return this->parameters;  }
		constexpr operator CharVector                  &() noexcept { return this->char_vec;    }
		constexpr operator pattern::TreeMatchVariable  &() noexcept { return this->tree_match;  }
		constexpr operator pattern::ValueMatchVariable &() noexcept { return this->value_match; }
	};

	static_assert(sizeof(MathUnion) * 8 == 128);
	using MathStore = BasicStore<MathUnion>;


	template<typename T>
	concept MathReference = Reference<T> && T::is_const && std::is_same_v<typename T::value_type, MathUnion>;

	template<typename T>
	concept MutMathReference = Reference<T> && !T::is_const && std::is_same_v<typename T::value_type, MathUnion>;


	using Ref = BasicSaveRef<Type, const MathStore>;
	using UnsaveRef = BasicUnsaveRef<Type, MathUnion>;
	using MutRef = BasicSaveRef<Type, MathStore>;

	static_assert(MathReference<Ref>);
	static_assert(MathReference<UnsaveRef>);
	static_assert(MutMathReference<MutRef>);


	//utility for NamedFn, types in Fn and types in Variadic
	namespace fn {
		OptComplex eval(Fn type, const std::array<OptComplex, 4>& param_vals);

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

		constexpr std::size_t arity(const Type type) noexcept 
		{ return fn_props_table[static_cast<unsigned>(type.to<Fn>())].arity; }

		struct VariadicProperties
		{
			Variadic type = Variadic::COUNT;
			std::string_view name = "";

			bool associative = false; //allows to flatten nested instances if true
		};

		//every item enumerated in Variadic (except COUNT, duh) may be listed here in order of apperance in Variadic
		constexpr auto variadic_props_table = std::to_array<VariadicProperties>({
			{ Comm::sum               , "sum"         , true  },
			{ Comm::product           , "product"     , true  },
			{ Comm::multiset          , "multiset"    , false },
			{ Comm::set               , "set"         , false },
			{ Comm::union_            , "union"       , true  },
			{ Comm::intersection      , "intersection", true  },
			{ NonComm::list           , "list"        , false },
			{ NonComm::ordered_sum    , "sum'"        , true  },
			{ NonComm::ordered_product, "product'"    , true  },
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

		constexpr bool is_associative(const Type type) noexcept 
		{ return is_associative(type.to<Variadic>()); }



		constexpr auto&        range(const UnsaveRef ref) noexcept { return ref->parameters; }
		constexpr auto&        range(const       Ref ref) noexcept { return ref->parameters; }
		constexpr auto    save_range(const       Ref ref) noexcept { return ref.cast<IndexVector>(); }
		constexpr auto         range(const    MutRef ref) noexcept { return ref.cast<IndexVector>(); }
		constexpr auto& unsave_range(const    MutRef ref) noexcept { return ref->parameters; }


		template<typename Reference>
		constexpr std::uint32_t named_fn_name_index(const Reference named_fn) noexcept
		{
			const IndexVector& parameters = *named_fn;
			return named_fn.index + parameters.node_count();
		}

		template<typename Reference>
		constexpr const CharVector& named_fn_name(const Reference named_fn) noexcept
		{
			return static_cast<const CharVector&>(named_fn.store->at(named_fn_name_index(named_fn)));
		}

		constexpr const CharVector& named_fn_name(const UnsaveRef named_fn) noexcept
		{
			return static_cast<const CharVector&>(*named_fn.raw_at(named_fn_name_index(named_fn)));
		}

		template<StoreLike Store_T, typename Parameters_T>
		inline TypedIdx build_named_fn(Store_T& store, const std::string_view name, const Parameters_T& params)
		{
			const std::size_t parameter_capacity = IndexVector::smallest_fit_capacity(params.size());
			const std::size_t nr_parameter_nodes = IndexVector::_node_count(parameter_capacity);

			const std::size_t name_capacity = CharVector::smallest_fit_capacity(name.size());
			const std::size_t nr_name_nodes = CharVector::_node_count(name_capacity);

			const std::size_t result_index = store.allocate_n(nr_parameter_nodes + nr_name_nodes);
			IndexVector::emplace(store.at(result_index), params, parameter_capacity);
			CharVector::emplace(store.at(result_index + nr_parameter_nodes), name, name_capacity);
			return TypedIdx(result_index, Type(NamedFn{}));
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
		[[nodiscard]] TypedIdx combine(const MutRef ref, const bool exact);

		//compares two subterms of perhaps different stores, assumes both to have their parameters parts sorted
		[[nodiscard]] std::strong_ordering compare(const UnsaveRef ref_1, const UnsaveRef ref_2);

		//sorts parameters parts by compare
		void sort(const MutRef ref);

		//counts number of logical nodes of subtree (note: logical nodes might be fewer than used slots in store)
		std::size_t count(const UnsaveRef ref);

		//copies subtree starting at src_ref into dst_store and returns its head
		[[nodiscard]] TypedIdx copy(const Ref src_ref, MathStore& dst_store);

		//returns true iff subtree starting at ref contains to_contain (or is to_contain itself)
		bool contains(const UnsaveRef ref, const TypedIdx to_contain);

		bool contains_variables(const UnsaveRef ref);

		//returns TypedIdx() if unsuccsessfull
		TypedIdx search_variable(const UnsaveRef ref, const std::string_view name);

		//calls first tree::combine, then tree::sort
		[[nodiscard]] TypedIdx establish_basic_order(MutRef ref);

		//returns pointer to field of parent of subtree, where subtree is held
		//only the non-const return value requires this function to not take a const store, 
		//  else handing in a reference as head might not be desired.
		TypedIdx* find_subtree_owner(MathStore& store, TypedIdx& head, const TypedIdx subtree);

		//expects ref to be head of term and ref.store to house ref exclusively 
		//(-> every position in store eighter free or used by (children of) ref exactly once)
		bool valid_storage(const Ref ref);

	} //namespace tree

}	//namespace bmath::intern

namespace bmath {

	namespace intern::pattern {
		struct RewriteRule;
	}

	class Term
	{
	public:
		intern::MathStore store;
		intern::TypedIdx head;

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
		bool match_and_replace(const intern::pattern::RewriteRule& p) noexcept; //returns true if match was found 
	};	//class Term

}	//namespace bmath



