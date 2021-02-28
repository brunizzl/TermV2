#pragma once

#include <concepts>
#include <type_traits>
#include <array>
#include <string>
#include <tuple>

#include "utility/meta.hpp"
#include "utility/stringLiteral.hpp"

#include "arithmeticTerm.hpp"
#include "pattern.hpp"

#include "ioArithmetic.hpp"

namespace bmath::intern::meta_pn {

	struct PatternMarker {};

	template<typename T>
	concept Pattern = std::derived_from<T, PatternMarker>;



	struct PredicateMarker {};

	template<typename T>
	concept Predicate = std::derived_from<T, PredicateMarker>;



	template<Pattern MatchPattern, Pattern ReplacePattern, Predicate... Conditions>
	struct Rule {};




	/////////////////////////////Definitions of Patterns
	//----------------------------------------------------------------------------------------------------------------------
	//----------------------------------------------------------------------------------------------------------------------

	/////////// recursive pattern components

	template<typename Category, Category Type, std::size_t MatchDataIndex, meta::ListInstance Operands> //MatchDataIdx only relevant for variadic
	struct FunctionPn :PatternMarker
	{
		static_assert(std::is_convertible_v<Category, Function>);
		static_assert(!std::is_same_v<Category, Fn> || fn::arity(Type) == meta::size_v<Operands>);

		template<Pattern Rhs> 
		constexpr Rule<FunctionPn, Rhs> operator=(Rhs) { return {}; }
	};


	template<Comm Type, std::size_t MatchDataIdx, Pattern... Operands>
	using CommutativePn = FunctionPn<Comm, Type, MatchDataIdx, meta::List<Operands...>>;

	template<NonComm Type, std::size_t MatchDataIdx, Pattern... Operands>
	using NonCommutativePn = FunctionPn<NonComm, Type, MatchDataIdx, meta::List<Operands...>>;

	template<Fn Type, Pattern... Operands>
	using FnPn = FunctionPn<Fn, Type, 0, meta::List<Operands...>>;



	/////////// non-recursive pattern components

	template<std::size_t MatchDataIndex>
	struct TreeMatchVariable :PatternMarker {};

	template<typename T>
	struct IsTreeMatchVariable :std::false_type {};

	template<std::size_t MatchDataIndex>
	struct IsTreeMatchVariable<TreeMatchVariable<MatchDataIndex>> :std::true_type {};

	template<char... Cs>
	constexpr auto operator "" _tree()
	{
		constexpr unsigned long long match_data_idx = parse_ull(std::to_array({ Cs... }));
		static_assert(match_data_idx < pattern::match::MatchData::max_tree_match_count);
		return TreeMatchVariable<match_data_idx>{};
	}



	template<std::size_t MatchDataIndex>
	struct MultiMatchVariable :PatternMarker {};

	template<std::size_t ID>
	struct MultiMatchTemp :PatternMarker {};

	template<char... Cs>
	constexpr auto operator "" _multi()
	{
		constexpr unsigned long long match_data_idx = parse_ull(std::to_array({ Cs... }));
		static_assert(match_data_idx < pattern::match::MatchData::max_variadic_count);
		return MultiMatchTemp<match_data_idx>{};
	}

	namespace detail_variables {
		template<std::size_t... Is>
		constexpr auto make_trees(std::index_sequence<Is...>)
		{
			static_assert(sizeof...(Is) <= pattern::match::MatchData::max_tree_match_count);
			return std::make_tuple(TreeMatchVariable<Is>{} ...);
		}

		template<std::size_t... Is>
		constexpr auto make_multis(std::index_sequence<Is...>)
		{
			static_assert(sizeof...(Is) <= pattern::match::MatchData::max_variadic_count);
			return std::make_tuple(MultiMatchTemp<Is>{} ...);
		}
	} //namespace detail_variables

	template<std::size_t N>
	constexpr auto make_tree_matches = detail_variables::make_trees(std::make_index_sequence<N>{});

	template<std::size_t N>
	constexpr auto make_multi_matches = detail_variables::make_multis(std::make_index_sequence<N>{});



	template<char... Name>
	struct VariablePn :PatternMarker { static_assert(sizeof...(Name) > 0); };


	template<double Re, double Im>
	struct ComplexPn :PatternMarker {};


	//this behavior makes the MakeComplex_t function necessairy
	static_assert(!std::is_same_v<ComplexPn<-0.0, 0.0>, ComplexPn<0.0, 0.0>>);

	template<double Re, double Im>
	struct MakeComplex { using type = ComplexPn<Re, Im>; };

	template<double Re, double Im>
	using MakeComplex_t = typename MakeComplex<Re, Im>::type;

	template<double Re>
	struct MakeComplex<Re, -0.0> { using type = ComplexPn<Re, 0.0>; };

	template<double Im>
	struct MakeComplex<-0.0, Im> { using type = ComplexPn<0.0, Im>; };

	template<>
	struct MakeComplex<-0.0, -0.0> { using type = ComplexPn<0.0, 0.0>; };



	template<char... Cs>
	constexpr ComplexPn<parse_double(std::to_array({ Cs... })), 0.0> operator "" _() { return {}; }

	template<char... Cs>
	constexpr ComplexPn<0.0, parse_double(std::to_array({ Cs... }))> operator "" _i() { return {}; }




	/////////////////////////////Ease pattern construction syntax
	//----------------------------------------------------------------------------------------------------------------------
	//----------------------------------------------------------------------------------------------------------------------

#define BMATH_DEFINE_FUNCTION(type, name) \
template<Pattern... Ops> constexpr FunctionPn<type, type::name, 0, meta::List<Ops...>> name(Ops...) { return {}; }

	BMATH_DEFINE_FUNCTION(Comm, set)
	BMATH_DEFINE_FUNCTION(Comm, multiset)
	BMATH_DEFINE_FUNCTION(Comm, union_)
	BMATH_DEFINE_FUNCTION(Comm, intersection)

	BMATH_DEFINE_FUNCTION(NonComm, list)
	BMATH_DEFINE_FUNCTION(NonComm, ordered_sum)
	BMATH_DEFINE_FUNCTION(NonComm, ordered_product)

	BMATH_DEFINE_FUNCTION(Fn, pow)
	BMATH_DEFINE_FUNCTION(Fn, log)
	BMATH_DEFINE_FUNCTION(Fn, sqrt)
	BMATH_DEFINE_FUNCTION(Fn, exp)
	BMATH_DEFINE_FUNCTION(Fn, ln)
	BMATH_DEFINE_FUNCTION(Fn, sin)
	BMATH_DEFINE_FUNCTION(Fn, cos)
	BMATH_DEFINE_FUNCTION(Fn, tan)
	BMATH_DEFINE_FUNCTION(Fn, sinh)
	BMATH_DEFINE_FUNCTION(Fn, cosh)
	BMATH_DEFINE_FUNCTION(Fn, tanh)
	BMATH_DEFINE_FUNCTION(Fn, asin)
	BMATH_DEFINE_FUNCTION(Fn, acos)
	BMATH_DEFINE_FUNCTION(Fn, atan)
	BMATH_DEFINE_FUNCTION(Fn, asinh)
	BMATH_DEFINE_FUNCTION(Fn, acosh)
	BMATH_DEFINE_FUNCTION(Fn, atanh)
	BMATH_DEFINE_FUNCTION(Fn, abs)
	BMATH_DEFINE_FUNCTION(Fn, arg)
	BMATH_DEFINE_FUNCTION(Fn, re)
	BMATH_DEFINE_FUNCTION(Fn, im)
	BMATH_DEFINE_FUNCTION(Fn, force)
	BMATH_DEFINE_FUNCTION(Fn, diff)


	using MinusOne = ComplexPn<-1.0, 0.0>;

	template<Pattern... Operands>
	using Sum = CommutativePn<Comm::sum, 0, Operands...>;

	template<Pattern... Operands>
	using Product = CommutativePn<Comm::product, 0, Operands...>;

	template<Pattern Base, Pattern Expo>
	using Pow = FnPn<Fn::pow, Base, Expo>;


	/////////// operator+

	template<Pattern Lhs, Pattern Rhs>
	struct Plus { using type = Sum<Lhs, Rhs>; };
	
	template<Pattern... Ops, Pattern Rhs>
	struct Plus<Sum<Ops...>, Rhs> { using type = Sum<Ops..., Rhs>; };

	template<Pattern Lhs, Pattern... Ops>
	struct Plus<Lhs, Sum<Ops...>> { using type = Sum<Lhs, Ops...>; };

	template<Pattern... LOps, Pattern... ROps>
	struct Plus<Sum<LOps...>, Sum<ROps...>> { using type = Sum<LOps..., ROps...>; };

	template<double Re1, double Im1, double Re2, double Im2>
	class Plus<ComplexPn<Re1, Im1>, ComplexPn<Re2, Im2>> 
	{
		static constexpr double new_re = Re1 + Re2;
		static constexpr double new_im = Im1 + Im2;
	public:
		using type = MakeComplex_t<new_re, new_im>; 
	};

	template<Pattern Lhs, Pattern Rhs>
	using Plus_t = typename Plus<Lhs, Rhs>::type;

	template<Pattern Lhs, Pattern Rhs> 
	constexpr Plus_t<Lhs, Rhs> operator+(Lhs, Rhs) { return {}; }


	/////////// operator*

	template<Pattern Lhs, Pattern Rhs>
	struct Times { using type = Product<Lhs, Rhs>; };

	template<Pattern... Ops, Pattern Rhs>
	struct Times<Product<Ops...>, Rhs> { using type = Product<Ops..., Rhs>; };

	template<Pattern Lhs, Pattern... Ops>
	struct Times<Lhs, Product<Ops...>> { using type = Product<Lhs, Ops...>; };

	template<Pattern... LOps, Pattern... ROps>
	struct Times<Product<LOps...>, Product<ROps...>> { using type = Product<LOps..., ROps...>; };

	template<double Re1, double Im1, double Re2, double Im2>
	class Times<ComplexPn<Re1, Im1>, ComplexPn<Re2, Im2>> 
	{
		static constexpr double new_re = Re1 * Re2 - Im1 * Im2;
		static constexpr double new_im = Re1 * Im2 + Im1 * Re2;
	public:
		using type = MakeComplex_t<new_re, new_im>;
	};

	template<Pattern Lhs, Pattern Rhs>
	using Times_t = typename Times<Lhs, Rhs>::type;

	template<Pattern Lhs, Pattern Rhs>
	constexpr Times_t<Lhs, Rhs> operator*(Lhs, Rhs) { return {}; }


	/////////// operator-

	template<Pattern P>
	struct Minus { using type = Times_t<P, MinusOne>; };

	template<double Re, double Im>
	class Minus<ComplexPn<Re, Im>>
	{
		static constexpr double new_re = -Re;
		static constexpr double new_im = -Im;
	public:
		using type = MakeComplex_t<new_re, new_im>;
	};

	template<Pattern P>
	using Minus_t = typename Minus<P>::type;

	template<Pattern P>
	constexpr Minus_t<P> operator-(P) { return {}; }

	template<Pattern Lhs, Pattern Rhs>
	constexpr Plus_t<Lhs, Minus_t<Rhs>> operator-(Lhs, Rhs) { return {}; }


	/////////// operator/

	template<Pattern P>
	struct Divide { using type = Pow<P, MinusOne>; };

	template<double Re, double Im>
	class Divide<ComplexPn<Re, Im>>
	{
		static constexpr double abs_squared = Re * Re + Im * Im;
		static constexpr double inv_re = Re / abs_squared;
		static constexpr double inv_im = -Im / abs_squared;
	public:
		using type = MakeComplex_t<inv_re, inv_im>;
	};

	template<Pattern P>
	using Divide_t = typename Divide<P>::type;

	template<Pattern Lhs, Pattern Rhs> 
	constexpr Times_t<Lhs, Divide_t<Rhs>> operator/(Lhs, Rhs) { return {}; }


	/////////// operator^

	template<Pattern Lhs, Pattern Rhs>
	constexpr Pow<Lhs, Rhs> operator^(Lhs, Rhs) { return {}; }




	/////////////////////////////Sorting Pattern
	//----------------------------------------------------------------------------------------------------------------------
	//----------------------------------------------------------------------------------------------------------------------


	/////////// generality

	template<Pattern>
	struct Generality;

	template<typename Category, Category Type, std::size_t MatchDataIndex, meta::ListInstance Operands>
	struct Generality<FunctionPn<Category, Type, MatchDataIndex, Operands>> :meta::Constant<generality((MathType)Type)> {};

	template<std::size_t MatchDataIndex>
	struct Generality<TreeMatchVariable<MatchDataIndex>> :meta::Constant<generality(pattern::TreeMatchNonOwning{})> {};

	template<std::size_t ID>
	struct Generality<MultiMatchTemp<ID>> :meta::Constant<generality(pattern::MultiParams{})> {};

	template<double Re, double Im>
	struct Generality<ComplexPn<Re, Im>> :meta::Constant<generality(Literal::complex)> {};

	template<char... Cs>
	struct Generality<VariablePn<Cs...>> :meta::Constant<generality(Literal::variable)> {};

	template<Pattern P> 
	constexpr int generality_v = Generality<P>::value;


	/////////// compare

	template<Pattern P1, Pattern P2>
	struct ComparePatterns :std::bool_constant<(generality_v<P1> < generality_v<P2>)> {};

	template<Pattern P1, Pattern P2>
	using ComparePatternsIndirection = ComparePatterns<P1, P2>;
	
	template<typename Category, Category Type, std::size_t LIdx, std::size_t RIdx, meta::ListInstance LOps, meta::ListInstance ROps>
	struct ComparePatterns<
		FunctionPn<Category, Type, LIdx, LOps>, 
		FunctionPn<Category, Type, RIdx, ROps>
	> :std::bool_constant<meta::smaller_v<LOps, ROps, ComparePatternsIndirection>> {};

	template<std::size_t Idx1, std::size_t Idx2>
	struct ComparePatterns<TreeMatchVariable<Idx1>, TreeMatchVariable<Idx2>> :std::bool_constant<(Idx1 < Idx2)> {};

	template<std::size_t ID1, std::size_t ID2>
	struct ComparePatterns<MultiMatchTemp<ID1>, MultiMatchTemp<ID2>> :std::bool_constant<(ID1 < ID2)> {};

	template<double Re1, double Im1, double Re2, double Im2>
	struct ComparePatterns<ComplexPn<Re1, Im1>, ComplexPn<Re2, Im2>>
		:std::bool_constant<(compare_complex(Complex{ Re1, Im1 }, Complex{ Re2, Im2 }) == std::strong_ordering::less)> {};



	/////////// sort and combine operands

	template<Pattern P>
	struct OrderPattern { using type = P; };

	template<Pattern P>
	using OrderPattern_t = typename OrderPattern<P>::type;

	//indirection necessairy, as otherwise local argument is implicitly inserted to OrderPattern template
	// in recursion call of OrderPattern
	template<meta::ListInstance Operands>
	using OrderEachOperand_t = meta::Map_t<OrderPattern, Operands>;


	template<Comm Type, meta::ListInstance Operands>
	struct OperandsToOperation { using type = FunctionPn<Comm, Type, 0, Operands>; };

	template<Comm Type, meta::ListInstance Operands>
	using OperandsToOperation_t = typename OperandsToOperation<Type, Operands>::type;

	template<typename Op1, typename... Ops>
	struct OperandsToOperation<Comm::sum, meta::List<Op1, Ops...>> 
	{ 
		using type = meta::Foldl_t<Plus, Op1, meta::List<Ops...>>; 
	};

	template<typename Op1, typename... Ops>
	struct OperandsToOperation<Comm::product, meta::List<Op1, Ops...>> 
	{ 
		using type = meta::Foldl_t<Times, Op1, meta::List<Ops...>>; 
	};


	template<Comm Type, meta::ListInstance Operands>
	class OrderPattern<FunctionPn<Comm, Type, 0, Operands>>
	{
		using IndividuallyOrderedOperands = OrderEachOperand_t<Operands>;
		using SortedOperands              = meta::Sort_t<IndividuallyOrderedOperands, ComparePatterns>;
	public:
		using type = OperandsToOperation_t<Type, SortedOperands>;
	};

	template<typename Category, Category Type, meta::ListInstance Operands>
	struct OrderPattern<FunctionPn<Category, Type, 0, Operands>>
	{
		using type = FunctionPn<Category, Type, 0, OrderEachOperand_t<Operands>>;
	};

	/////////// set MatchDataIndex in multimatch and variadic to correct value and surround multimatch in rhs with correct variadic
	namespace detail_arm_variadic {

		template<auto EnclosingType, std::size_t ID, std::size_t MatchDataIdx>
		struct MultiMatchBuidingDatum 
		{
			static constexpr auto enclosing_type = EnclosingType;
			static constexpr std::size_t id = ID;
			static constexpr std::size_t index = MatchDataIdx;
		};



		/////////// arm left hand side of rule

		template<Pattern P, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx>
		struct ArmMatchSideResult 
		{
			using ResultPattern = P;
			using BuildingData = MultiMatchBuildingData;
			static constexpr std::size_t index = NxtIdx;
		};

		template<Pattern P, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx>
		struct ArmMatchSide { using type = ArmMatchSideResult<P, MultiMatchBuildingData, NxtIdx>; };

		template<Pattern P, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx>
		using ArmMatchSide_t = typename ArmMatchSide<P, MultiMatchBuildingData, NxtIdx>::type;

		template<meta::ListInstance ArmedOperands, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx>
		struct ArmOperandAccumulator
		{
			using Operands = ArmedOperands;
			using BuildingData = MultiMatchBuildingData;
			static constexpr std::size_t index = NxtIdx;
		};

		template<auto EnclosingVariadicType, std::size_t EnclosingVariadicIndex>
		struct ArmOperand
		{
			template<typename LastResult, Pattern Operand>
			struct Call;

			template<meta::ListInstance ArmedOperands, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx, Pattern Operand>
			class Call<ArmOperandAccumulator<ArmedOperands, MultiMatchBuildingData, NxtIdx>, Operand>
			{
				using ArmedOperandData = ArmMatchSide_t<Operand, MultiMatchBuildingData, NxtIdx>;
			public:
				using type = ArmOperandAccumulator<
					meta::Concat_t<ArmedOperands, meta::List<typename ArmedOperandData::ResultPattern>>,
					typename ArmedOperandData::BuildingData,
					ArmedOperandData::index
				>;
			};

			template<meta::ListInstance ArmedOperands, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx, std::size_t ID>
			struct Call<ArmOperandAccumulator<ArmedOperands, MultiMatchBuildingData, NxtIdx>, MultiMatchTemp<ID>>
			{
				static_assert(Function(EnclosingVariadicType).is<Variadic>(), 
					"only variadic operations may contain multi-match-variables");
				using type = ArmOperandAccumulator<
					meta::Concat_t<ArmedOperands, meta::List<MultiMatchVariable<EnclosingVariadicIndex>>>,
					meta::Cons_t<MultiMatchBuidingDatum<EnclosingVariadicType, ID, EnclosingVariadicIndex>, MultiMatchBuildingData>,
					NxtIdx
				>;
			};
		};

		template<typename Category, Category Type, meta::ListInstance Operands, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx>
		class ArmMatchSide<FunctionPn<Category, Type, 0, Operands>, MultiMatchBuildingData, NxtIdx>
		{
			static constexpr bool arming_variadic   = Function(Type).is<Variadic>();
			static constexpr std::size_t this_index = arming_variadic ? NxtIdx : 0;
			static constexpr std::size_t next_index = arming_variadic ? this_index + 1 : NxtIdx;

			using OperandsResult = meta::Foldl_t<
				typename ArmOperand<Type, this_index>::Call,
				ArmOperandAccumulator<meta::List<>, MultiMatchBuildingData, next_index>,
				Operands
			>;
		public:
			using type = ArmMatchSideResult<
				FunctionPn<Category, Type, this_index, typename OperandsResult::Operands>,
				typename OperandsResult::BuildingData,
				OperandsResult::index
			>;
		};

		static_assert(std::is_same_v<
			ArmMatchSide_t<FunctionPn<Comm, Comm::sum, 0, meta::List<>>, meta::List<>, 3>,
			ArmMatchSideResult<FunctionPn<Comm, Comm::sum, 3, meta::List<>>, meta::List<>, 4>
		>);

		static_assert(std::is_same_v<
			ArmMatchSide_t<
				FunctionPn<Comm, Comm::sum, 0, meta::List<decltype(-2_), MultiMatchTemp<8>>>, 
				meta::List<>, 
				3>,
			ArmMatchSideResult<
				FunctionPn<Comm, Comm::sum, 3, meta::List<decltype(-2_), MultiMatchVariable<3>>>,
				meta::List<MultiMatchBuidingDatum<Comm::sum, 8, 3>>, 
				4>
		>);




		/////////// arm right hand side of rule

		template<Pattern P, auto ParentType, meta::ListInstance MultiMatchBuildingData>
		struct ArmReplaceSide { using type = P; };

		template<Pattern P, auto ParentType, meta::ListInstance MultiMatchBuildingData>
		using ArmReplaceSide_t = typename ArmReplaceSide<P, ParentType, MultiMatchBuildingData>::type;

		template<std::size_t ID, auto ParentType, meta::ListInstance MultiMatchBuildingData>
		class ArmReplaceSide<MultiMatchTemp<ID>, ParentType, MultiMatchBuildingData>
		{
			template<typename BuildingDatum>
			struct IsThisBuildingDatum :std::bool_constant<BuildingDatum::id == ID> {};

			using ThisBuildingDatum = meta::Find_t<IsThisBuildingDatum, MultiMatchBuildingData>;
			static_assert(!std::is_same_v<ThisBuildingDatum, meta::FoundNothing>, 
				"only multi-match-variables present in match side may occur in replacement side.");

			using UnwrappedResult = MultiMatchVariable<ThisBuildingDatum::index>;
			using WrappedResult = FunctionPn<
				std::remove_cv_t<decltype(ThisBuildingDatum::enclosing_type)>,
				ThisBuildingDatum::enclosing_type,
				0,
				meta::List<UnwrappedResult>>;
			static constexpr bool parent_is_enclosing_type = MathType(ThisBuildingDatum::enclosing_type) == MathType(ParentType);
		public:
			using type = std::conditional_t<parent_is_enclosing_type, UnwrappedResult, WrappedResult>;
		};

		template<typename Category, Category Type, meta::ListInstance Operands, auto ParentType, meta::ListInstance MultiMatchBuildingData>
		class ArmReplaceSide<FunctionPn<Category, Type, 0, Operands>, ParentType, MultiMatchBuildingData>
		{
			template<Pattern P>
			using ArmOperand = ArmReplaceSide<P, Type, MultiMatchBuildingData>;

			using ArmedOperands = meta::Map_t<ArmOperand, Operands>;
		public:
			using type = FunctionPn<Category, Type, 0, ArmedOperands>;
		};



		template<Pattern MatchSide, Pattern ReplaceSide, Predicate... Condts>
		class ArmVaridic
		{
			using MatchResult = ArmMatchSide_t<MatchSide, meta::List<>, 0>;
			static_assert(MatchResult::index <= pattern::match::MatchData::max_variadic_count, 
				"maximum count of variadic operations in match side may not exceed pattern::match::MatchData::max_variadic_count");
			using ArmedMatchSide = typename MatchResult::ResultPattern;
			using MultiMatchBuidingData = typename MatchResult::BuildingData;

			using ArmedReplaceSide = ArmReplaceSide_t<ReplaceSide, Literal::complex, MultiMatchBuidingData>;
		public:
			using type = Rule<ArmedMatchSide, ArmedReplaceSide, Condts...>;
		};

	} //namespace detail_arm_variadic

	template<Pattern MatchSide, Pattern ReplaceSide, Predicate... Condts>
	using ArmVariadic_t = typename detail_arm_variadic::ArmVaridic<MatchSide, ReplaceSide, Condts...>::type;


	/////////// construct pattern

	template<Pattern MP, Pattern RP, Predicate... Cs>
	constexpr ArmVariadic_t<OrderPattern_t<MP>, OrderPattern_t<RP>, Cs...> make_rule(Rule<MP, RP>, Cs...) { return {}; }





	/////////////////////////////Definitions of Predicates
	//----------------------------------------------------------------------------------------------------------------------
	//----------------------------------------------------------------------------------------------------------------------

#define BMATH_DEFINE_DOMAIN(upper, lower) \
template<Pattern P> struct upper :PredicateMarker {};\
template<Pattern P> constexpr upper<P> lower(P) { return {}; }

	BMATH_DEFINE_DOMAIN(IsNat, is_nat)
	BMATH_DEFINE_DOMAIN(IsNat0, is_nat0)
	BMATH_DEFINE_DOMAIN(IsInt, is_int)
	BMATH_DEFINE_DOMAIN(IsReal, is_real)
	BMATH_DEFINE_DOMAIN(IsValue, is_value)

	template<Pattern P> requires (IsTreeMatchVariable<P>::value)
	struct IsVariable :PredicateMarker {};

	template<Pattern P> requires (IsTreeMatchVariable<P>::value)
	constexpr IsVariable<P> is_variable(P) { return {}; }

	enum class Relation 
	{
		//no preconditions
		equal, unequal, contains, 

		//these imply is_real for both arguments (else evaluate to false)
		smaller, larger, smaller_equal, larger_equal, divides
	};

	template<Relation relation, Pattern Lhs, Pattern Rhs>
	struct InRelation :PredicateMarker {};

#define BMATH_DEFINE_RELATION_OPERATOR(name, op) \
template<Pattern Lhs, Pattern Rhs> constexpr InRelation<name, Lhs, Rhs> op(Lhs, Rhs) { return {}; }

	BMATH_DEFINE_RELATION_OPERATOR(Relation::equal, operator==)
	BMATH_DEFINE_RELATION_OPERATOR(Relation::unequal, operator!=)
	BMATH_DEFINE_RELATION_OPERATOR(Relation::contains, contains)
	BMATH_DEFINE_RELATION_OPERATOR(Relation::smaller, operator<)
	BMATH_DEFINE_RELATION_OPERATOR(Relation::larger, operator>)
	BMATH_DEFINE_RELATION_OPERATOR(Relation::smaller_equal, operator<=)
	BMATH_DEFINE_RELATION_OPERATOR(Relation::larger_equal, operator>=)
	BMATH_DEFINE_RELATION_OPERATOR(Relation::divides, operator|)

	template<Predicate Lhs, Predicate Rhs>
	struct Or :PredicateMarker {};

	template<Predicate Lhs, Predicate Rhs>
	constexpr Or<Lhs, Rhs> operator||(Lhs, Rhs) { return {}; }

	template<Predicate Lhs, Predicate Rhs>
	struct And :PredicateMarker {};

	template<Predicate Lhs, Predicate Rhs>
	constexpr And<Lhs, Rhs> operator&&(Lhs, Rhs) { return {}; }

	template<Predicate P>
	struct Not :PredicateMarker {};

	template<Predicate P>
	constexpr Not<P> operator!(P) { return {}; }




	/////////////////////////////Test Facilities
	//----------------------------------------------------------------------------------------------------------------------
	//----------------------------------------------------------------------------------------------------------------------
	
	namespace detail_name {
		template<typename>
		struct Name { static constexpr StringLiteral value = "error"; };

		template<typename T>
		constexpr auto name_v = Name<T>::value;

		namespace detail_separate {
			template<typename T, std::size_t N>
			constexpr auto concat(const char(&first)[N]) { return first + name_v<T>; }

			template<std::size_t N1, std::size_t N2, typename T, typename... Ts> requires (sizeof...(Ts) > 0)
			constexpr auto impl(const char(&first)[N1], const char(&separator)[N2]) { return first + name_v<T> + (concat<Ts>(separator) + ...); }

			template<std::size_t N1, std::size_t N2, typename T>
			constexpr auto impl(const char(&first)[N1], const char(&separator)[N2]) { return first + name_v<T>; }

			template<std::size_t N1, std::size_t N2>
			constexpr auto impl(const char(&first)[N1], const char(&separator)[N2]) { return StringLiteral(""); }
		} //namespace detail_separate

		template<std::size_t N1, std::size_t N2, typename... Ts>
		constexpr auto separate(const char(&first)[N1], const char(&separator)[N2], Ts...) 
		{ 
			return detail_separate::impl<N1, N2, Ts...>(first, separator);
		}


		template<Pattern Lhs, Pattern Rhs, Predicate... Conds>
		struct Name<Rule<Lhs, Rhs, Conds...>>
		{
			static constexpr auto value = name_v<Lhs> + " = " + name_v<Rhs> + separate(", ", ", ", Conds{}...);
		};

		template<typename Category, Category Type, std::size_t Idx, Pattern... Ops>
		struct Name<FunctionPn<Category, Type, Idx, meta::List<Ops...>>>
		{
			static constexpr auto value = StringLiteral<fn::name_of(Type).size()>(fn::name_of(Type).data()) + 
				":" + ull_to_string_literal<Idx>() + "(" + separate("", ", ", Ops{}...) + ")";
		};

		template<double Re, double Im>
		struct Name<ComplexPn<Re, Im>>
		{
			static constexpr auto value = complex_to_string_literal<Re, Im>();
		};

		template<char... Cs>
		struct Name<VariablePn<Cs...>> 
		{ 
			static constexpr auto value = StringLiteral(std::array{ Cs... });
		};

		template<std::size_t I>
		struct Name<TreeMatchVariable<I>> 
		{ 
			static constexpr auto value = "T" + ull_to_string_literal<I>();
		};

		template<std::size_t ID>
		struct Name<MultiMatchTemp<ID>>
		{ 
			static constexpr auto value = "M" + ull_to_string_literal<ID>() + "'...";
		};

		template<std::size_t I>
		struct Name<MultiMatchVariable<I>>
		{
			static constexpr auto value = "M" + ull_to_string_literal<I>() + "...";
		};

		template<template<typename> class InDomain, Pattern T> requires (Predicate<InDomain<T>>)
		struct Name<InDomain<T>>
		{
			static constexpr auto value = "in_domain(" + name_v<T> + ")";
		};

		template<Relation R, Pattern T1, Pattern T2>
		struct Name<InRelation<R, T1, T2>>
		{
			static constexpr auto value = "in_relation(" + name_v<T1> + ", " + name_v<T2> + ")";
		};
	} //namespace detail_name

	template<typename T>
	constexpr std::string_view name(T) { return detail_name::name_v<std::remove_cv_t<T>>; }

} //namespace bmath::intern::meta_pn