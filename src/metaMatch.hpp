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

	template<typename T>
	concept FunctionProps = requires { T::function_type; };

	template<auto Type, std::size_t MatchDataIndex> 
	struct VariadicProps
	{ 
		static_assert(std::is_same_v<decltype(Type), Comm> || std::is_same_v<decltype(Type), NonComm>);
		static constexpr auto function_type = Type;
	}; 
	
	template<Fn Type>
	struct FnProps { static constexpr auto function_type = Type; };

	template<char... Cs>
	struct NamedFnProps { static constexpr auto function_type = NamedFn{}; };


	template<FunctionProps Props, Pattern... Operands>
	struct FunctionPn :PatternMarker
	{
		template<Pattern Rhs> 
		constexpr meta::Pair<FunctionPn, Rhs> operator=(Rhs) { return {}; }
	}; 

	template<Fn Type, Pattern... Operands>
	struct FunctionPn<FnProps<Type>, Operands...> :PatternMarker
	{
		static_assert(fn::arity(Type) == sizeof...(Operands));

		template<Pattern Rhs>
		constexpr meta::Pair<FunctionPn, Rhs> operator=(Rhs) { return {}; }
	};

	template<FunctionProps Props>
	struct CurriedFunctionPn 
	{  
		template<Pattern... Operands>
		using type = FunctionPn<Props, Operands...>;
	};

	template<FunctionProps Props, meta::ListInstance Operands>
	struct MakeFunctionPn;

	template<FunctionProps Props, meta::ListInstance Operands>
	using MakeFunctionPn_t = typename MakeFunctionPn<Props, Operands>::type;

	template<FunctionProps Props, Pattern... Operands>
	struct MakeFunctionPn<Props, meta::List<Operands...>> { using type = FunctionPn<Props, Operands...>; };


	template<Comm Type, std::size_t MatchDataIdx, Pattern... Operands>
	using CommutativePn = FunctionPn<VariadicProps<Type, MatchDataIdx>, Operands...>;

	template<NonComm Type, std::size_t MatchDataIdx, Pattern... Operands>
	using NonCommutativePn = FunctionPn<VariadicProps<Type, MatchDataIdx>, Operands...>;

	template<Fn Type, Pattern... Operands>
	using FnPn = FunctionPn<FnProps<Type>, Operands...>;

	template<StringLiteral Name, Pattern... Operands>
	using NamedFnPn = FunctionPn<NamedFnProps<Name>, Operands...>;



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

#define BMATH_DEFINE_VARIADIC(type, name) \
template<Pattern... Ops> constexpr FunctionPn<VariadicProps<type::name, 0>, Ops...> name(Ops...) { return {}; }

#define BMATH_DEFINE_FN(name) \
template<Pattern... Ops> constexpr FunctionPn<FnProps<Fn::name>, Ops...> name(Ops...) { return {}; }

	BMATH_DEFINE_VARIADIC(Comm, set)
	BMATH_DEFINE_VARIADIC(Comm, multiset)
	BMATH_DEFINE_VARIADIC(Comm, union_)
	BMATH_DEFINE_VARIADIC(Comm, intersection)

	BMATH_DEFINE_VARIADIC(NonComm, list)
	BMATH_DEFINE_VARIADIC(NonComm, ordered_sum)
	BMATH_DEFINE_VARIADIC(NonComm, ordered_product)

	BMATH_DEFINE_FN(pow)
	BMATH_DEFINE_FN(log)
	BMATH_DEFINE_FN(sqrt)
	BMATH_DEFINE_FN(exp)
	BMATH_DEFINE_FN(ln)
	BMATH_DEFINE_FN(sin)
	BMATH_DEFINE_FN(cos)
	BMATH_DEFINE_FN(tan)
	BMATH_DEFINE_FN(sinh)
	BMATH_DEFINE_FN(cosh)
	BMATH_DEFINE_FN(tanh)
	BMATH_DEFINE_FN(asin)
	BMATH_DEFINE_FN(acos)
	BMATH_DEFINE_FN(atan)
	BMATH_DEFINE_FN(asinh)
	BMATH_DEFINE_FN(acosh)
	BMATH_DEFINE_FN(atanh)
	BMATH_DEFINE_FN(abs)
	BMATH_DEFINE_FN(arg)
	BMATH_DEFINE_FN(re)
	BMATH_DEFINE_FN(im)
	BMATH_DEFINE_FN(force)
	BMATH_DEFINE_FN(diff)

#define BMATH_DEFINE_NAMEDFN(name) \
template<bmath::intern::meta_pn::Pattern... Ops> \
constexpr bmath::intern::meta_pn::FunctionPn<bmath::intern::ToCharSeq_t<bmath::intern::meta_pn::NamedFnProps, bmath::intern::StringLiteral(#name)>, Ops...> name(Ops...) { return {}; }


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

	template<FunctionProps Props, Pattern... Operands>
	struct Generality<FunctionPn<Props, Operands...>> :meta::Constant<generality((MathType)Props::function_type)> {};

	template<std::size_t MatchDataIndex>
	struct Generality<TreeMatchVariable<MatchDataIndex>> :meta::Constant<generality(pattern::TreeMatchNonOwning{})> {};

	template<std::size_t ID>
	struct Generality<MultiMatchTemp<ID>> :meta::Constant<generality(pattern::MultiParams{})> {};

	template<std::size_t Idx>
	struct Generality<MultiMatchVariable<Idx>> :meta::Constant < generality(pattern::MultiParams{}) > {};

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
	
	template<auto Type, std::size_t LIdx, std::size_t RIdx, Pattern... LOps, Pattern... ROps>
	struct ComparePatterns<
		FunctionPn<VariadicProps<Type, LIdx>, LOps...>, 
		FunctionPn<VariadicProps<Type, RIdx>, ROps...>
	> :std::bool_constant<meta::smaller_v<meta::List<LOps...>, meta::List<ROps...>, ComparePatternsIndirection>> {};

	template<Fn Type, Pattern... LOps, Pattern... ROps>
	struct ComparePatterns<
		FunctionPn<FnProps<Type>, LOps...>,
		FunctionPn<FnProps<Type>, ROps...>
	> :std::bool_constant<meta::smaller_v<meta::List<LOps...>, meta::List<ROps...>, ComparePatternsIndirection>> {};

	template<StringLiteral Name, Pattern... LOps, Pattern... ROps>
	struct ComparePatterns<
		FunctionPn<NamedFnProps<Name>, LOps...>,
		FunctionPn<NamedFnProps<Name>, ROps...>
	> :std::bool_constant<meta::smaller_v<meta::List<LOps...>, meta::List<ROps...>, ComparePatternsIndirection>> {};

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
	template<Pattern... Operands>
	using OrderEachOperand_t = meta::DirectMap_t<meta::List, OrderPattern, Operands...>;

	template<Comm Type, meta::ListInstance Operands>
	struct OperandsToOperation { using type = MakeFunctionPn_t<VariadicProps<Type, 0>, Operands>; };

	template<Comm Type, meta::ListInstance Operands>
	using OperandsToOperation_t = typename OperandsToOperation<Type, Operands>::type;

	template<typename Op, typename... Ops>
	struct OperandsToOperation<Comm::sum, meta::List<Op, Ops...>> 
	{ 
		using type = meta::DirectFoldl_t<Plus, Op, Ops...>; 
	};

	template<typename Op, typename... Ops>
	struct OperandsToOperation<Comm::product, meta::List<Op, Ops...>> 
	{ 
		using type = meta::DirectFoldl_t<Times, Op, Ops...>; 
	};


	template<Comm Type, Pattern... Operands>
	class OrderPattern<FunctionPn<VariadicProps<Type, 0>, Operands...>>
	{
		using IndividuallyOrderedOperands = OrderEachOperand_t<Operands...>;
		using SortedOperands              = meta::Sort_t<IndividuallyOrderedOperands, ComparePatterns>;
	public:
		using type = OperandsToOperation_t<Type, SortedOperands>;
	};

	template<FunctionProps Props, Pattern... Operands>
	struct OrderPattern<FunctionPn<Props, Operands...>>
	{
		using type = MakeFunctionPn_t<Props, OrderEachOperand_t<Operands...>>;
	};

	/////////// set MatchDataIndex in multimatch and variadic to correct value, surround multimatch in rhs with correct variadic, add outhermost multimatch
	namespace detail_arm_rule {

		template<auto EnclosingType, std::size_t ID, std::size_t MatchDataIdx>
		struct MultiMatchBuidingDatum 
		{
			static constexpr auto enclosing_type = EnclosingType;
			static constexpr std::size_t id = ID;
			static constexpr std::size_t index = MatchDataIdx;
		};



		/////////// arm left hand side of rule

		template<Pattern P, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx>
		struct SetMatchSideIndicesResult 
		{
			using ResultPattern = P;
			using BuildingData = MultiMatchBuildingData;
			static constexpr std::size_t index = NxtIdx;
		};

		template<Pattern P, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx>
		struct SetMatchSideIndices { using type = SetMatchSideIndicesResult<P, MultiMatchBuildingData, NxtIdx>; };

		template<Pattern P, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx>
		using SetMatchSideIndices_t = typename SetMatchSideIndices<P, MultiMatchBuildingData, NxtIdx>::type;

		template<meta::ListInstance OperandsCorrectIndices, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx>
		struct SetOperandIndicesAccumulator
		{
			using Operands = OperandsCorrectIndices;
			using BuildingData = MultiMatchBuildingData;
			static constexpr std::size_t index = NxtIdx;
		};

		template<auto EnclosingVariadicType, std::size_t EnclosingVariadicIndex>
		struct SetOperandIndices
		{
			template<typename LastResult, Pattern Operand>
			struct apply;

			template<meta::ListInstance OperandsCorrectIndices, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx, Pattern Operand>
			class apply<SetOperandIndicesAccumulator<OperandsCorrectIndices, MultiMatchBuildingData, NxtIdx>, Operand>
			{
				using ArmedOperandData = SetMatchSideIndices_t<Operand, MultiMatchBuildingData, NxtIdx>;
			public:
				using type = SetOperandIndicesAccumulator<
					meta::Concat_t<OperandsCorrectIndices, meta::List<typename ArmedOperandData::ResultPattern>>,
					typename ArmedOperandData::BuildingData,
					ArmedOperandData::index
				>;
			};

			template<meta::ListInstance OperandsCorrectIndices, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx, std::size_t ID>
			class apply<SetOperandIndicesAccumulator<OperandsCorrectIndices, MultiMatchBuildingData, NxtIdx>, MultiMatchTemp<ID>>
			{
				static_assert(Function(EnclosingVariadicType).is<Variadic>(), 
					"only variadic operations may contain multi-match-variables");

				template<typename BuildingDatum>
				struct IsInSameFunction { static constexpr bool value = BuildingDatum::index == EnclosingVariadicIndex; };
				static_assert(!std::is_same_v<decltype(EnclosingVariadicType), Comm> || 
					std::is_same_v<meta::Find_t<IsInSameFunction, MultiMatchBuildingData>, meta::FoundNothing>, 
					"commutative functions may only contain up to one multi match variable each");
			public:
				using type = SetOperandIndicesAccumulator<
					meta::Concat_t<OperandsCorrectIndices, meta::List<MultiMatchVariable<EnclosingVariadicIndex>>>,
					meta::Cons_t<MultiMatchBuidingDatum<EnclosingVariadicType, ID, EnclosingVariadicIndex>, MultiMatchBuildingData>,
					NxtIdx
				>;
			};
		};

		template<auto Type, Pattern... Operands, meta::ListInstance MultiMatchBuildingData, std::size_t ThisIdx>
		class SetMatchSideIndices<FunctionPn<VariadicProps<Type, 0>, Operands...>, MultiMatchBuildingData, ThisIdx>
		{
			using OperandsResult = meta::Foldl_t<
				typename SetOperandIndices<Type, ThisIdx>::apply,
				SetOperandIndicesAccumulator<meta::List<>, MultiMatchBuildingData, ThisIdx + 1>,
				meta::List<Operands...>
			>;
		public:
			using type = SetMatchSideIndicesResult<
				MakeFunctionPn_t<VariadicProps<Type, ThisIdx>, typename OperandsResult::Operands>,
				typename OperandsResult::BuildingData,
				OperandsResult::index
			>;
		}; 
		
		template<FunctionProps Props, Pattern... Operands, meta::ListInstance MultiMatchBuildingData, std::size_t NxtIdx>
		class SetMatchSideIndices<FunctionPn<Props, Operands...>, MultiMatchBuildingData, NxtIdx>
		{
			using OperandsResult = meta::Foldl_t<
				typename SetOperandIndices<Props::function_type, NxtIdx>::apply,
				SetOperandIndicesAccumulator<meta::List<>, MultiMatchBuildingData, NxtIdx>,
				meta::List<Operands...>
			>;
		public:
			using type = SetMatchSideIndicesResult<
				MakeFunctionPn_t<Props, typename OperandsResult::Operands>,
				typename OperandsResult::BuildingData,
				OperandsResult::index
			>;
		};

		static_assert(std::is_same_v<
			SetMatchSideIndices_t<FunctionPn<VariadicProps<Comm::sum, 0>>, meta::List<>, 3>,
			SetMatchSideIndicesResult<FunctionPn<VariadicProps<Comm::sum, 3>>, meta::List<>, 4>
		>);

		static_assert(std::is_same_v<
			SetMatchSideIndices_t<
				FunctionPn<VariadicProps<Comm::sum, 0>, decltype(-2_), MultiMatchTemp<8>>,
				meta::List<>, 
				3>,
			SetMatchSideIndicesResult<
				FunctionPn<VariadicProps<Comm::sum, 3>, decltype(-2_), MultiMatchVariable<3>>,
				meta::List<MultiMatchBuidingDatum<Comm::sum, 8, 3>>, 
				4>
		>);




		/////////// arm right hand side of rule

		template<Pattern P, auto ParentType, meta::ListInstance MultiMatchBuildingData>
		struct SetReplaceSideIndices { using type = P; };

		template<Pattern P, auto ParentType, meta::ListInstance MultiMatchBuildingData>
		using SetReplaceSideIndices_t = typename SetReplaceSideIndices<P, ParentType, MultiMatchBuildingData>::type;

		template<std::size_t ID, auto ParentType, meta::ListInstance MultiMatchBuildingData>
		class SetReplaceSideIndices<MultiMatchTemp<ID>, ParentType, MultiMatchBuildingData>
		{
			template<typename BuildingDatum>
			struct IsThisBuildingDatum :std::bool_constant<BuildingDatum::id == ID> {};

			using ThisBuildingDatum = meta::Find_t<IsThisBuildingDatum, MultiMatchBuildingData>;
			static_assert(!std::is_same_v<ThisBuildingDatum, meta::FoundNothing>, 
				"only multi-match-variables present in match side may occur in replacement side.");

			using UnwrappedResult = MultiMatchVariable<ThisBuildingDatum::index>;
			using WrappedResult = FunctionPn<VariadicProps<ThisBuildingDatum::enclosing_type, 0>, UnwrappedResult>;
			static constexpr bool parent_is_enclosing_type = MathType(ThisBuildingDatum::enclosing_type) == MathType(ParentType);
		public:
			using type = std::conditional_t<parent_is_enclosing_type, UnwrappedResult, WrappedResult>;
		};

		template<FunctionProps Props, Pattern... Operands, auto ParentType, meta::ListInstance MultiMatchBuildingData>
		class SetReplaceSideIndices<FunctionPn<Props, Operands...>, ParentType, MultiMatchBuildingData>
		{
			template<Pattern P>
			using SetOperand = SetReplaceSideIndices<P, Props::function_type, MultiMatchBuildingData>;

			using OperandsCorrectIndices = meta::Map_t<SetOperand, meta::List<Operands...>>;
		public:
			using type = MakeFunctionPn_t<Props, OperandsCorrectIndices>;
		};



		/////////// add outermost multimatch (if outermost operation is variadic and multimatch is not already present)
		//note: outermost operation always matched first -> MatchDataIndex always 0

		template<Pattern MatchSide, Pattern ReplaceSide>
		struct AddOutermostMulti { using type = meta::Pair<MatchSide, ReplaceSide>; };

		template<Pattern MatchSide, Pattern ReplaceSide>
		using AddOutermostMulti_t = typename AddOutermostMulti<MatchSide, ReplaceSide>::type;

		template<Comm Type, Pattern... Operands, Pattern ReplaceSide> 
		requires (!std::is_same_v<meta::Last_t<meta::List<Operands...>>, MultiMatchVariable<0>> &&
			fn::is_associative(Variadic(Type)) && std::is_same_v<Comm, decltype(Type)>)
		struct AddOutermostMulti<FunctionPn<VariadicProps<Type, 0>, Operands...>, ReplaceSide> 
		{
			using type = meta::Pair<
				FunctionPn<VariadicProps<Type, 0>, Operands..., MultiMatchVariable<0>>,
				FunctionPn<VariadicProps<Type, 0>, ReplaceSide, MultiMatchVariable<0>>
			>;
		}; 

		static_assert(std::is_same_v<
			AddOutermostMulti_t<Sum<decltype(1337_)>, decltype(-1.25_)>,
			meta::Pair<Sum<decltype(1337_), MultiMatchVariable<0>>, Sum<decltype(-1.25_), MultiMatchVariable<0>>>
		>);
		static_assert(std::is_same_v<
			AddOutermostMulti_t<Sum<decltype(1337_), MultiMatchVariable<0>>, decltype(-1.25_)>,
			meta::Pair<Sum<decltype(1337_), MultiMatchVariable<0>>, decltype(-1.25_)>
		>);
		static_assert(std::is_same_v<
			AddOutermostMulti_t<CommutativePn<Comm::set, 0, decltype(1337_)>, decltype(-1.25_)>,
			meta::Pair<CommutativePn<Comm::set, 0, decltype(1337_)>, decltype(-1.25_)>
		>);


		template<NonComm Type, Pattern... Operands, Pattern ReplaceSide>
		requires (!std::is_same_v<meta::Head_t<meta::List<Operands...>>, MultiMatchVariable<0>> && 
		          !std::is_same_v<meta::Last_t<meta::List<Operands...>>, MultiMatchVariable<0>> &&
			fn::is_associative(Variadic(Type)) && std::is_same_v<NonComm, decltype(Type)>)
		struct AddOutermostMulti<FunctionPn<VariadicProps<Type, 0>, Operands...>, ReplaceSide>
		{
			using type = meta::Pair <
				FunctionPn<VariadicProps<Type, 0>, MultiMatchVariable<0>, Operands..., MultiMatchVariable<0>>,
				FunctionPn<VariadicProps<Type, 0>, MultiMatchVariable<0>, ReplaceSide, MultiMatchVariable<0>>
			>;
		};

		template<NonComm Type, Pattern... Operands, Pattern ReplaceSide>
		requires ( std::is_same_v<meta::Head_t<meta::List<Operands...>>, MultiMatchVariable<0>> &&
		          !std::is_same_v<meta::Last_t<meta::List<Operands...>>, MultiMatchVariable<0>> &&
			fn::is_associative(Variadic(Type)) && std::is_same_v<NonComm, decltype(Type)>)
		struct AddOutermostMulti<FunctionPn<VariadicProps<Type, 0>, Operands...>, ReplaceSide>
		{
			using type = meta::Pair<
				FunctionPn<VariadicProps<Type, 0>, Operands..., MultiMatchVariable<0>>,
				FunctionPn<VariadicProps<Type, 0>, ReplaceSide, MultiMatchVariable<0>>
			>;
		};

		template<NonComm Type, Pattern... Operands, Pattern ReplaceSide>
		requires (!std::is_same_v<meta::Head_t<meta::List<Operands...>>, MultiMatchVariable<0>> &&
		           std::is_same_v<meta::Last_t<meta::List<Operands...>>, MultiMatchVariable<0>> &&
			fn::is_associative(Variadic(Type)) && std::is_same_v<NonComm, decltype(Type)>)
		struct AddOutermostMulti<FunctionPn<VariadicProps<Type, 0>, Operands...>, ReplaceSide>
		{
			using type = meta::Pair<
				FunctionPn<VariadicProps<Type, 0>, MultiMatchVariable<0>, Operands...>,
				FunctionPn<VariadicProps<Type, 0>, MultiMatchVariable<0>, ReplaceSide>
			>;
		};

		/////////// combine all actions

		template<Pattern MatchSide, Pattern ReplaceSide, Predicate... Condts>
		class ArmRule
		{
			//set indices
			using SetMatchIndicesResult = SetMatchSideIndices_t<OrderPattern_t<MatchSide>, meta::List<>, 0>;
			static_assert(SetMatchIndicesResult::index <= pattern::match::MatchData::max_variadic_count,
				"maximum count of variadic operations in match side may not exceed pattern::match::MatchData::max_variadic_count");
			using MatchSideCorrectIndices = typename SetMatchIndicesResult::ResultPattern;
			using MultiMatchBuidingData = typename SetMatchIndicesResult::BuildingData;

			using ReplaceSideCorrectIndices = SetReplaceSideIndices_t<ReplaceSide, Literal::complex, MultiMatchBuidingData>;

			//add outermost multi match 
			using MultiMatchAddedPair = AddOutermostMulti_t<MatchSideCorrectIndices, ReplaceSideCorrectIndices>;

			using FinalMatchSide = meta::Fst_t<MultiMatchAddedPair>;
			using FinalReplaceSide = OrderPattern_t < meta::Snd_t<MultiMatchAddedPair>>;
		public:
			using type = Rule<FinalMatchSide, FinalReplaceSide, Condts...>;
		};

	} //namespace detail_arm_rule

	template<Pattern MatchSide, Pattern ReplaceSide, Predicate... Condts>
	using ArmRule_t = typename detail_arm_rule::ArmRule<MatchSide, ReplaceSide, Condts...>::type;


	/////////// construct pattern

	template<Pattern MP, Pattern RP, Predicate... Cs>
	constexpr auto make_rule(meta::Pair<MP, RP>, Cs...) 
	{ 
		using ResultRule = ArmRule_t<MP, RP, Cs...>;
		return ResultRule{};
	}





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

		template<auto Type, std::size_t I, Pattern... Ops>
		struct Name<FunctionPn<VariadicProps<Type, I>, Ops...>>
		{
			static constexpr auto value = BMATH_SV_TO_LITERAL(fn::name_of((Variadic)Type)) +
				":" + ull_to_string_literal<I>() + "(" + separate("", ", ", Ops{}...) + ")";
		};
		
		template<Fn Type, Pattern... Ops>
		struct Name<FunctionPn<FnProps<Type>, Ops...>>
		{
			static constexpr auto value = BMATH_SV_TO_LITERAL(fn::name_of(Type)) +
				"(" + separate("", ", ", Ops{}...) + ")";
		};

		template<char... Cs, Pattern... Ops>
		struct Name<FunctionPn<NamedFnProps<Cs...>, Ops...>>
		{
			static constexpr auto value = StringLiteral(std::array{ Cs... }) + "(" + separate("", ", ", Ops{}...) + ")";
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