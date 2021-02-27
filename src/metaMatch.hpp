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

	template<typename Category, Category Type, std::size_t MatchDataIndex, meta::ListInstance Ops> //MatchDataIdx only relevant for variadic
	struct FunctionPn :PatternMarker
	{
		using Operands = Ops;

		static_assert(std::is_enum_v<Category>);
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

	template<std::size_t MatchDataIndex, bool Owning = false>
	struct TreeMatchVariable :PatternMarker {};

	template<typename T>
	struct IsTreeMatchVariable :std::false_type {};

	template<std::size_t MatchDataIndex, bool Owning>
	struct IsTreeMatchVariable<TreeMatchVariable<MatchDataIndex, Owning>> :std::true_type {};

	template<char... Cs>
	constexpr auto operator "" _tree()
	{
		constexpr unsigned long long match_data_idx = parse_ull(std::to_array({ Cs... }));
		static_assert(match_data_idx < pattern::match::MatchData::max_tree_match_count);
		return TreeMatchVariable<match_data_idx, false>{};
	}



	template<std::size_t MatchDataIndex, auto MatchedInType = nullptr>
	struct MultiMatchVariable :PatternMarker {};

	template<char... Cs>
	constexpr auto operator "" _multi()
	{
		constexpr unsigned long long match_data_idx = parse_ull(std::to_array({ Cs... }));
		static_assert(match_data_idx < pattern::match::MatchData::max_variadic_count);
		return MultiMatchVariable<match_data_idx, nullptr>{};
	}

	namespace detail_variables {
		template<std::size_t... Is>
		constexpr auto make_trees(std::index_sequence<Is...>)
		{
			static_assert(sizeof...(Is) <= pattern::match::MatchData::max_tree_match_count);
			return std::make_tuple(TreeMatchVariable<Is, false>{} ...);
		}

		template<std::size_t... Is>
		constexpr auto make_multis(std::index_sequence<Is...>)
		{
			static_assert(sizeof...(Is) <= pattern::match::MatchData::max_variadic_count);
			return std::make_tuple(MultiMatchVariable<Is, nullptr>{} ...);
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
	struct Generality<TreeMatchVariable<MatchDataIndex, false>> :meta::Constant<generality(pattern::TreeMatchNonOwning{})> {};

	template<std::size_t MatchDataIndex>
	struct Generality<TreeMatchVariable<MatchDataIndex, true>> :meta::Constant<generality(pattern::Restriction::any)> {};

	template<std::size_t MatchDataIndex, auto MatchedInType>
	struct Generality<MultiMatchVariable<MatchDataIndex, MatchedInType>> :meta::Constant<generality(pattern::MultiParams{})> {};

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

	template<std::size_t Idx1, bool Owns1, std::size_t Idx2, bool Owns2>
	struct ComparePatterns<TreeMatchVariable<Idx1, Owns1>, TreeMatchVariable<Idx2, Owns2>> 
		:std::bool_constant<(Idx1 < Idx2 || Owns1 < Owns2)> {};

	template<std::size_t Idx1, auto MatchedInType1, std::size_t Idx2, auto MatchedInType2>
	struct ComparePatterns<MultiMatchVariable<Idx1, MatchedInType1>, MultiMatchVariable<Idx2, MatchedInType2>>
		:std::bool_constant<(Idx1 < Idx2)> {};

	template<double Re1, double Im1, double Re2, double Im2>
	struct ComparePatterns<ComplexPn<Re1, Im1>, ComplexPn<Re2, Im2>>
		:std::bool_constant<(compare_complex(Complex{ Re1, Im1 }, Complex{ Re2, Im2 }) == std::strong_ordering::less)> {};



	/////////// sort

	template<Pattern P>
	struct OrderPattern :std::type_identity<P> {};

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

	/////////// construct pattern

	template<Pattern MP, Pattern RP, Predicate... Cs>
	constexpr Rule<OrderPattern_t<MP>, OrderPattern_t<RP>, Cs...> make_rule(Rule<MP, RP>, Cs...) { return {}; }





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
			constexpr auto separate(const char(&first)[N1], const char(&separator)[N2]) { return first + name_v<T> + (concat<Ts>(separator) + ...); }

			template<std::size_t N1, std::size_t N2, typename T>
			constexpr auto separate(const char(&first)[N1], const char(&separator)[N2]) { return first + name_v<T>; }

			template<std::size_t N1, std::size_t N2, typename... Ts> requires (sizeof...(Ts) == 0)
			constexpr auto separate(const char(&first)[N1], const char(&separator)[N2]) { return StringLiteral(""); }
		} //namespace detail_separate

		template<std::size_t N1, std::size_t N2, typename... Ts>
		constexpr auto separate(const char(&first)[N1], const char(&separator)[N2], meta::List<Ts...>) 
		{ 
			return detail_separate::separate<N1, N2, Ts...>(first, separator); 
		}


		template<Pattern Lhs, Pattern Rhs, Predicate... Conds>
		struct Name<Rule<Lhs, Rhs, Conds...>>
		{
			static constexpr auto value = name_v<Lhs> + " = " + name_v<Rhs> + separate(", ", ", ", meta::List<Conds...>{});
		};

		template<auto Type, std::size_t Idx, Pattern... Ops>
		struct Name<FunctionPn<decltype(Type), Type, Idx, meta::List<Ops...>>>
		{
			static constexpr auto value = StringLiteral<fn::name_of(Type).size()>(fn::name_of(Type).data()) + ":" +
				ull_to_string_literal<Idx>() + "(" + separate("", ", ", meta::List<Ops...>{}) + ")";
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

		template<std::size_t I, bool O>
		struct Name<TreeMatchVariable<I, O>> 
		{ 
			static constexpr auto value = "T" + ull_to_string_literal<I>();
		};

		template<std::size_t I, auto T>
		struct Name<MultiMatchVariable<I, T>> 
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