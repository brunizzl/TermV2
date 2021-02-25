#pragma once

#include <concepts>
#include <type_traits>
#include <array>
#include <string>

#include "utility/meta.hpp"

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

	template<typename Category, Category Type, meta::ListInstance Operands>
	struct FunctionPn :PatternMarker
	{
		static_assert(std::is_enum_v<Category>);
		static_assert(!std::is_same_v<Category, Fn> || fn::arity(Type) == meta::size_v<Operands>);

		template<Pattern Rhs> 
		constexpr Rule<FunctionPn, Rhs> operator=(Rhs) { return {}; }
	};

	template<Comm Type, Pattern... Operands>
	using CommutativePn = FunctionPn<Comm, Type, meta::List<Operands...>>;

	template<NonComm Type, Pattern... Operands>
	using NonCommutativePn = FunctionPn<NonComm, Type, meta::List<Operands...>>;

	template<Fn Type, Pattern... Operands>
	using FnPn = FunctionPn<Fn, Type, meta::List<Operands...>>;


	/////////// non-recursive pattern components

	template<std::size_t MatchDataIndex, bool Owning = false>
	struct TreeMatchVariable :PatternMarker {};

	template<typename T>
	struct IsTreeMatchVariable :std::false_type {};

	template<std::size_t MatchDataIndex, bool Owning>
	struct IsTreeMatchVariable<TreeMatchVariable<MatchDataIndex, Owning>> :std::true_type {};

	template<char... Cs>
	constexpr auto operator "" _TM() 
	{
		constexpr auto name = std::array{ Cs... };
		constexpr std::size_t match_data_index = parse_ull(name.begin(), name.end()).first;
		static_assert(match_data_index < pattern::match::MatchData::max_tree_match_count);
		return TreeMatchVariable<match_data_index>{};
	}

	template<std::size_t MatchDataIndex, auto MatchedInType = nullptr>
	struct MultiMatchVariable :PatternMarker {};

	template<char... Cs>
	constexpr auto operator "" _MM()
	{
		constexpr auto name = std::array{ Cs... };
		constexpr std::size_t match_data_index = parse_ull(name.begin(), name.end()).first;
		static_assert(match_data_index < pattern::match::MatchData::max_variadic_count);
		return MultiMatchVariable<match_data_index>{};
	}


	template<char... Name>
	struct VariablePn :PatternMarker 
	{
		static_assert(sizeof...(Name) > 0);
	};


	template<double Re, double Im>
	struct ComplexPn :PatternMarker {};

	template<char... Cs>
	constexpr auto operator "" _() noexcept 
	{ 
		constexpr auto name = std::array<char, sizeof...(Cs)>{ Cs... };
		constexpr double real = parse_double(name.begin(), name.end());
		return ComplexPn<real, 0.0>{}; 
	}

	template<char... Cs>
	constexpr auto operator "" _i() noexcept
	{
		constexpr auto name = std::array<char, sizeof...(Cs)>{ Cs... };
		constexpr double imag = parse_double(name.begin(), name.end());
		return ComplexPn<0.0, imag>{};
	}




	/////////////////////////////Ease pattern construction syntax
	//----------------------------------------------------------------------------------------------------------------------
	//----------------------------------------------------------------------------------------------------------------------

#define BMATH_DEFINE_FUNCTION(type, name) \
template<Pattern... Ops> constexpr FunctionPn<type, type::name, meta::List<Ops...>> name(Ops...) { return {}; }

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
	using Sum = CommutativePn<Comm::sum, Operands...>;

	template<Pattern... Operands>
	using Product = CommutativePn<Comm::product, Operands...>;

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
		using type = ComplexPn<new_re, new_im>; 
	};

	template<Pattern Lhs, Pattern Rhs>
	using Plus_t = typename Plus<Lhs, Rhs>::type;

	template<Pattern Lhs, Pattern Rhs> 
	constexpr Plus_t<Lhs, Rhs> operator+(Lhs, Rhs) { return {}; }


	/////////// operator-

	template<Pattern P>
	struct Minus { using type = Product<P, MinusOne>; };

	template<double Re, double Im>
	class Minus<ComplexPn<Re, Im>> 
	{
		static constexpr double new_re = -Re;
		static constexpr double new_im = -Im;
	public:
		using type = ComplexPn<new_re, new_im>;
	};

	template<Pattern P>
	using Minus_t = typename Minus<P>::type;

	template<Pattern P> 
	constexpr Minus_t<P> operator-(P) { return {}; }

	template<Pattern Lhs, Pattern Rhs> 
	constexpr decltype(Lhs{} + Minus_t<Rhs>{}) operator-(Lhs, Rhs) { return {}; }


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
		using type = ComplexPn<new_re, new_im>; 
	};

	template<Pattern Lhs, Pattern Rhs>
	using Times_t = typename Times<Lhs, Rhs>::type;

	template<Pattern Lhs, Pattern Rhs>
	constexpr Times_t<Lhs, Rhs> operator*(Lhs, Rhs) { return {}; }


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
		using type = ComplexPn<inv_re, inv_im>;
	};

	template<Pattern P>
	using Divide_t = typename Divide<P>::type;

	template<Pattern Lhs, Pattern Rhs> 
	constexpr decltype(Lhs{} * Divide_t<Rhs>{}) operator/(Lhs, Rhs) { return {}; }


	/////////// operator^

	template<Pattern Lhs, Pattern Rhs>
	constexpr Pow<Lhs, Rhs> operator^(Lhs, Rhs) { return {}; }




	/////////////////////////////Sorting Pattern
	//----------------------------------------------------------------------------------------------------------------------
	//----------------------------------------------------------------------------------------------------------------------


	/////////// generality

	template<Pattern>
	struct Generality;

	template<typename Category, Category Type, meta::ListInstance Operands>
	struct Generality<FunctionPn<Category, Type, Operands>> :meta::Constant<bmath::intern::generality(Type)> {};

	template<std::size_t MatchDataIndex, bool Owning>
	struct Generality<TreeMatchVariable<MatchDataIndex, Owning>> :meta::Constant<3001> {};

	template<std::size_t MatchDataIndex, auto MatchedInType>
	struct Generality<MultiMatchVariable<MatchDataIndex, MatchedInType>> :meta::Constant<3999> {};

	template<double Re, double Im>
	struct Generality<ComplexPn<Re, Im>> :meta::Constant<1000> {};

	template<char... Cs>
	struct Generality<VariablePn<Cs...>> :meta::Constant<1001> {};

	template<Pattern P> 
	constexpr int generality_v = Generality<P>::value;


	/////////// compare

	template<Pattern P1, Pattern P2>
	struct ComparePatterns :std::bool_constant<(generality_v<P1> < generality_v<P2>)> {};
	
	template<typename Category, Category Type, Pattern... LOps, Pattern... ROps>
	struct ComparePatterns<
		FunctionPn<Category, Type, meta::List<LOps...>>, 
		FunctionPn<Category, Type, meta::List<ROps...>>
	> :std::bool_constant<(sizeof...(LOps) < sizeof...(ROps))> {}; //TODO: compare recursively


	/////////// sort

	template<Pattern P>
	struct SortPattern :std::type_identity<P> {};

	template<Pattern P>
	using SortPattern_t = typename SortPattern<P>::type;

	//indirection necessairy, as otherwise local argument is implicitly inserted to SortPattern template
	template<meta::ListInstance Operands>
	using SortEachOperand_t = meta::Map_t<SortPattern, Operands>;

	template<typename Category, Category Type, meta::ListInstance Operands>
	class SortPattern<FunctionPn<Category, Type, Operands>>
	{
		using IndividuallySortedOperands = SortEachOperand_t<Operands>;
		using SortedOperands = meta::Sort_t<IndividuallySortedOperands, ComparePatterns>;
	public:
		using type = FunctionPn<Category, Type, SortedOperands>;
	};

	/////////// construct pattern

	template<Pattern MP, Pattern RP, Predicate... Cs>
	constexpr Rule<SortPattern_t<MP>, SortPattern_t<RP>, Cs...> make_rule(Rule<MP, RP>, Cs...) { return {}; }



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
		equal, unequal, //no preconditions
		smaller, larger, smaller_equal, larger_equal //these imply is_real for both arguments (else evaluate to false)
	};

	template<Relation relation, Pattern Lhs, Pattern Rhs>
	struct InRelation :PredicateMarker {};

#define BMATH_DEFINE_RELATION(name, op) \
template<Pattern Lhs, Pattern Rhs> constexpr InRelation<name, Lhs, Rhs> operator op(Lhs, Rhs) { return {}; }

	BMATH_DEFINE_RELATION(Relation::equal, ==)
	BMATH_DEFINE_RELATION(Relation::unequal, !=)
	BMATH_DEFINE_RELATION(Relation::smaller, <)
	BMATH_DEFINE_RELATION(Relation::larger, >)
	BMATH_DEFINE_RELATION(Relation::smaller_equal, <=)
	BMATH_DEFINE_RELATION(Relation::larger_equal, >=)

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




	/////////////////////////////Test
	//----------------------------------------------------------------------------------------------------------------------
	//----------------------------------------------------------------------------------------------------------------------

	//"a, b | a^2 + 2 a b + b^2 = (a + b)^2" 
	constexpr auto a = 0_TM;
	constexpr auto b = 1_TM;
	constexpr auto rule_1 = make_rule((a^2_) + 2_*a*b + (b^2_) = (a + b)^2_);

	//"x :variable, a :any, as :sum... | diff(a + as, x) = diff(a, x) + diff(as, x)"
	constexpr auto x = 0_TM;
	constexpr auto u = 1_TM;
	constexpr auto vs = 0_MM;
	constexpr auto rule_2 = make_rule(diff(u + vs, x) = diff(u, x) + diff(vs, x), is_variable(x));

	//"k :int | sin((2 k + 0.5) 'pi') =  1" 
	constexpr auto k = 0_TM;
	constexpr auto pi = VariablePn<'p', 'i'>{};
	constexpr auto rule_3 = make_rule(sin(k * pi) = 1_, is_int((k - 1_) / 2_), 3_ > 2_);

	constexpr auto rule_4 = make_rule((a + b) + 1_ + (2_ + 3_i) + (1_ + a + b) = 0.5_);
	constexpr auto rule_5 = make_rule((a * b) * 1_ * (2_ * 3_i) * (1_ * a * b) = 0.5_);

	namespace detail_to_string {
		template<typename>
		struct ToString;

		template<typename T>
		std::string concat(const char* const first) { return first + ToString<T>::call(); }

		template<typename T, typename... Ts> requires (sizeof...(Ts) > 0)
		std::string separate(const char* const first, const char* const separator) { return first + ToString<T>::call() + (concat<Ts>(separator) + ...); }

		template<typename T>
		std::string separate(const char* const first, const char* const separator) { return first + ToString<T>::call(); }

		template<typename... Ts> requires (sizeof...(Ts) == 0)
		std::string separate(const char* const first, const char* const separator) { return first; }


		template<Pattern Lhs, Pattern Rhs, Predicate... Conds>
		struct ToString<Rule<Lhs, Rhs, Conds...>>
		{
			static std::string call() 
			{
				return ToString<Lhs>::call() + " = " + ToString<Rhs>::call() + separate<Conds...>(", ", ", ");
			}
		};

		template<auto Type, Pattern... Ops>
		struct ToString<FunctionPn<decltype(Type), Type, meta::List<Ops...>>>
		{
			static std::string call() { return std::string(fn::name_of(Type)) + "(" + separate<Ops...>("", ", ") + ")"; }
		};

		template<double Re, double Im>
		struct ToString<ComplexPn<Re, Im>>
		{
			static std::string call()
			{
				std::string res;
				print::append_complex(Complex{ Re, Im }, res, 0);
				return res;
			}
		};

		template<char... Cs>
		struct ToString<VariablePn<Cs...>> { static std::string call() { return std::string{ Cs... }; } };

		template<std::size_t I, bool O>
		struct ToString<TreeMatchVariable<I, O>> { static std::string call() { return "T" + std::to_string(I); } };

		template<std::size_t I, auto T>
		struct ToString<MultiMatchVariable<I, T>> { static std::string call() { return "M" + std::to_string(I) + "..."; } };

		template<template<typename> class InDomain, Pattern T> requires (Predicate<InDomain<T>>)
			struct ToString<InDomain<T>>
		{
			static std::string call() { return "in_domain(" + ToString<T>::call() + ")"; }
		};

		template<Relation R, Pattern T1, Pattern T2>
		struct ToString<InRelation<R, T1, T2>>
		{
			static std::string call() { return "in_relation(" + ToString<T1>::call() + ", " + ToString<T2>::call() + ")"; }
		};
	} //namespace detail_to_string

	template<typename T>
	std::string to_string(T) { return detail_to_string::ToString<std::remove_cv_t<T>>::call(); }

} //namespace bmath::intern::meta_pn