#pragma once

#include <type_traits>
#include <concepts>
#include <compare>

//general concepts
namespace bmath::intern {
	template<typename T>
	concept Eq = requires (T a, T b) {
		{a == b} -> std::same_as<bool>;
		{a != b} -> std::same_as<bool>;
	};

	template<typename T>
	concept Ord = Eq<T> &&
		requires (T a, T b) {
			{a < b}  -> std::same_as<bool>;
			{a > b}  -> std::same_as<bool>;
			{a <= b} -> std::same_as<bool>;
			{a >= b} -> std::same_as<bool>;
	};

	template<typename T>
	concept Num = Eq<T> &&
		requires (T a) {
			{-a}    -> std::same_as<T>;
			{a + a} -> std::same_as<T>;
			{a - a} -> std::same_as<T>;
			{a* a} -> std::same_as<T>;
			{a / a} -> std::same_as<T>;
	};


	template<typename From, typename To>
	concept ExplicitlyConvertibleTo =
		requires (From from) { static_cast<To>(from); };


	/////////////////   Callable

	template<typename F, typename... Args>
	concept Callable = requires (F f, Args... args) { f(args...); };

	template<typename F, typename R, typename... Args>
	concept CallableTo = requires (F f, Args... args) { {f(args...)} -> std::convertible_to<R>; };


	/////////////////   InstanceOf

	template<typename T, template<typename...> class Template>
	struct DecideInstanceOf :std::false_type {};

	template<template<typename...> class Template, typename... Args>
	struct DecideInstanceOf<Template<Args...>, Template> :std::true_type {};

	template<typename T, template<typename...> class Template>
	concept InstanceOf = DecideInstanceOf<T, Template>::value;

} //namespace bmath::intern

namespace bmath::intern::meta {

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////   Operations on Lists   ////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


	template<long long Val>
	using I_c = std::integral_constant<long long, Val>;

	template<typename... Ts>
	struct List 
	{
		constexpr std::bool_constant<sizeof...(Ts) == 0> is_empty() { return {}; }
		constexpr I_c<sizeof...(Ts)> size() { return {}; }
	};


	/////////////////   Cons

	template<typename, InstanceOf<List>>
	struct Cons;

	template<typename Head, InstanceOf<List> List_>
	using Cons_t = typename Cons<Head, List_>::type;

	template<typename Head, typename... Tail>
	struct Cons<Head, List<Tail...>> { using type = List<Head, Tail...>; };
	
	
	/////////////////   Concat

	template<InstanceOf<List>, InstanceOf<List>>
	struct Concat;

	template<typename... Elems_1, typename... Elems_2>
	struct Concat<List<Elems_1...>, List<Elems_2...>> { using type = List<Elems_1..., Elems_2...>; };

	template<InstanceOf<List> List_1, InstanceOf<List> List_2>
	using Concat_t = typename Concat<List_1, List_2>::type;

	static_assert(std::is_same_v<Concat_t<List<int, int>, List<double, nullptr_t>>, List<int, int, double, nullptr_t>>);


	/////////////////   Contains

	template<typename, InstanceOf<List>>
	struct Contains;

	template<typename T, InstanceOf<List> L>
	constexpr bool contains_v = Contains<T, L>::value;

	template<typename T>
	struct Contains<T, List<>> 
		:std::false_type {};

	template<typename T, typename T1>
	struct Contains<T, List<T1>> 
		:std::bool_constant<std::is_same_v<T, T1>> {};

	template<typename T, typename T1, typename T2>
	struct Contains<T, List<T1, T2>> 
		:std::bool_constant<std::is_same_v<T, T1> || std::is_same_v<T, T2>> {};

	template<typename T, typename T1, typename T2, typename T3>
	struct Contains<T, List<T1, T2, T3>> 
		:std::bool_constant<std::is_same_v<T, T1> || std::is_same_v<T, T2> || std::is_same_v<T, T3>> {};

	template<typename T, typename T1, typename T2, typename T3, typename T4>
	struct Contains<T, List<T1, T2, T3, T4>> 
		:std::bool_constant<std::is_same_v<T, T1> || std::is_same_v<T, T2> || std::is_same_v<T, T3> || std::is_same_v<T, T4>> {};

	template<typename T, typename T1, typename T2, typename T3, typename T4, typename T5, typename... Ts>
	struct Contains<T, List<T1, T2, T3, T4, T5, Ts...>>
		:std::bool_constant<contains_v<T, List<T1, T2, T3, T4>> || contains_v<T, List<T5, Ts...>>> {};

	static_assert(contains_v<int, List<bool, void, double, int, bool>>);
	static_assert(!contains_v<int, List<bool, void, double, bool>>);


	/////////////////   Intersection

	template<InstanceOf<List>, InstanceOf<List>>
	struct Intersection;

	template<InstanceOf<List> L1, InstanceOf<List> L2>
	using Intersection_t = typename Intersection<L1, L2>::type;

	template<typename T, typename... Ts, InstanceOf<List> L2>
	class Intersection <List<T, Ts...>, L2> 
	{
		using TailRes = Intersection_t<List<Ts...>, L2>;
	public:
		using type = std::conditional_t<contains_v<T, L2>, Cons_t<T, TailRes>, TailRes>;
	};

	template<InstanceOf<List> L2>
	struct Intersection <List<>, L2> { using type = List<>; };

	static_assert(std::is_same_v<Intersection_t<List<bool, int, char>, List<float, double, bool>>, List<bool>>);


	/////////////////   Disjoint

	template<InstanceOf<List> L1, InstanceOf<List> L2>
	constexpr bool disjoint_v = std::bool_constant<std::is_same_v<Intersection_t<L1, L2>, List<>>>::value;


	/////////////////   Filter

	template<template <typename> class P, InstanceOf<List> L>
	struct Filter;

	template<template <typename> class P, InstanceOf<List> L>
	using Filter_t = typename Filter<P, L>::type;

	template<template <typename> class P, typename T, typename... Ts>
	class Filter<P, List<T, Ts...>>
	{
		using TailRes = Filter_t<P, List<Ts...>>;
	public:
		using type = std::conditional_t<P<T>::value, Cons_t<T, TailRes>, TailRes>;
	};

	template<template <typename> class P>
	struct Filter<P, List<>> { using type = List<>; };

	static_assert(std::is_same_v<Filter_t<std::is_integral, List<int, bool, double, long, float>>, List<int, bool, long>>);


	/////////////////   IndexOf

	template<typename T, InstanceOf<List> L, std::size_t Offset>
	struct DecideIndexOf;

	template<typename T, std::size_t Offset>
	struct DecideIndexOf<T, List<>, Offset> :I_c<-1>{};

	template<typename T, typename... Ts, std::size_t Offset>
	struct DecideIndexOf<T, List<T, Ts...>, Offset> :I_c<Offset> {};

	template<typename T1, typename T2, typename... Ts, std::size_t Offset>
	struct DecideIndexOf<T1, List<T2, Ts...>, Offset> :I_c<DecideIndexOf<T1, List<Ts...>, Offset + 1>::value> {};

	template<typename T, InstanceOf<List> L>
	constexpr std::size_t index_of_v = DecideIndexOf<T, L, 0>::value;

	static_assert(index_of_v<I_c<-7>, List<I_c<2>, I_c<5>, I_c<-7>>> == 2);


	///////////////////   Sort (requires take and drop)
	//namespace detail_sort {
	//	template<InstanceOf<List>, InstanceOf<List>, template<typename, typename> class Compare>
	//	struct Merge;
	//
	//	template<InstanceOf<List> L1, InstanceOf<List> L2, template<typename, typename> class Compare>
	//	using Merge_t = typename Merge<L1, L2, Compare>::type;
	//
	//	template<typename... Ts, template<typename, typename> class Compare>
	//	struct Merge<List<Ts...>, List<>, Compare> { using type = List<Ts...>; };
	//
	//	template<typename... Ts, template<typename, typename> class Compare>
	//	struct Merge<List<>, List<Ts...>, Compare> { using type = List<Ts...>; };
	//
	//	template<typename L, typename... Ls, typename R, typename... Rs, template<typename, typename> class Compare>
	//	struct Merge<List<L, Ls...>, List<R, Rs...>, Compare>
	//	{
	//		using type = std::conditional_t<
	//			Compare<L, R>::value,
	//			Cons_t<L, Merge_t<List<Ls...>, List<R, Rs...>, Compare>>,
	//			Cons_t<R, Merge_t<List<L, Ls...>, List<Rs...>, Compare>>
	//		>;
	//	};
	//} //namespace detail_sort
	//
	//template<InstanceOf<List>, template<typename, typename> class Compare>
	//struct Sort;
	//
	//template<InstanceOf<List> L, template<typename, typename> class Compare>
	//using Sort_t = typename Sort<L, Compare>::type;
	//
	//template<template<typename, typename> class Compare>
	//struct Sort<List<>, Compare> { using type = List<>; };
	//
	//template<typename T, template<typename, typename> class Compare>
	//struct Sort<List<T>, Compare> { using type = List<T>; };
	//
	//template<typename... Ts, template<typename, typename> class Compare>
	//struct Sort<List<Ts...>, Compare>
	//{
	//private:
	//	static constexpr auto n_halfs = I_c<sizeof...(Ts) / 2>{};
	//	using Lhs = Sort_t<decltype(take(n_halfs, List<Ts...>{})), Compare>;
	//	using Rhs = Sort_t<decltype(drop(n_halfs, List<Ts...>{})), Compare>;
	//public:
	//	using type = detail_sort::Merge_t<Lhs, Rhs, Compare>;
	//};
	//
	//template<typename L, typename R>
	//struct Less :std::bool_constant<(L::value < R::value)> {};
	//
	//static_assert(std::is_same_v<Sort_t<List<I_c<2>, I_c<5>, I_c<-7>>, Less>, List<I_c<-7>, I_c<2>, I_c<5>>>);

} //namespace bmath::intern::meta
