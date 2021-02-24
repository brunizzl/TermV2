#pragma once

#include <type_traits>
#include <concepts>
#include <compare>
#include <iterator>

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
			{a * a} -> std::same_as<T>;
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


	/////////////////   IterOver

	template<typename I, typename T>
	concept IterOver = std::random_access_iterator<I> &&
		requires (I i) {
			{*i} -> std::same_as<T&>;
	};


	/////////////////  Container

	template<typename C, typename T>
	concept ContainerOf = std::is_same_v<T, typename C::value_type>;

	static_assert(ContainerOf<std::array<int, 3>, int>);
} //namespace bmath::intern

namespace bmath::intern::meta {

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////   Operations on Lists of types   ///////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


	template<long long Val>
	using I_c = std::integral_constant<long long, Val>;

	template<typename... Ts>
	struct List;


	/////////////////   ListInstance

	template<typename>
	struct DecideTListInstance :std::false_type {};

	template<typename... Ts>
	struct DecideTListInstance<List<Ts...>> :std::true_type {};

	template<typename T>
	concept ListInstance = DecideTListInstance<T>::value;


	/////////////////   Size

	template<ListInstance>
	struct Size;

	template<typename... Ts>
	struct Size<List<Ts...>> :I_c<sizeof...(Ts)> {};

	template<ListInstance L>
	constexpr std::size_t size_v = Size<L>::value;


	/////////////////   Cons

	template<typename, ListInstance>
	struct Cons;

	template<typename T, ListInstance L>
	using Cons_t = typename Cons<T, L>::type;

	template<typename T, typename... Ts>
	struct Cons<T, List<Ts...>> { using type = List<T, Ts...>; };
	
	
	/////////////////   Concat

	template<ListInstance, ListInstance>
	struct Concat;

	template<typename... Elems_1, typename... Elems_2>
	struct Concat<List<Elems_1...>, List<Elems_2...>> { using type = List<Elems_1..., Elems_2...>; };

	template<ListInstance List_1, ListInstance List_2>
	using Concat_t = typename Concat<List_1, List_2>::type;

	static_assert(std::is_same_v<Concat_t<List<int, int>, List<double, nullptr_t>>, List<int, int, double, nullptr_t>>);


	/////////////////   Contains

	template<typename, ListInstance>
	struct Contains;

	template<typename T, ListInstance L>
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

	template<ListInstance, ListInstance>
	struct Intersection;

	template<ListInstance L1, ListInstance L2>
	using Intersection_t = typename Intersection<L1, L2>::type;

	template<typename T, typename... Ts, ListInstance L2>
	class Intersection <List<T, Ts...>, L2> 
	{
		using TailRes = Intersection_t<List<Ts...>, L2>;
	public:
		using type = std::conditional_t<contains_v<T, L2>, Cons_t<T, TailRes>, TailRes>;
	};

	template<ListInstance L2>
	struct Intersection <List<>, L2> { using type = List<>; };

	static_assert(std::is_same_v<Intersection_t<List<bool, int, char>, List<float, double, bool>>, List<bool>>);


	/////////////////   Disjoint

	template<ListInstance L1, ListInstance L2>
	constexpr bool disjoint_v = std::bool_constant<std::is_same_v<Intersection_t<L1, L2>, List<>>>::value;


	/////////////////   Filter

	template<template <typename> class P, ListInstance L>
	struct Filter;

	template<template <typename> class P, ListInstance L>
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

	template<typename T, ListInstance L>
	struct IndexOf;

	template<typename T, ListInstance L>
	constexpr std::size_t index_of_v = IndexOf<T, L>::value;

	template<typename T>
	struct IndexOf<T, List<>> :I_c<0>{};

	template<typename T, typename... Ts>
	struct IndexOf<T, List<T, Ts...>> :I_c<0> {};

	template<typename T1, typename T2, typename... Ts>
	struct IndexOf<T1, List<T2, Ts...>> :I_c<IndexOf<T1, List<Ts...>>::value + 1> {};

	static_assert(index_of_v<I_c<-7>, List<I_c<2>, I_c<5>, I_c<-7>>> == 2);
	static_assert(index_of_v<I_c<-8>, List<I_c<2>, I_c<5>, I_c<-7>>> == 3);


	/////////////////   Drop

	template<std::size_t N, ListInstance L>
	struct Drop
	{
		using type = List<>;
	};

	template<std::size_t N, ListInstance L>
	using Drop_t = typename Drop<N, L>::type;

	template<std::size_t N, typename T, typename... Ts> requires (N <= sizeof...(Ts))
	struct Drop<N, List<T, Ts...>> 
	{
		using type = Drop_t<N - 1, List<Ts...>>;
	};

	template<typename T, typename... Ts>
	struct Drop<0, List<T, Ts...>>
	{
		using type = List<T, Ts...>;
	};

	static_assert(std::is_same_v<Drop_t<2, List<char, int, double>>, List<double>>);


	/////////////////   Take

	template<std::size_t N, ListInstance L>
	struct Take
	{
		using type = L;
	};

	template<std::size_t N, ListInstance L>
	using Take_t = typename Take<N, L>::type;

	template<std::size_t N, typename T, typename... Ts> requires (N <= sizeof...(Ts))
	struct Take<N, List<T, Ts...>>
	{
		using type = Cons_t<T, Take_t<N - 1, List<Ts...>>>;
	};

	template<typename T, typename... Ts>
	struct Take<0, List<T, Ts...>>
	{
		using type = List<>;
	};

	static_assert(std::is_same_v<Take_t<2, List<char, int, double>>, List<char, int>>);


	/////////////////   Sort
	namespace detail_sort {
		template<ListInstance, ListInstance, template<typename, typename> class Compare>
		struct Merge;
	
		template<ListInstance L1, ListInstance L2, template<typename, typename> class Compare>
		using Merge_t = typename Merge<L1, L2, Compare>::type;
	
		template<typename... Ts, template<typename, typename> class Compare>
		struct Merge<List<Ts...>, List<>, Compare> { using type = List<Ts...>; };
	
		template<typename... Ts, template<typename, typename> class Compare>
		struct Merge<List<>, List<Ts...>, Compare> { using type = List<Ts...>; };
	
		template<typename L, typename... Ls, typename R, typename... Rs, template<typename, typename> class Compare>
		struct Merge<List<L, Ls...>, List<R, Rs...>, Compare>
		{
			using type = std::conditional_t<
				Compare<L, R>::value,
				Cons_t<L, Merge_t<List<Ls...>, List<R, Rs...>, Compare>>,
				Cons_t<R, Merge_t<List<L, Ls...>, List<Rs...>, Compare>>
			>;
		};
	} //namespace detail_sort
	
	template<ListInstance, template<typename, typename> class Compare>
	struct Sort;
	
	template<ListInstance L, template<typename, typename> class Compare>
	using Sort_t = typename Sort<L, Compare>::type;
	
	template<template<typename, typename> class Compare>
	struct Sort<List<>, Compare> { using type = List<>; };
	
	template<typename T, template<typename, typename> class Compare>
	struct Sort<List<T>, Compare> { using type = List<T>; };
	
	template<typename... Ts, template<typename, typename> class Compare>
	struct Sort<List<Ts...>, Compare>
	{
	private:
		static constexpr std::size_t n_halfs =sizeof...(Ts) / 2;
		using Lhs = Sort_t<Take_t<n_halfs, List<Ts...>>, Compare>;
		using Rhs = Sort_t<Drop_t<n_halfs, List<Ts...>>, Compare>;
	public:
		using type = detail_sort::Merge_t<Lhs, Rhs, Compare>;
	};
	
	template<typename L, typename R>
	struct Less :std::bool_constant<(L::value < R::value)> {};
	
	static_assert(std::is_same_v<Sort_t<List<I_c<2>, I_c<5>, I_c<-7>>, Less>, List<I_c<-7>, I_c<2>, I_c<5>>>);

} //namespace bmath::intern::meta

namespace bmath::intern::ct {

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////   Operations on Sequences of values   //////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	template<long long Val>
	using I_c = std::integral_constant<long long, Val>;

	//own type, because std::integer_sequence works exclusively for integer types as T
	template<typename T, T... Vals>
	struct Seq
	{
		using value_type = T;
	};

	template<int... ints>
	using Ints = Seq<int, ints...>;


	/////////////////   SeqInstance

	template<typename>
	struct DecideSeqInstance :std::false_type {};

	template<typename T, T... xs>
	struct DecideSeqInstance<Seq<T, xs...>> :std::true_type {};

	template<typename T>
	concept SeqInstance = DecideSeqInstance<T>::value;


	/////////////////   Comparison

	template<SeqInstance A, SeqInstance B>
	constexpr std::bool_constant<std::is_same_v<A, B>> operator==(A, B) { return {}; }

	template<SeqInstance A, SeqInstance B>
	constexpr std::bool_constant<!std::is_same_v<A, B>> operator!=(A, B) { return {}; }


	/////////////////   Size

	template<SeqInstance>
	struct Size;

	template<SeqInstance A>
	constexpr std::size_t size_v = Size<A>::value;

	template<SeqInstance S>
	constexpr std::size_t size(S) { return size_v<S>; }

	template<typename T, T... xs>
	struct Size<Seq<T, xs...>> :I_c<sizeof...(xs)> {};

	/////////////////   Cons

	template<auto, SeqInstance>
	struct Cons;

	template<auto x, SeqInstance A>
	using Cons_t = typename Cons<x, A>::type;

	template<auto x, SeqInstance S>
	constexpr Cons_t<x, S> cons(S) { return {}; }

	template<typename T, T x, T... xs>
	struct Cons<x, Seq<T, xs...>> { using type = Seq<T, x, xs...>; };

	static_assert(std::is_same_v<Cons_t<1, Ints<2, 3, 4>>, Ints<1, 2, 3, 4>>);


	/////////////////   Concat

	template<SeqInstance, SeqInstance>
	struct Concat;

	template<SeqInstance A1, SeqInstance A2>
	using Concat_t = typename Concat<A1, A2>::type;

	template<SeqInstance A1, SeqInstance A2>
	constexpr Concat_t<A1, A2> concat(A1, A2) { return {}; }

	template<typename T, T... xs, T...ys>
	struct Concat<Seq<T, xs...>, Seq<T, ys...>> { using type = Seq<T, xs..., ys...>; };

	static_assert(std::is_same_v<Concat_t<Ints<0, 1>, Ints<2, 3, 4>>, Ints<0, 1, 2, 3, 4>>);


	/////////////////   IndexOf

	template<auto x, SeqInstance A>
	struct IndexOf;

	template<auto x, SeqInstance A>
	constexpr std::size_t index_of_v = IndexOf<x, A>::value;

	template<auto x, SeqInstance A>
	constexpr std::size_t index_of(A) { return index_of_v<x, A>; }

	template<typename T, T x>
	struct IndexOf<x, Seq<T>> :I_c<0> {};

	template<typename T, T x, T... xs>
	struct IndexOf<x, Seq<T, x, xs...>> :I_c<0> {};

	template<typename T, T x, T y, T... ys>
	struct IndexOf<x, Seq<T, y, ys...>> :I_c<IndexOf<x, Seq<T, ys...>>::value + 1> {};

	static_assert(index_of_v<-7, Ints<2, 5, -7>> == 2);
	static_assert(index_of_v<-8, Ints<2, 5, -7>> == 3);


	/////////////////   Filter

	template<template <auto> class P, SeqInstance S>
	struct Filter;

	template<template <auto> class P, SeqInstance S>
	using Filter_t = typename Filter<P, S>::type;

	template<template <auto> class P, typename T, T t, T... ts>
	class Filter<P, Seq<T, t, ts...>>
	{
		using TailRes = Filter_t<P, Seq<T, ts...>>;
	public:
		using type = std::conditional_t<P<t>::value, Cons_t<t, TailRes>, TailRes>;
	};

	template<template <auto> class P, typename T>
	struct Filter<P, Seq<T>> { using type = Seq<T>; };

	template<auto i>
	struct Smaller3 :std::bool_constant<(i < 3)> {};

	static_assert(std::is_same_v<Filter_t<Smaller3, Ints<1, 2, 3, 4, 5, 6, 0>>, Ints<1, 2, 0>>);

	template<typename T, CallableTo<bool, T> P>
	constexpr Seq<T> filter(P, Seq<T>) { return {}; }

	template<typename T, T t, T... ts, CallableTo<bool, T> P>
	constexpr auto filter(P p, Seq<T, t, ts...>)
	{
		constexpr auto filtered_tail = filter(p, Seq<T, ts...>{});
		if constexpr (p(t)) { return cons<t>(filtered_tail); }
		else                { return filtered_tail; }
	}

	static_assert(filter([](int i) { return i < 3; }, Ints<1, 2, 3, 4, 5, 6, 0>{}) == Ints<1, 2, 0>{});

} //namespace bmath::intern::ct
