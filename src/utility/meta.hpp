#pragma once

#include <type_traits>
#include <concepts>
#include <compare>
#include <iterator>
#include <algorithm>
#include <cassert>
#include <utility>

namespace simp::detail_print_type {
	template<typename T>
	struct PrintType
	{
		static_assert(std::is_same_v<void, T>&& std::is_same_v<int, T>);
		static constexpr bool value = true;
	};
} //namespace simp::detail_print_type

#define BMATH_PRINT_TYPE(T) static_assert(detail_print_type::PrintType<T>::value)


//general concepts
namespace simp {
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
	concept MinimalNum = Eq<T> &&
		requires (T a, T b) {
			{a += b} -> std::same_as<T&>;
			{a -= b} -> std::same_as<T&>;
			{a *= b} -> std::same_as<T&>;
			{a /= b} -> std::same_as<T&>;
	};

	template<typename T>
	concept Num = MinimalNum<T> &&
		requires (T a, T b) {
			{-a}    -> std::same_as<T>;
			{a + b} -> std::same_as<T>;
			{a - b} -> std::same_as<T>;
			{a * b} -> std::same_as<T>;
			{a / b} -> std::same_as<T>;
	};


	template<typename From, typename To>
	concept ExplicitlyConvertibleTo =
		requires (From from) { static_cast<To>(from); };


	/////////////////   Callable

	template<typename F, typename... Args>
	concept Callable = requires (F f, Args... args) { f(args...); };

	template<typename F, typename R, typename... Args>
	concept CallableTo = requires (F f, Args... args) { { f(args...) } -> std::convertible_to<R>; };

	template<typename F, typename... Args>
	concept Procedure = requires (F f, Args... args) { { f(args...) } -> std::same_as<void>; };


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
		 requires (I i) { {*i} -> std::convertible_to<const T&>; }
	;


	/////////////////  Container

	template<typename C, typename T>
	concept ContainerOf = std::is_same_v<T, typename C::value_type> &&
		requires (C c) { {c.begin()} -> IterOver<T>;
			             {c.end()  } -> IterOver<T>;
						 {c.size() } -> std::convertible_to<std::size_t>;
	};

	static_assert(ContainerOf<std::string_view, char>);




	/////////////////  number parsing

	//expects iterator pair of first char to parse and first char to not parse
	//returns parsed number as .first and smallest power of 10 larger than parsed number as .second
	template<IterOver<char> Iter>
	constexpr std::pair<unsigned long long, unsigned long long> parse_ull(const Iter begin_, Iter iter)
	{
		auto res = std::pair{ 0ull, 1ull };
		while (iter > begin_) {
			const unsigned digit = *(--iter) - unsigned('0');
			assert(digit < 10);
			res.first += digit * res.second;
			res.second *= 10;
		}
		return res;
	}

	constexpr auto _1234 = "12345.250";
	static_assert(parse_ull(+_1234, _1234 + 4).first == 1234);

	template<IterOver<char> Iter>
	constexpr double parse_double(const Iter start, const Iter stop) 
	{
		const Iter dot_pos = std::find(start, stop, '.');
		const double integer_part = parse_ull(start, dot_pos).first;

		if (dot_pos < stop) {
			const Iter decimal_start = dot_pos + 1;
			const auto [upscaled_decimals, upscale_factor] = parse_ull(decimal_start, stop);
			return integer_part + upscaled_decimals / (double)upscale_factor;
		}
		return integer_part;
	}

	static_assert(parse_double(+_1234, _1234 + 8) == 12345.25);

} //namespace simp

namespace simp::meta {

	template<auto Val>
	struct Constant
	{
		using value_type = decltype(Val);
		using type = Constant<Val>;
		static constexpr auto value = Val;
	};



	template<typename T1, typename T2>
	struct Pair {};


	template<InstanceOf<Pair> P>
	struct Fst;

	template<InstanceOf<Pair> P>
	using Fst_t = typename Fst<P>::type;

	template<typename T1, typename T2>
	struct Fst<Pair<T1, T2>> { using type = T1; };


	template<InstanceOf<Pair> P>
	struct Snd;

	template<InstanceOf<Pair> P>
	using Snd_t = typename Snd<P>::type;

	template<typename T1, typename T2>
	struct Snd<Pair<T1, T2>> { using type = T2; };



	template<typename T>
	struct ValueIdentity { static constexpr auto value = T::value; };



	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////   Operations on Lists of types   ///////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	template<typename... Ts>
	struct List {};


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
	struct Size<List<Ts...>> :Constant<sizeof...(Ts)> {};

	template<ListInstance L>
	constexpr std::size_t size_v = Size<L>::value;


	/////////////////   Element Access

	template<typename T, typename...>
	using DirectHead_t = T;

	template<ListInstance L>
	struct Head;

	template<ListInstance L>
	using Head_t = typename Head<L>::type;

	template<typename T, typename... Ts>
	struct Head<List<T, Ts...>> { using type = T; };


	template<ListInstance L>
	struct Last;

	template<ListInstance L>
	using Last_t = typename Last<L>::type;

	template<typename T>
	struct Last<List<T>> { using type = T; };

	template<typename T, typename... Ts>
	struct Last<List<T, Ts...>> { using type = Last_t<List<Ts...>>; };

	static_assert(std::is_same_v<int, Last_t<List<bool, double, float, int>>>);


	/////////////////   Cons

	template<typename, ListInstance>
	struct Cons;

	template<typename T, ListInstance L>
	using Cons_t = typename Cons<T, L>::type;

	template<typename T, typename... Ts>
	struct Cons<T, List<Ts...>> { using type = List<T, Ts...>; };
	
	
	/////////////////   Concat

	template<ListInstance...>
	struct Concat { using type = List<>; };

	template<typename... Ls>
	using Concat_t = typename Concat<Ls...>::type;

	template<ListInstance L>
	struct Concat<L> { using type = L; };

	template<typename... Ts, typename... Us>
	struct Concat<List<Ts...>, List<Us...>> { using type = List<Ts..., Us...>; };

	template<typename... Ts, typename... Us, typename... Vs>
	struct Concat<List<Ts...>, List<Us...>, List<Vs...>> { using type = List<Ts..., Us..., Vs...>; };

	template<typename... Ts, typename... Us, typename... Vs, typename... Ws>
	struct Concat<List<Ts...>, List<Us...>, List<Vs...>, List<Ws...>> { using type = List<Ts..., Us..., Vs..., Ws...>; };

	template<typename... Ts, typename... Us, typename... Vs, typename... Ws, typename... Lists>
	struct Concat<List<Ts...>, List<Us...>, List<Vs...>, List<Ws...>, Lists...> 
	{ 
		using type = Concat_t<List<Ts..., Us..., Vs..., Ws...>, Lists...>; 
	};

	static_assert(std::is_same_v<Concat_t<List<int, int>, List<double, nullptr_t>>, List<int, int, double, nullptr_t>>);


	/////////////////   Contains

	template<typename, ListInstance>
	struct Contains;

	template<typename T, ListInstance L>
	constexpr bool contains_v = Contains<T, L>::value;

	template<typename T, typename... Ts>
	struct Contains<T, List<Ts...>> 
		:std::bool_constant<(std::is_same_v<T, Ts> || ...)> {};

	template<typename T, typename... Ts>
	constexpr bool direct_contains_v = (std::is_same_v<T, Ts> || ...);

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


	/////////////////   Find
	struct FoundNothing {};

	template<template <typename> class P, ListInstance L>
	struct Find { using type = FoundNothing; };

	template<template <typename> class P, ListInstance L>
	using Find_t = typename Find<P, L>::type;

	template<template <typename> class P, typename T, typename... Ts>
		requires (P<T>::value)
	struct Find<P, List<T, Ts...>> { using type = T; };

	template<template <typename> class P, typename T, typename... Ts>
		requires (!P<T>::value)
	struct Find<P, List<T, Ts...>> { using type = Find_t<P, List<Ts...>>; };


	/////////////////   Map

	template<template<typename...> class Container, template<typename> class F, typename... Ts>
	using DirectMap_t = Container<typename F<Ts>::type...>;

	template<template<typename> class F, ListInstance L>
	struct Map;

	template<template<typename> class F, ListInstance L>
	using Map_t = typename Map<F, L>::type;

	template<template<typename> class F, typename... Ts>
	struct Map<F, List<Ts...>> { using type = List<typename F<Ts>::type...>; };


	/////////////////   Foldl

	template<template<typename, typename> class F, typename Init, typename... Ts>
	struct DirectFoldl { using type = Init; };

	template<template<typename, typename> class F, typename Init, typename... Ts>
	using DirectFoldl_t = typename DirectFoldl<F, Init, Ts...>::type;

	template<template<typename, typename> class F, typename Init, typename T, typename... Ts>
	struct DirectFoldl<F, Init, T, Ts...> { using type = DirectFoldl_t<F, typename F<Init, T>::type, Ts...>; };

	template<template<typename, typename> class F, typename Init, ListInstance L>
	struct Foldl;

	template<template<typename, typename> class F, typename Init, ListInstance L>
	using Foldl_t = typename Foldl<F, Init, L>::type;

	template<template<typename, typename> class F, typename Init, typename... Ts>
	struct Foldl<F, Init, List<Ts...>>
	{
		using type = DirectFoldl_t<F, Init, Ts...>;
	};


	/////////////////   IndexOf

	template<typename T, ListInstance L>
	struct IndexOf;

	template<typename T, ListInstance L>
	constexpr std::size_t index_of_v = IndexOf<T, L>::value;

	template<typename T>
	struct IndexOf<T, List<>> :Constant<0>{};

	template<typename T, typename... Ts>
	struct IndexOf<T, List<T, Ts...>> :Constant<0> {};

	template<typename T1, typename T2, typename... Ts>
	struct IndexOf<T1, List<T2, Ts...>> :Constant<IndexOf<T1, List<Ts...>>::value + 1> {};

	static_assert(index_of_v<Constant<7>, List<Constant<2>, Constant<5>, Constant<7>>> == 2);
	static_assert(index_of_v<Constant<8>, List<Constant<2>, Constant<5>, Constant<7>>> == 3);


	/////////////////   Drop

	template<std::size_t N, ListInstance L>
	struct Drop { using type = List<>; };

	template<std::size_t N, ListInstance L>
	using Drop_t = typename Drop<N, L>::type;

	template<std::size_t N, typename T, typename... Ts> requires (N <= sizeof...(Ts))
	struct Drop<N, List<T, Ts...>> { using type = Drop_t<N - 1, List<Ts...>>; };

	template<typename T, typename... Ts>
	struct Drop<0, List<T, Ts...>> { using type = List<T, Ts...>; };

	static_assert(std::is_same_v<Drop_t<2, List<char, int, double>>, List<double>>);


	/////////////////   Take

	template<std::size_t N, ListInstance L>
	struct Take { using type = L; };

	template<std::size_t N, ListInstance L>
	using Take_t = typename Take<N, L>::type;

	template<std::size_t N, typename T, typename... Ts> requires (N <= sizeof...(Ts))
	struct Take<N, List<T, Ts...>> { using type = Cons_t<T, Take_t<N - 1, List<Ts...>>>; };

	template<typename T, typename... Ts>
	struct Take<0, List<T, Ts...>> { using type = List<>; };

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
	
	static_assert(std::is_same_v<Sort_t<List<Constant<2>, Constant<5>, Constant<1>>, Less>, List<Constant<1>, Constant<2>, Constant<5>>>);


	/////////////////   compare lists lexicografically

	template<ListInstance L1, ListInstance L2, template<typename, typename> class ElemSmaller>
	struct Smaller;

	template<ListInstance L1, ListInstance L2, template<typename, typename> class ElemSmaller>
	constexpr bool smaller_v = Smaller<L1, L2, ElemSmaller>::value;

	template<template<typename, typename> class ElemSmaller>
	struct Smaller<List<>, List<>, ElemSmaller> :std::false_type {};

	template<template<typename, typename> class ElemSmaller, typename... Ts>
	struct Smaller<List<Ts...>, List<>, ElemSmaller> :std::false_type {};

	template<template<typename, typename> class ElemSmaller, typename... Ts>
	struct Smaller<List<>, List<Ts...>, ElemSmaller> :std::true_type {};

	template<template<typename, typename> class ElemSmaller, typename T, typename... Ts, typename U, typename... Us>
	struct Smaller<List<T, Ts...>, List<U, Us...>, ElemSmaller>
		:std::bool_constant<ElemSmaller<T, U>::value || smaller_v<List<Ts...>, List<Us...>, ElemSmaller>> {};

	template<typename T1, typename T2> 
	struct IntSmallerBool :std::bool_constant<std::is_same_v<int, T1> && std::is_same_v<bool, T2>> {};
	static_assert(smaller_v<List<int, bool, bool>, List<bool>, IntSmallerBool>);
	static_assert(!smaller_v<List<bool, int, bool>, List<bool, int>, IntSmallerBool>);
	static_assert(!smaller_v<List<bool, int, bool>, List<bool, int, bool>, IntSmallerBool>);





	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////   Operations on Sequences of values   //////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	template<auto... Vals>
	struct Seq {};


	/////////////////   SeqInstance

	template<typename>
	struct DecideSeqInstance :std::false_type {};

	template<auto... xs>
	struct DecideSeqInstance<Seq<xs...>> :std::true_type {};

	template<typename T>
	concept SeqInstance = DecideSeqInstance<T>::value;


	/////////////////   Comparison

	template<SeqInstance A, SeqInstance B>
	constexpr std::bool_constant<std::is_same_v<A, B>> operator==(A, B) { return {}; }

	template<SeqInstance A, SeqInstance B>
	constexpr std::bool_constant<!std::is_same_v<A, B>> operator!=(A, B) { return {}; }


	/////////////////   Size

	template<SeqInstance>
	struct SSize;

	template<SeqInstance A>
	constexpr std::size_t ssize_v = SSize<A>::value;

	template<SeqInstance S>
	constexpr std::size_t size(S) { return size_v<S>; }

	template<auto... xs>
	struct SSize<Seq<xs...>> :Constant<sizeof...(xs)> {};


	/////////////////   SCons

	template<auto, SeqInstance>
	struct SCons;

	template<auto x, SeqInstance A>
	using SCons_t = typename SCons<x, A>::type;

	template<auto x, SeqInstance S>
	constexpr SCons_t<x, S> cons(S) { return {}; }

	template<auto x, auto... xs>
	struct SCons<x, Seq<xs...>> { using type = Seq<x, xs...>; };

	static_assert(std::is_same_v<SCons_t<1, Seq<2, 3, 4>>, Seq<1, 2, 3, 4>>);


	/////////////////   contains

	template<auto x, SeqInstance S>
	struct SContains;

	template<auto x, SeqInstance S>
	constexpr bool scontains_v = SContains<x, S>::value;

	template<auto x, auto... xs>
	struct SContains<x, Seq<xs...>> { static constexpr bool value = ((x == xs) || ...); };

	template<auto x, auto... xs>
	constexpr bool direct_scontains_v = ((x == xs) || ...);

	static_assert(direct_scontains_v<2, 1, 4, 2, 3>);
	static_assert(!direct_scontains_v<7, 1, 4, 2, 3>);

	/////////////////   intersection

	template<SeqInstance, SeqInstance>
	struct SInstersection;

	template<SeqInstance S1, SeqInstance S2>
	using SIntersection_t = typename SInstersection<S1, S2>::type;

	template<auto... ys>
	struct SInstersection<Seq<>, Seq<ys...>> { using type = Seq<>; };

	template<auto x, auto... xs, auto... ys>
	class SInstersection<Seq<x, xs...>, Seq<ys...>>
	{
		using Tail = SIntersection_t<Seq<xs...>, Seq<ys...>>;
	public:
		using type = std::conditional_t<direct_scontains_v<x, ys...>,
			SCons_t<x, Tail>,
			Tail
		>;
	};


	/////////////////   SeqToList

	template<template <auto> class F, SeqInstance S>
	struct SeqToList;

	template<template <auto> class F, SeqInstance S>
	using SeqToList_t = typename SeqToList<F, S>::type;

	template<template <auto> class F, auto... xs>
	struct SeqToList<F, Seq<xs...>>
	{
		using type = List<typename F<xs>::type...>;
	};

	static_assert(std::is_same_v<List<Constant<1>, Constant<2>, Constant<3>>, SeqToList_t<Constant, Seq<1, 2, 3>>>);


	/////////////////   SConcat

	template<SeqInstance...>
	struct SConcat { using type = Seq<>; };

	template<typename... Ls>
	using SConcat_t = typename SConcat<Ls...>::type;

	template<SeqInstance L>
	struct SConcat<L> { using type = L; };

	template<auto... xs, auto... ys>
	struct SConcat<Seq<xs...>, Seq<ys...>> { using type = Seq<xs..., ys...>; };

	template<auto... xs, auto... ys, auto... zs>
	struct SConcat<Seq<xs...>, Seq<ys...>, Seq<zs...>> { using type = Seq<xs..., ys..., zs...>; };

	template<auto... xs, auto... ys, auto... zs, typename... Seqs>
	struct SConcat<Seq<xs...>, Seq<ys...>, Seq<zs...>, Seqs...>
	{
		using type = SConcat_t<Seq<xs..., ys..., zs...>, Seqs...>;
	};

	static_assert(std::is_same_v<SConcat_t<
		Seq<0, 1>, Seq<2, 3, 4>, Seq<1, 1>, Seq<0>, Seq<1, 2>>, 
		Seq<0, 1, 2, 3, 4, 1, 1, 0, 1, 2>
	>);


	/////////////////   IndexOf

	template<auto x, SeqInstance A>
	struct SIndexOf;

	template<auto x, SeqInstance A>
	constexpr std::size_t sindex_of_v = SIndexOf<x, A>::value;

	template<auto x, SeqInstance A>
	constexpr std::size_t index_of(A) { return index_of_v<x, A>; }

	template<auto x>
	struct SIndexOf<x, Seq<>> :Constant<0> {};

	template<auto x, auto... xs>
	struct SIndexOf<x, Seq<x, xs...>> :Constant<0> {};

	template<auto x, auto y, auto... ys>
	struct SIndexOf<x, Seq<y, ys...>> :Constant<SIndexOf<x, Seq<ys...>>::value + 1> {};

	static_assert(sindex_of_v<-7, Seq<2, 5, -7>> == 2);
	static_assert(sindex_of_v<-8, Seq<2, 5, -7>> == 3);


	/////////////////   Filter

	template<template <auto> class P, SeqInstance S>
	struct SFilter;

	template<template <auto> class P, SeqInstance S>
	using SFilter_t = typename SFilter<P, S>::type;

	template<template <auto> class P, auto x, auto... xs>
	class SFilter<P, Seq<x, xs...>>
	{
		using TailRes = SFilter_t<P, Seq<xs...>>;
	public:
		using type = std::conditional_t<P<x>::value, SCons_t<x, TailRes>, TailRes>;
	};

	template<template <auto> class P>
	struct SFilter<P, Seq<>> { using type = Seq<>; };

	template<auto i>
	struct Smaller3 :std::bool_constant<(i < 3)> {};

	static_assert(std::is_same_v<SFilter_t<Smaller3, Seq<1, 2, 3, 4, 5, 6, 0>>, Seq<1, 2, 0>>);

	template<typename Predicate>
	constexpr Seq<> filter(Predicate, Seq<>) { return {}; }

	template<typename T, T x, auto... xs, CallableTo<bool, T> P>
	constexpr auto filter(P p, Seq<x, xs...>)
	{
		constexpr auto filtered_tail = filter(p, Seq<xs...>{});
		if constexpr (p(x)) { return cons<x>(filtered_tail); }
		else                { return filtered_tail; }
	}

	static_assert(filter([](int i) { return i < 3; }, Seq<1, 2, 3, 4, 5, 6, 0>{}) == Seq<1, 2, 0>{});

} //namespace simp::meta
