#pragma once

#include <type_traits>
#include <concepts>
#include <compare>
#include <iterator>
#include <algorithm>
#include <cassert>
#include <utility>

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
		(requires (I i) { {*i} -> std::same_as<T&>;       } || 
		 requires (I i) { {*i} -> std::same_as<const T&>; })
	;


	/////////////////  Container

	template<typename C, typename T>
	concept ContainerOf = std::is_same_v<T, typename C::value_type> &&
		requires (C c) { {c.begin()} -> IterOver<T>;
			             {c.end()  } -> IterOver<T>;
	};

	static_assert(ContainerOf<std::array<int, 3>, int>);
	static_assert(ContainerOf<std::string_view, char>);




	/////////////////  number parsing

	//expects iterator pair of first char to parse and first char to not parse
	//returns parsed number as .first and smallest power of 10 larger than parsed number as .second
	template<IterOver<char> Iter>
	constexpr std::pair<unsigned long long, unsigned long long> parse_ull(const Iter begin_, Iter iter)
	{
		auto res = std::pair{ 0ull, 1ull };
		while (iter > begin_) {
			const unsigned char digit = *(--iter) - '0';
			assert(digit < 10);
			res.first += digit * res.second;
			res.second *= 10;
		}
		return res;
	}

	template<typename T>
	constexpr unsigned long long parse_ull(const T& chars) { return parse_ull(chars.begin(), chars.end()).first; }

	constexpr auto _1234_sv = std::string_view("1234");
	static_assert(parse_ull(std::string_view("1234")) == 1234);

	template<IterOver<char> Iter>
	constexpr double parse_double(const Iter start, const Iter stop) 
	{
		const Iter dot_pos = std::find(start, stop, '.');
		const double integer_part = intern::parse_ull(start, dot_pos).first;

		if (dot_pos < stop) {
			const Iter decimal_start = dot_pos + 1;
			const auto [upscaled_decimals, upscale_factor] = intern::parse_ull(decimal_start, stop);
			return integer_part + upscaled_decimals / (double)upscale_factor;
		}
		return integer_part;
	}

	template<typename T>
	constexpr double parse_double(const T& chars) { return parse_double(chars.begin(), chars.end()); }

	static_assert(parse_double(std::string_view("1234")) == 1234.0);

} //namespace bmath::intern

namespace bmath::intern::meta {

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


	/////////////////   SConcat

	template<SeqInstance, SeqInstance>
	struct SConcat;

	template<SeqInstance A1, SeqInstance A2>
	using SConcat_t = typename SConcat<A1, A2>::type;

	template<SeqInstance A1, SeqInstance A2>
	constexpr SConcat_t<A1, A2> concat(A1, A2) { return {}; }

	template<auto... xs, auto... ys>
	struct SConcat<Seq<xs...>, Seq<ys...>> { using type = Seq<xs..., ys...>; };

	static_assert(std::is_same_v<SConcat_t<Seq<0, 1>, Seq<2, 3, 4>>, Seq<0, 1, 2, 3, 4>>);


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

} //namespace bmath::intern::meta
