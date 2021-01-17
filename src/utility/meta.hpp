#pragma once

#include <type_traits>
#include <concepts>

#include <array>
#include <algorithm>

namespace bmath::intern::meta {

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


	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////   Value Types   ////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	template<typename T, T V>
	struct Constant
	{
		static constexpr T value = V;
		explicit constexpr operator T() const { return V; }
		constexpr T val() const { return V; }
	};

	template<auto V>
	using Const_ = Constant<decltype(V), V>;


	template<long long N>
	using Int_ = Constant<long long, N>;


	template<Num T, T V>
	constexpr Constant<T, -V> operator-(Constant<T, V>) { return {}; }

	template<Num T, T V1, T V2>
	constexpr Constant<T, V1 + V2> operator+(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<Num T, T V1, T V2>
	constexpr Constant<T, V1 - V2> operator-(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<Num T, T V1, T V2>
	constexpr Constant<T, V1 * V2> operator*(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<Num T, T V1, T V2>
	constexpr Constant<T, V1 / V2> operator/(Constant<T, V1>, Constant<T, V2>) { return {}; }


	template<bool B>
	using Bool_ = Constant<bool, B>;

	using False_ = Bool_<false>;
	using True_  = Bool_<true>;

	template<bool B1, bool B2>
	constexpr Bool_<B1 && B2> operator&&(Bool_<B1>, Bool_<B2>) { return {}; }

	template<bool B1, bool B2>
	constexpr Bool_<B1 || B2> operator||(Bool_<B1>, Bool_<B2>) { return {}; }

	template<bool B>
	constexpr Bool_<!B> operator!(Bool_<B>) { return {}; }

	template<Ord T, T V1, T V2>
	constexpr Bool_<(V1 < V2)> operator<(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<Ord T, T V1, T V2>
	constexpr Bool_<(V1 > V2)> operator>(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<Ord T, T V1, T V2>
	constexpr Bool_<(V1 <= V2)> operator<=(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<Ord T, T V1, T V2>
	constexpr Bool_<(V1 >= V2)> operator>=(Constant<T, V1>, Constant<T, V2>) { return {}; }


	template<typename T1, typename T2> requires (std::is_empty_v<T1> || std::is_empty_v<T2>)
	constexpr Bool_<std::is_same_v<T1, T2>> operator==(T1, T2) { return {}; }

	template<typename T1, typename T2> requires (std::is_empty_v<T1> || std::is_empty_v<T2>)
	constexpr Bool_<!std::is_same_v<T1, T2>> operator!=(T1, T2) { return {}; }


	static_assert(Int_<4>{} + Int_<6>{} == Int_<10>{});
	static_assert(Int_<4>{} + Int_<8>{} >= Int_<10>{});

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////   Operations on Lists   ////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


	template<typename... Ts>
	struct List 
	{
		consteval Bool_<sizeof...(Ts) == 0> is_empty() { return {}; }
		constexpr Int_<sizeof...(Ts)> size() { return {}; }
	};


	/////////////////   cons

	template<typename T, typename... Ts>
	constexpr List<T, Ts...> cons(List<Ts...>) { return {}; }

	static_assert(cons<int>(List<bool>{}) == List<int, bool>{});


	/////////////////   concat

	template<typename... Ts_1, typename... Ts_2>
	constexpr List<Ts_1..., Ts_2...> concat(List<Ts_1...>, List<Ts_2...>) { return {}; }

	static_assert(concat(List<int, int>(), List<double, nullptr_t>()) == List<int, int, double, nullptr_t>());


	/////////////////   drop

	template<auto n, typename T, typename... Ts> requires (n < 4)
	constexpr auto drop(Int_<n>, List<T, Ts...>)
	{
		if constexpr (n <= 0) { return List<T, Ts...>{}; }
		else if constexpr (n == 1) { return List<Ts...>{}; }
		else { return drop(Int_<n - 1>{}, List<Ts...>{}); }
	}

	template<auto n, typename T1, typename T2, typename T3, typename T4, typename... Ts> requires (n >= 4)
	constexpr auto drop(Int_<n>, List<T1, T2, T3, T4, Ts...>)
	{
		return drop(Int_<n - 4>{}, List<Ts...>{});
	}

	static_assert(drop(Int_<2>{}, List<int, int, long, bool>{}) == List<long, bool>{});
	static_assert(drop(Int_<7>{}, List<int, int, int, bool, int, int, long, bool, double>{}) == List<bool, double>{});


	/////////////////   take

	template<auto n, typename T, typename... Ts> requires (n < 4)
	constexpr auto take(Int_<n>, List<T, Ts...>)
	{
		if constexpr (n <= 0) { return List<>{}; }
		else if constexpr (n == 1) { return List<T>{}; }
		else { return cons<T>(take(Int_<n - 1>{}, List<Ts...>{})); }
	}

	template<auto n, typename T1, typename T2, typename T3, typename T4, typename... Ts> requires (n >= 4)
	constexpr auto take(Int_<n>, List<T1, T2, T3, T4, Ts...>)
	{
		return concat(List<T1, T2, T3, T4>{}, take(Int_<n - 4>{}, List<Ts...>{}));
	}

	static_assert(take(Int_<2>{}, List<int, int, long, bool>{}) == List<int, int>{});
	static_assert(take(Int_<7>{}, List<int, int, int, bool, int, int, long, bool, double>{})
		== List<int, int, int, bool, int, int, long>{});


	/////////////////   access

	template<typename T, typename... Ts>
	constexpr T head(List<T, Ts...>) { return {}; }

	template<typename T, typename... Ts>
	constexpr List<Ts...> tail(List<T, Ts...>) { return {}; }

	template<typename T1, typename T2, typename T3, typename T4, typename... Ts> requires (sizeof...(Ts) > 0)
	constexpr auto last(List<T1, T2, T3, T4, Ts...>) { return last(List<Ts...>{}); }

	template<typename T1, typename T2, typename T3, typename T4>
	constexpr T4 last(List<T1, T2, T3, T4>) { return {}; }

	template<typename T1, typename T2, typename T3>
	constexpr T3 last(List<T1, T2, T3>) { return {}; }

	template<typename T1, typename T2>
	constexpr T2 last(List<T1, T2>) { return {}; }

	template<typename T>
	constexpr T last(List<T>) { return {}; }

	template<long long N, InstanceOf<List> L>
	constexpr auto at(Int_<N> n, L l) { return meta::head(meta::drop(n, l)); }

	static_assert(head(List<Int_<1>, Int_<2>, Int_<3>>{}) == Int_<1>{});
	static_assert(tail(List<Int_<1>, Int_<2>, Int_<3>>{}) == List<Int_<2>, Int_<3>>{});
	static_assert(last(List<Int_<1>, Int_<2>, Int_<3>>{}) == Int_<3>{});
	static_assert(last(List<Int_<1>, Int_<2>, Int_<3>, Int_<4>>{}) == Int_<4>{});
	static_assert(last(List<Int_<1>, Int_<2>, Int_<3>, Int_<4>, Int_<5>>{}) == Int_<5>{});
	static_assert(at(Int_<2>{}, List<Int_<1>, Int_<2>, Int_<3>, Int_<4>, Int_<5>>{}) == Int_<3>{});


	/////////////////   containes
	
	template<typename T>
	constexpr False_ contains(List<>) { return {}; }

	template<typename T, typename Head, typename... Tail>
	constexpr auto contains(List<Head, Tail...>) 
	{
		return Bool_<std::is_same_v<T, Head>>{} || contains<T>(List<Tail...>{});
	}

	static_assert(contains<int>(List<bool, void, double, int, bool>{}));
	static_assert(!contains<int>(List<bool, void, double, bool>{}));


	/////////////////   intersection

	template<typename T, typename... Ts, InstanceOf<List> Rhs>
	constexpr auto intersection(List<T, Ts...>, Rhs)
	{
		constexpr auto tail_intersection = intersection(List<Ts...>{}, Rhs{});
		if constexpr (contains<T>(Rhs{})) { return cons<T>(tail_intersection); }
		else                              { return tail_intersection; }
	}

	template<InstanceOf<List> Rhs>
	constexpr List<> intersection(List<>, Rhs) { return {}; }

	static_assert(intersection(List<bool, int, char>{}, List<float, double, bool>{}) == List<bool>{});


	/////////////////   disjoint

	template<InstanceOf<List> L1, InstanceOf<List> L2>
	constexpr auto disjoint(L1, L2) { return intersection(L1{}, L2{}) == List<>{}; }

	static_assert(disjoint(List<bool, int, char>{}, List<float, double>{}));
	static_assert(!disjoint(List<bool, int, char>{}, List<float, double, int>{}));


	/////////////////   filter

	template<typename P>
	constexpr List<> filter(P, List<>) { return {}; }

	template<typename T, typename... Ts, Callable<T> P> 
	constexpr auto filter(P p, List<T, Ts...>) 
	{
		constexpr InstanceOf<List> auto filtered_tail = filter(p, List<Ts...>{});
		if constexpr (p(T{})) { return cons<T>(filtered_tail); }
		else                  { return filtered_tail; }
	}

	static_assert(
		filter(
			[](auto t) { return !std::is_integral_v<decltype(t)>; },
			List<int, float, bool, double>{}
		) == List<float, double>{});



	/////////////////   map

	template<typename F>
	constexpr List<> map(F, List<>) { return {}; }

	template<typename T, typename... Ts, Callable<T> F>
	constexpr auto map(F f, List<T, Ts...>)
	{
		return cons<decltype(f(std::declval<T>()))>(map(f, List<Ts...>{}));
	}

	//static_assert(map([](auto x) { return x + Int_<4>{}; }, List<Int_<2>, Int_<5>, Int_<-7>>{}) 
	//	== List<Int_<6>, Int_<9>, Int_<-3>>{});


	/////////////////   find

	struct Nothing {};

	template<typename P>
	constexpr Nothing find(List<>, P) { return{}; }

	template<typename T, typename... Ts, Callable<T> P>
		requires (sizeof...(Ts) < 3)
	constexpr auto find(List<T, Ts...>, P p) 
	{
		if constexpr (p(T{})) { return T{}; }
		else                  { return find(List<Ts...>{}, p); }
	}

	template<typename T1, typename T2, typename T3, typename T4, typename... Ts, Callable<T1> P>
	constexpr auto find(List<T1, T2, T3, T4, Ts...>, P p)
	{
		if constexpr      (p(T1{})) { return T1{}; }
		else if constexpr (p(T2{})) { return T2{}; }
		else if constexpr (p(T3{})) { return T3{}; }
		else if constexpr (p(T4{})) { return T4{}; }
		else                        { return find(List<Ts...>{}, p); }
	}

	//static_assert(find(List<Int_<2>, Int_<5>, Int_<-7>>{}, [](auto v) { return v == Int_<-7>{}; }) == Int_<-7>{});


	/////////////////   find_index
	namespace find_detail {
		template<long long N, typename P>
		constexpr Int_<-1> loop(List<>, Int_<N>, P) { return {}; }

		template<typename T, typename... Ts, long long N, Callable<T> P>
			requires (sizeof...(Ts) < 3)
		constexpr auto loop(List<T, Ts...>, Int_<N> n, P p)
		{
			if constexpr (p(T{})) { return n; }
			else                  { return loop(List<Ts...>{}, n + Int_<1>{}, p); }
		}

		template<typename T1, typename T2, typename T3, typename T4, typename... Ts, long long N, Callable<T1> P>
		constexpr auto loop(List<T1, T2, T3, T4, Ts...>, Int_<N> n, P p)
		{
			if constexpr      (p(T1{})) { return n + Int_<0>{}; }
			else if constexpr (p(T2{})) { return n + Int_<1>{}; }
			else if constexpr (p(T3{})) { return n + Int_<2>{}; }
			else if constexpr (p(T4{})) { return n + Int_<3>{}; }
			else                        { return loop(List<Ts...>{}, n + Int_<4>{}, p); }
		}

	} //namespace find_detail

	template<InstanceOf<List> L, typename P>
	constexpr auto find_index(L l, P p) { return find_detail::loop(l, Int_<0>{}, p); }

	//static_assert(find_index(List<Int_<2>, Int_<5>, Int_<-7>>{}, [](auto v) { return (bool)(v == Int_<-7>{}); }) == Int_<2>{});


	/////////////////   find_index

	namespace index_detail {
		template<typename Needle, long long N>
		constexpr Int_<-1> loop(List<>, Int_<N> n) { return {}; }

		template<typename Needle, typename T, typename... Ts, long long N>
			requires (sizeof...(Ts) < 3)
		constexpr auto loop(List<T, Ts...>, Int_<N> n)
		{
			if constexpr (std::is_same_v<Needle, T>) { return n; }
			else                                     { return loop<Needle>(List<Ts...>{}, n + Int_<1>{}); }
		}

		template<typename Needle, typename T1, typename T2, typename T3, typename T4, typename... Ts, long long N>
		constexpr auto loop(List<T1, T2, T3, T4, Ts...>, Int_<N> n)
		{
			if constexpr      (std::is_same_v<Needle, T1>) { return n + Int_<0>{}; }
			else if constexpr (std::is_same_v<Needle, T2>) { return n + Int_<1>{}; }
			else if constexpr (std::is_same_v<Needle, T3>) { return n + Int_<2>{}; }
			else if constexpr (std::is_same_v<Needle, T4>) { return n + Int_<3>{}; }
			else                                           { return loop<Needle>(List<Ts...>{}, n + Int_<4>{}); }
		}
	} //namespace index_detail

	template<typename Needle, InstanceOf<List> L>
	constexpr auto index_of(L l) { return index_detail::loop<Needle>(l, Int_<0>{}); }

	static_assert(index_of<Int_<-7>>(List<Int_<2>, Int_<5>, Int_<-7>>{}) == Int_<2>{});


	/////////////////   sort (function based)

	namespace sort_detail {
		template<typename... Ts, typename C>
		constexpr auto merge(List<Ts...> l, List<>, C) { return l; }

		template<typename... Ts, typename C>
		constexpr auto merge(List<>, List<Ts...> l, C) { return l; }

		template<typename L, typename... Ls, typename R, typename... Rs, Callable<L, R> C>
		constexpr auto merge(List<L, Ls...>, List<R, Rs...>, C c)
		{
			if constexpr (c(L{}, R{})) { return cons<L>(merge(List<Ls...>{}, List<R, Rs...>{}, c)); }
			else                       { return cons<R>(merge(List<L, Ls...>{}, List<Rs...>{}, c)); }
		}
	} //namespace sort_detail

	template<typename C>
	constexpr List<> sort(List<>, C) { return {}; }

	template<typename T, typename C>
	constexpr List<T> sort(List<T>, C) { return {}; }

	template<typename T1, typename T2, typename... Ts, Callable<T1, T2> C>
	constexpr auto sort(List<T1, T2, Ts...> l, C c)
	{
		constexpr auto n_halfs = Int_<sizeof...(Ts) / 2 + 1>{};
		auto lhs = sort(take(n_halfs, l), c);
		auto rhs = sort(drop(n_halfs, l), c);
		return sort_detail::merge(lhs, rhs, c);
	}

	static_assert(sort(List<Int_<3>, Int_<2>, Int_<5>, Int_<10>, Int_<-7>>{}, [](auto l, auto r) { return l < r; })
		== List<Int_<-7>, Int_<2>, Int_<3>, Int_<5>, Int_<10>>{});


	/////////////////   sort (type based)

	namespace sort_detail {
		template<InstanceOf<List>, InstanceOf<List>, template<typename, typename> class Compare>
		struct Merge;
	
		template<InstanceOf<List> L1, InstanceOf<List> L2, template<typename, typename> class Compare>
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
				decltype(cons<L>(Merge_t<List<Ls...>, List<R, Rs...>, Compare>{})),
				decltype(cons<R>(Merge_t<List<L, Ls...>, List<Rs...>, Compare>{}))
			>;
		};
	} //namespace sort_detail
	
	template<InstanceOf<List>, template<typename, typename> class Compare>
	struct Sort;
	
	template<InstanceOf<List> L, template<typename, typename> class Compare>
	using Sort_t = typename Sort<L, Compare>::type;
	
	template<template<typename, typename> class Compare>
	struct Sort<List<>, Compare> { using type = List<>; };
	
	template<typename T, template<typename, typename> class Compare>
	struct Sort<List<T>, Compare> { using type = List<T>; };
	
	template<typename... Ts, template<typename, typename> class Compare>
	struct Sort<List<Ts...>, Compare>
	{
	private:
		static constexpr auto n_halfs = Int_<sizeof...(Ts) / 2>{};
		using Lhs = Sort_t<decltype(take(n_halfs, List<Ts...>{})), Compare>;
		using Rhs = Sort_t<decltype(drop(n_halfs, List<Ts...>{})), Compare>;
	public:
		using type = sort_detail::Merge_t<Lhs, Rhs, Compare>;
	};
	
	template<typename L, typename R>
	struct Less :Bool_<(bool)(L{} < R{})> {};
	
	static_assert(Sort_t<List<Int_<2>, Int_<5>, Int_<-7>>, Less>{} == List<Int_<-7>, Int_<2>, Int_<5>>{});


	/////////////////   reverse

	namespace reverse_detail {
		template<typename... Ts>
		constexpr auto loop(List<Ts...> l, List<>) { return l; }

		template<typename... Ls, typename R, typename... Rs> requires (sizeof...(Rs) < 3)
		constexpr auto loop(List<Ls...>, List<R, Rs...>) 
		{ 
			return loop(List<R, Ls...>{}, List<Rs...>{});
		}

		template<typename... Ls, typename R1, typename R2, typename R3, typename R4, typename... Rs>
			constexpr auto loop(List<Ls...>, List<R1, R2, R3, R4, Rs...>)
		{
			return loop(List<R4, R3, R2, R1, Ls...>{}, List<Rs...>{});
		}
	} //namespace reverse_detail

	template<InstanceOf<List> L>
	constexpr auto reverse(L l) { return reverse_detail::loop(List<>{}, l); }

	static_assert(reverse(List<int, bool, char, float, double>{}) == List<double, float, char, bool, int>{});

} //namespace bmath::intern::meta

namespace bmath::intern::arr {
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////   Operations on std::array   ///////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	template<std::array arr>
	using Array_ = meta::Constant<decltype(arr), arr>;

	template<typename T1, typename T2, std::size_t N>
	constexpr bool holds(std::array<T2, N>) { return std::is_same_v<T1, T2>; }


	/////////////////   combining and removing

	template<typename T, std::size_t N1, std::size_t N2>
	constexpr std::array<T, N1 + N2> concat(const std::array<T, N1>& arr_1, const std::array<T, N2>& arr_2)
	{
		std::array<T, N1 + N2> result;
		std::copy_n(arr_1.data(), N1, result.data());
		std::copy_n(arr_2.data(), N2, result.data() + N1);
		return result;
	}

	template<typename T, std::size_t N>
	constexpr std::array<T, N + 1> cons(const T& val, const std::array<T, N>& arr)
	{
		std::array<T, N + 1> result = { val };
		std::copy_n(arr.data(), N, result.data() + 1);
		return result;
	}

	template<std::size_t Delta, typename T, std::size_t N> requires (N >= Delta)
		constexpr std::array<T, N - Delta> drop(const std::array<T, N>& arr)
	{
		std::array<T, N - Delta> result;
		std::copy_n(arr.data() + Delta, N - Delta, result.data());
		return result;
	}

	template<std::size_t Delta, typename T, std::size_t N> requires (N >= Delta)
		constexpr std::array<T, Delta> take(const std::array<T, N>& arr)
	{
		std::array<T, Delta> result;
		std::copy_n(arr.data(), Delta, result.data());
		return result;
	}


	/////////////////   map

	namespace from_list_detail {
		template<typename T1, meta::Callable<T1> F>
		constexpr auto loop(meta::List<T1>, F f) { return std::array{ f(T1{}) }; }

		template<typename T1, typename T2, meta::Callable<T1> F>
		constexpr auto loop(meta::List<T1, T2>, F f) { return std::array{ f(T1{}), f(T2{}) }; }

		template<typename T1, typename T2, typename T3, meta::Callable<T1> F>
		constexpr auto loop(meta::List<T1, T2, T3>, F f) { return std::array{ f(T1{}), f(T2{}), f(T3{}) }; }

		template<typename T1, typename T2, typename T3, typename T4, meta::Callable<T1> F>
		constexpr auto loop(meta::List<T1, T2, T3, T4>, F f) { return std::array{ f(T1{}), f(T2{}), f(T3{}), f(T4{}) }; }

		template<typename T1, typename T2, typename T3, typename T4, typename... Ts, meta::Callable<T1> F>
			requires (sizeof...(Ts) > 0)
		constexpr auto loop(meta::List<T1, T2, T3, T4, Ts...>, F f)
		{
			return arr::concat(std::array{ f(T1{}), f(T2{}), f(T3{}), f(T4{}) }, loop(meta::List<Ts...>{}, f));
		}
	} //namespace from_list_detail

	template<typename T, typename... Ts, meta::Callable<T> F>
	constexpr auto from_list(F f, meta::List<T, Ts...> l) { return from_list_detail::loop<T>(l, f); }

	//static_assert(from_list([](auto x) { return x.val(); }, meta::List<meta::Int_<1>, meta::Int_<2>, meta::Int_<3>>{})
	//	== std::to_array({ 1ll, 2ll, 3ll }));



	template<typename T, std::size_t N>
	constexpr long long index_of(const T& t, const std::array<T, N>& arr)
	{
		const auto iter = std::find(arr.begin(), arr.end(), t);
		if (iter != arr.end()) {
			return std::distance(arr.begin(), iter);
		}
		return -1ll;
	}

	static_assert(index_of(4, std::array{ 1, 2, 4, 5, 6, 7 }) == 2);
	static_assert(index_of(8, std::array{ 1, 2, 4, 5, 6, 7 }) == -1);



	template<typename T, std::size_t N, meta::Callable<T> F>
	constexpr auto map(F f, const std::array<T, N>& arr)
	{
		std::array<decltype(f(arr[0])), N> result = {};
		std::transform(arr.begin(), arr.end(), result.begin(), f);
		return result;
	}

	static_assert(arr::map([](auto x) { return x + 3; }, std::array{ 1, 2, 3 }) == std::array{ 4, 5, 6 });


	template<typename T, T... xs>
	constexpr std::array<T, sizeof...(xs)> make() { return { xs... }; }

	static_assert(make<int>() == std::array<int, 0>{});

} //namespace bmath::intern::arr