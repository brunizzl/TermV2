#pragma once

#include <type_traits>
#include <concepts>

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

	template<typename>
	struct Type {};



	template<typename T, T V>
	struct Constant
	{
		static constexpr T value = V;
		using type = T;
		explicit constexpr operator T() const { return V; }
	};


	template<typename C> requires (requires { C::value; })
	constexpr typename C::type value(C) { return C::value; }



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


	template<Eq T, T V1, T V2>
	constexpr Bool_<V1 == V2> operator==(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<Eq T, T V1, T V2>
	constexpr Bool_<V1 != V2> operator!=(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<Ord T, T V1, T V2>
	constexpr Bool_<(V1 < V2)> operator<(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<Ord T, T V1, T V2>
	constexpr Bool_<(V1 > V2)> operator>(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<Ord T, T V1, T V2>
	constexpr Bool_<(V1 <= V2)> operator<=(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<Ord T, T V1, T V2>
	constexpr Bool_<(V1 >= V2)> operator>=(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<typename T1, typename T2>
	constexpr Bool_<std::is_same_v<T1, T2>> operator==(Type<T1>, Type<T2>) { return {}; }

	template<typename T1, typename T2>
	constexpr Bool_<!std::is_same_v<T1, T2>> operator!=(Type<T1>, Type<T2>) { return {}; }


	static_assert(Int_<4>{} + Int_<6>{} == Int_<10>{});

	static_assert(Int_<4>{} + Int_<8>{} >= Int_<10>{});

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////   Operations on Lists   ////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


	template<typename...>
	struct List {};


	/////////////////   is_empty

	template<typename... Ts>
	constexpr Bool_<sizeof...(Ts) == 0> is_empty(List<Ts...>) { return {}; }

	static_assert(is_empty(List<>{}));
	static_assert(!is_empty(List<int, bool>{}));


	/////////////////   size

	template<typename... Ts>
	constexpr Int_<sizeof...(Ts)> size(List<Ts...>) { return {}; }

	static_assert(size(List<int, bool, void>{}) == Int_<3>{});


	/////////////////   comparing lists

	template<InstanceOf<List>, InstanceOf<List>>
	struct EqualLists :False_ {};

	template<typename... Ts>
	struct EqualLists<List<Ts...>, List<Ts...>> :True_ {};

	template<InstanceOf<List> L1, InstanceOf<List> L2>
	constexpr Bool_<EqualLists<L1, L2>::value> operator==(L1, L2) { return {}; }

	template<InstanceOf<List> L1, InstanceOf<List> L2>
	constexpr Bool_<!EqualLists<L1, L2>::value> operator!=(L1, L2) { return {}; }

	static_assert(List<int, bool>{} == List<int, bool>{});
	static_assert(List<int>{} != List<int, bool>{});

	/////////////////   cons

	template<typename T, typename... Ts>
	constexpr List<T, Ts...> cons(List<Ts...>) { return {}; }

	static_assert(cons<int>(List<bool>{}) == List<int, bool>{});


	/////////////////   concat

	template<typename... Ts_1, typename... Ts_2>
	constexpr List<Ts_1..., Ts_2...> concat(List<Ts_1...>, List<Ts_2...>) { return {}; }

	static_assert(concat(List<int, int>(), List<double, nullptr_t>()) == List<int, int, double, nullptr_t>());


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

	template<typename T, typename... Ts, CallableTo<bool, T> P> 
	constexpr auto filter(P p, List<T, Ts...>) 
	{
		constexpr auto filtered_tail = filter(p, List<Ts...>{});
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


	/////////////////   index

	template<typename Needle, typename T, typename... Ts>
	constexpr auto index(List<T, Ts...>) 
	{
		if constexpr (Type<Needle>{} == Type<T>{}) { return Int_<0>{}; }
		else if constexpr (sizeof...(Ts) == 0)     { return Int_<-1>{}; }
		else {
			const auto tail_idx = index<Needle>(List<Ts...>{}); 
			if constexpr (tail_idx != Int_<-1>{}) { return tail_idx + Int_<1>{}; }
			else                                  { return Int_<-1>{}; }
		}
	}

	static_assert(index<int>(List<char, bool, float, int, double, long>{}) == Int_<3>{});
	static_assert(index<int>(List<char, bool, float, double, long>{}) == Int_<-1>{});


	/////////////////   find

	struct Nothing {};

	template<typename P>
	constexpr Nothing find(List<>, P) { return{}; }

	template<typename T, typename... Ts, Callable<T> P>
	constexpr auto find(List<T, Ts...>, P p) 
	{
		if constexpr (p(T{})) { return T{}; }
		else                  { return find(List<Ts...>{}, p); }
	}

	//static_assert(find(List<Int_<2>, Int_<5>, Int_<-7>>{}, [](auto v) { return v == Int_<-7>{}; }) == Int_<-7>{});


	/////////////////   drop

	template<auto n, typename T, typename... Ts> requires (n < 4)
	constexpr auto drop(Int_<n>, List<T, Ts...>) 
	{
		if constexpr      (n <= 0) { return List<T, Ts...>{}; }
		else if constexpr (n == 1) { return List<Ts...>{}; }
		else                       { return drop(Int_<n - 1>{}, List<Ts...>{}); }
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
		if constexpr      (n <= 0) { return List<>{}; }
		else if constexpr (n == 1) { return List<T>{}; }
		else                       { return cons<T>(take(Int_<n - 1>{}, List<Ts...>{})); }
	}

	template<auto n, typename T1, typename T2, typename T3, typename T4, typename... Ts> requires (n >= 4)
	constexpr auto take(Int_<n>, List<T1, T2, T3, T4, Ts...>)
	{
		return concat(List<T1, T2, T3, T4>{}, take(Int_<n - 4>{}, List<Ts...>{})); 
	}

	static_assert(take(Int_<2>{}, List<int, int, long, bool>{}) == List<int, int>{});
	static_assert(take(Int_<7>{}, List<int, int, int, bool, int, int, long, bool, double>{}) 
		== List<int, int, int, bool, int, int, long>{});


	/////////////////   sort (function based)

	namespace sort_detail {
		template<typename... Ts, typename C>
		constexpr List<Ts...> merge(List<Ts...>, List<>, C) { return {}; }

		template<typename... Ts, typename C>
		constexpr List<Ts...> merge(List<>, List<Ts...>, C) { return {}; }

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

} //namespace bmath::intern::meta