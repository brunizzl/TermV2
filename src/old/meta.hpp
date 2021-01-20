#pragma ince

namespace bmath::intern::meta { 

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

	template<Eq T, T V1, T V2>
	constexpr Bool_<(V1 == V2)> operator==(Constant<T, V1>, Constant<T, V2>) { return {}; }

	template<Eq T, T V1, T V2>
	constexpr Bool_<(V1 != V2)> operator!=(Constant<T, V1>, Constant<T, V2>) { return {}; }


	static_assert(Int_<4>{} + Int_<6>{} == Int_<10>{});
	static_assert(Int_<4>{} + Int_<8>{} >= Int_<10>{});
    
    
    

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////   Operations on Lists   ////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


	template<typename... Ts>
	struct List 
	{
		consteval std::bool_constant<sizeof...(Ts) == 0> is_empty() { return {}; }
		consteval Int_<sizeof...(Ts)> size() { return {}; }
	};

	template<InstanceOf<List> L1, InstanceOf<List> L2>
	constexpr std::bool_constant<std::is_same_v<L1, L2>> operator==(L1, L2) { return {}; }

	template<InstanceOf<List> L1, InstanceOf<List> L2>
	constexpr std::bool_constant<!std::is_same_v<L1, L2>> operator!=(L1, L2) { return {}; }
    

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

	template<typename T, typename T1>
	constexpr Bool_<std::is_same_v<T, T1>> 
		contains(List<T1>) { return {}; }

	template<typename T, typename T1, typename T2>
	constexpr Bool_<std::is_same_v<T, T1> || std::is_same_v<T, T2>> 
		contains(List<T1, T2>) { return {}; }

	template<typename T, typename T1, typename T2, typename T3>
	constexpr Bool_<std::is_same_v<T, T1> || std::is_same_v<T, T2> || std::is_same_v<T, T3>> 
		contains(List<T1, T2, T3>) { return {}; }

	template<typename T, typename T1, typename T2, typename T3, typename T4>
	constexpr Bool_<std::is_same_v<T, T1> || std::is_same_v<T, T2> || std::is_same_v<T, T3> || std::is_same_v<T, T4>>
		contains(List<T1, T2, T3, T4>) { return {}; }

	template<typename T, typename T1, typename T2, typename T3, typename T4, typename... Ts>
	constexpr auto contains(List<T1, T2, T3, T4, Ts...>)
	{
		return Bool_<std::is_same_v<T, T1> || std::is_same_v<T, T2> || std::is_same_v<T, T3> || std::is_same_v<T, T4>>{} || 
			contains<T>(List<Ts...>{});
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

	static_assert(map([](auto x) { return x + Int_<4>{}; }, List<Int_<2>, Int_<5>, Int_<-7>>{}) 
		== List<Int_<6>, Int_<9>, Int_<-3>>{});


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

	static_assert(find(List<Int_<2>, Int_<5>, Int_<-7>>{}, [](auto v) { return v == Int_<-7>{}; }) == Int_<-7>{});


	/////////////////   find_index
	namespace detail_find {
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

	} //namespace detail_find

	template<InstanceOf<List> L, typename P>
	constexpr auto find_index(L l, P p) { return detail_find::loop(l, Int_<0>{}, p); }

	static_assert(find_index(List<Int_<2>, Int_<5>, Int_<-7>>{}, [](auto v) { return (bool)(v == Int_<-7>{}); }) == Int_<2>{});


	/////////////////   sort (function based)

	namespace detail_sort {
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
	} //namespace detail_sort

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
		return detail_sort::merge(lhs, rhs, c);
	}

	static_assert(sort(List<Int_<3>, Int_<2>, Int_<5>, Int_<10>, Int_<-7>>{}, [](auto l, auto r) { return l < r; })
		== List<Int_<-7>, Int_<2>, Int_<3>, Int_<5>, Int_<10>>{});


	/////////////////   reverse

	namespace detail_reverse {
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
	} //namespace detail_reverse

	template<InstanceOf<List> L>
	constexpr auto reverse(L l) { return detail_reverse::loop(List<>{}, l); }

	static_assert(reverse(List<int, bool, char, float, double>{}) == List<double, float, char, bool, int>{});


	/////////////////   index_of

	namespace detail_index {
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
	} //namespace detail_index

	template<typename Needle, InstanceOf<List> L>
	constexpr auto index_of(L l) { return detail_index::loop<Needle>(l, Int_<0>{}); }

	static_assert(index_of<Int_<-7>>(List<Int_<2>, Int_<5>, Int_<-7>>{}) == Int_<2>{});

} //namespace bmath::intern::meta
