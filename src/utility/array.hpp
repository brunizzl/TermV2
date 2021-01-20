#pragma once

#include <cstddef>
#include <algorithm>
#include <type_traits>
#include <array>

#include "meta.hpp"

namespace bmath::intern::arr {
	
	template<typename T, typename ArrT>
	constexpr bool holds_v = std::is_same_v<T, typename ArrT::value_type>;

	/////////////////   concat
	namespace detail_concat {
		template <typename T, std::size_t N1, std::size_t N2, std::size_t... I1, std::size_t... I2>
		constexpr std::array<T, N1 + N2> impl(
			const std::array<T, N1>& arr_1, const std::array<T, N2>& arr_2, std::index_sequence<I1...>, std::index_sequence<I2...>)
		{
			return { arr_1[I1]..., arr_2[I2]... };
		}
	} //namespace detail_concat

	template<typename T, std::size_t N1, std::size_t N2>
	constexpr std::array<T, N1 + N2> concat(const std::array<T, N1>& arr_1, const std::array<T, N2>& arr_2)
	{
		return detail_concat::impl(arr_1, arr_2, std::make_index_sequence<N1>{}, std::make_index_sequence<N2>{});
	}

	//static_assert(concat(std::array{ 1, 2, 3 }, std::array{ 4, 5, 6 }) == std::array{ 1, 2, 3, 4, 5, 6 });


	/////////////////   cons
	namespace detail_cons {
		template <typename T, std::size_t N, std::size_t... I>
		constexpr std::array<T, N + 1> impl(const T& val, const std::array<T, N>& arr_, std::index_sequence<I...>)
		{
			return { val, arr_[I]... };
		}
	} //namespace detail_cons

	template<typename T, std::size_t N>
	constexpr std::array<T, N + 1> cons(const T& val, const std::array<T, N>& arr_)
	{
		return detail_cons::impl(val, arr_, std::make_index_sequence<N>{});
	}

	//static_assert(cons(1, std::array{ 2, 3, 4 }) == std::array{ 1, 2, 3, 4 });


	/////////////////   subrange / take / drop
	namespace detail_subrange {
		template <std::size_t N_out, typename T, std::size_t... I>
		constexpr std::array<T, N_out> impl(const T* arr_, std::index_sequence<I...>)
		{
			return { arr_[I]... };
		}
	} //namespace detail_subrange

	template<std::size_t Start, std::size_t Length, typename T, std::size_t N>
	constexpr std::array<T, Length> subrange(const std::array<T, N>& arr_)
	{
		static_assert(N >= Start + Length);
		return detail_subrange::impl<Length>(arr_.data() + Start, std::make_index_sequence<Length>{});
	}

	//static_assert(subrange<3, 4>(std::array{ 1, 2, 3, 4, 5, 6, 7 ,8 }) == std::array{ 4, 5, 6, 7 });


	template<std::size_t Delta, typename T, std::size_t N>
	constexpr std::array<T, Delta> take(const std::array<T, N>& arr_)
	{
		static_assert(N >= Delta);
		return detail_subrange::impl<Delta>(arr_.data(), std::make_index_sequence<Delta>{});
	}

	//static_assert(take<3>(std::array{ 1, 2, 3, 4, 5, 6, 7 ,8 }) == std::array{ 1, 2, 3 });


	template<std::size_t Delta, typename T, std::size_t N>
	constexpr std::array<T, N - Delta> drop(const std::array<T, N>& arr_)
	{
		static_assert(N >= Delta);
		return detail_subrange::impl<N - Delta>(arr_.data() + Delta, std::make_index_sequence<N - Delta>{});
	}

	//static_assert(drop<3>(std::array{ 1, 2, 3, 4, 5, 6, 7 ,8 }) == std::array{ 4, 5, 6, 7, 8 });



	/////////////////   from_list
	namespace detail_from_list {
		template<typename T1, Callable<T1> F>
		constexpr auto loop(meta::List<T1>, F f) { return std::array{ f(T1{}) }; }

		template<typename T1, typename T2, Callable<T1> F>
		constexpr auto loop(meta::List<T1, T2>, F f) { return std::array{ f(T1{}), f(T2{}) }; }

		template<typename T1, typename T2, typename T3, Callable<T1> F>
		constexpr auto loop(meta::List<T1, T2, T3>, F f) { return std::array{ f(T1{}), f(T2{}), f(T3{}) }; }

		template<typename T1, typename T2, typename T3, typename T4, Callable<T1> F>
		constexpr auto loop(meta::List<T1, T2, T3, T4>, F f) { return std::array{ f(T1{}), f(T2{}), f(T3{}), f(T4{}) }; }

		template<typename T1, typename T2, typename T3, typename T4, typename... Ts, Callable<T1> F>
			requires (sizeof...(Ts) > 0)
		constexpr auto loop(meta::List<T1, T2, T3, T4, Ts...>, F f)
		{
			return arr::concat(std::array{ f(T1{}), f(T2{}), f(T3{}), f(T4{}) }, loop(meta::List<Ts...>{}, f));
		}
	} //namespace detail_from_list

	template<typename T, typename... Ts, Callable<T> F>
	constexpr auto from_list(F f, meta::List<T, Ts...> l) { return detail_from_list::loop<T>(l, f); }

	//static_assert(from_list([](auto x) { return x.val(); }, meta::List<meta::Int_<1>, meta::Int_<2>, meta::Int_<3>>{})
	//	== std::to_array({ 1ll, 2ll, 3ll }));


	/////////////////   index_of

	template<typename T, std::size_t N>
	constexpr long long index_of(const T& t, const std::array<T, N>& arr)
	{
		const auto iter = std::find(arr.begin(), arr.end(), t);
		if (iter != arr.end()) {
			return std::distance(arr.begin(), iter);
		}
		return -1ll;
	}

	//static_assert(index_of(4, std::array{ 1, 2, 4, 5, 6, 7 }) == 2);
	//static_assert(index_of(8, std::array{ 1, 2, 4, 5, 6, 7 }) == -1);


	/////////////////   map

	namespace detail_map {
		template<class T, std::size_t N, Callable<T> F, std::size_t... I>
		constexpr auto impl(F f, const std::array<T, N>& input, std::index_sequence<I...>)
		{ 
			using ResT = decltype(f(std::declval<T>()));
			return std::array<ResT, N>{ f(input[I])... };
		}
	} //detail_map

	template<typename T, std::size_t N, Callable<T> F>
	constexpr auto map(F f, const std::array<T, N>& input)
	{
		return detail_map::impl(f, input, std::make_index_sequence<N>{});
	}

	//static_assert(arr::map([](auto x) { return x + 3; }, std::array{ 1, 2, 3 }) == std::array{ 4, 5, 6 });

} //namespace bmath::intern::arr