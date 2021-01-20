#pragma once

#include <cstddef>
#include <algorithm>
#include <type_traits>
#include <array>

#include "meta.hpp"

namespace bmath::intern {


    template<typename T, std::size_t N>
	struct Array
	{
		T data_[N];

		using value_type = T;

		constexpr Array() noexcept :data_{} {}

		template<typename... Ts>
		constexpr Array(T t, Ts... ts) noexcept :data_{ t, ts... } {}

		constexpr std::size_t size() const noexcept { return N; }

		constexpr       T& operator[](std::size_t i)       noexcept { return this->data_[i]; }
		constexpr const T& operator[](std::size_t i) const noexcept { return this->data_[i]; }

		constexpr       T& front()       noexcept { return this->data_[0]; }
		constexpr const T& front() const noexcept { return this->data_[0]; }

		constexpr       T& back()       noexcept { return this->data_[N - 1]; }
		constexpr const T& back() const noexcept { return this->data_[N - 1]; }

		constexpr       T* data()       noexcept { return this->data_; }
		constexpr const T* data() const noexcept { return this->data_; }

		constexpr       T* begin()       noexcept { return this->data_; }
		constexpr const T* begin() const noexcept { return this->data_; }

		constexpr       T* end()       noexcept { return this->data_ + N; }
		constexpr const T* end() const noexcept { return this->data_ + N; }

		constexpr std::strong_ordering operator<=>(const Array&) const noexcept = default;
	}; //struct Array
	
	template <class T, class... U>
	Array(T, U...) -> Array<T, 1 + sizeof...(U)>;


	template<typename T>
	struct Array<T, 0>
	{
	private:
		static constexpr T* ptr() noexcept { return nullptr; }

	public:
		using value_type = T;

		constexpr Array() noexcept = default;

		constexpr std::size_t size() const noexcept { return 0; }

		constexpr       T& operator[](std::size_t)       noexcept { return *Array::ptr(); }
		constexpr const T& operator[](std::size_t) const noexcept { return *Array::ptr(); }

		constexpr       T& front()       noexcept { return *Array::ptr(); }
		constexpr const T& front() const noexcept { return *Array::ptr(); }

		constexpr       T& back()       noexcept { return *Array::ptr(); }
		constexpr const T& back() const noexcept { return *Array::ptr(); }

		constexpr       T* data()       noexcept { return nullptr; }
		constexpr const T* data() const noexcept { return nullptr; }

		constexpr       T* begin()       noexcept { return nullptr; }
		constexpr const T* begin() const noexcept { return nullptr; }

		constexpr       T* end()       noexcept { return nullptr; }
		constexpr const T* end() const noexcept { return nullptr; }

		constexpr std::strong_ordering operator<=>(const Array&) const noexcept = default;
	}; //class Array<T, 0>



	namespace arr {
		template<typename T, typename ArrT>
		constexpr bool holds_v = std::is_same_v<T, typename ArrT::value_type>;

		/////////////////   concat
		namespace detail_concat {
			template <typename T, std::size_t N1, std::size_t N2, std::size_t... I1, std::size_t... I2>
			constexpr Array<T, N1 + N2> impl(
				const Array<T, N1>& arr_1, const Array<T, N2>& arr_2, std::index_sequence<I1...>, std::index_sequence<I2...>)
			{
				return { arr_1[I1]..., arr_2[I2]... };
			}
		} //namespace detail_concat

		template<typename T, std::size_t N1, std::size_t N2>
		constexpr Array<T, N1 + N2> concat(const Array<T, N1>& arr_1, const Array<T, N2>& arr_2)
		{
			return detail_concat::impl(arr_1, arr_2, std::make_index_sequence<N1>{}, std::make_index_sequence<N2>{});
		}

		//static_assert(concat(Array{ 1, 2, 3 }, Array{ 4, 5, 6 }) == Array{ 1, 2, 3, 4, 5, 6 });


		/////////////////   cons
		namespace detail_cons {
			template <typename T, std::size_t N, std::size_t... I>
			constexpr Array<T, N + 1> impl(const T& val, const Array<T, N>& arr_, std::index_sequence<I...>)
			{
				return { val, arr_[I]... };
			}
		} //namespace detail_cons

		template<typename T, std::size_t N>
		constexpr Array<T, N + 1> cons(const T& val, const Array<T, N>& arr_)
		{
			return detail_cons::impl(val, arr_, std::make_index_sequence<N>{});
		}

		//static_assert(cons(1, Array{ 2, 3, 4 }) == Array{ 1, 2, 3, 4 });


		/////////////////   subrange / take / drop
		namespace detail_subrange {
			template <std::size_t N_out, typename T, std::size_t... I>
			constexpr Array<T, N_out> impl(const T* arr_, std::index_sequence<I...>)
			{
				return { arr_[I]... };
			}
		} //namespace detail_subrange

		template<std::size_t Start, std::size_t Length, typename T, std::size_t N>
		constexpr Array<T, Length> subrange(const Array<T, N>& arr_)
		{
			static_assert(N >= Start + Length);
			return detail_subrange::impl<Length>(arr_.data() + Start, std::make_index_sequence<Length>{});
		}

		//static_assert(subrange<3, 4>(Array{ 1, 2, 3, 4, 5, 6, 7 ,8 }) == Array{ 4, 5, 6, 7 });


		template<std::size_t Delta, typename T, std::size_t N>
		constexpr Array<T, Delta> take(const Array<T, N>& arr_)
		{
			static_assert(N >= Delta);
			return detail_subrange::impl<Delta>(arr_.data(), std::make_index_sequence<Delta>{});
		}

		//static_assert(take<3>(Array{ 1, 2, 3, 4, 5, 6, 7 ,8 }) == Array{ 1, 2, 3 });


		template<std::size_t Delta, typename T, std::size_t N>
		constexpr Array<T, N - Delta> drop(const Array<T, N>& arr_)
		{
			static_assert(N >= Delta);
			return detail_subrange::impl<N - Delta>(arr_.data() + Delta, std::make_index_sequence<N - Delta>{});
		}

		//static_assert(drop<3>(Array{ 1, 2, 3, 4, 5, 6, 7 ,8 }) == Array{ 4, 5, 6, 7, 8 });



		/////////////////   from_list
		namespace detail_from_list {
			template<typename T1, Callable<T1> F>
			constexpr auto loop(meta::List<T1>, F f) { return Array{ f(T1{}) }; }

			template<typename T1, typename T2, Callable<T1> F>
			constexpr auto loop(meta::List<T1, T2>, F f) { return Array{ f(T1{}), f(T2{}) }; }

			template<typename T1, typename T2, typename T3, Callable<T1> F>
			constexpr auto loop(meta::List<T1, T2, T3>, F f) { return Array{ f(T1{}), f(T2{}), f(T3{}) }; }

			template<typename T1, typename T2, typename T3, typename T4, Callable<T1> F>
			constexpr auto loop(meta::List<T1, T2, T3, T4>, F f) { return Array{ f(T1{}), f(T2{}), f(T3{}), f(T4{}) }; }

			template<typename T1, typename T2, typename T3, typename T4, typename... Ts, Callable<T1> F>
				requires (sizeof...(Ts) > 0)
			constexpr auto loop(meta::List<T1, T2, T3, T4, Ts...>, F f)
			{
				return arr::concat(Array{ f(T1{}), f(T2{}), f(T3{}), f(T4{}) }, loop(meta::List<Ts...>{}, f));
			}
		} //namespace detail_from_list

		template<typename T, typename... Ts, Callable<T> F>
		constexpr auto from_list(F f, meta::List<T, Ts...> l) { return detail_from_list::loop<T>(l, f); }

		//static_assert(from_list([](auto x) { return x.val(); }, meta::List<meta::Int_<1>, meta::Int_<2>, meta::Int_<3>>{})
		//	== std::to_array({ 1ll, 2ll, 3ll }));


		/////////////////   index_of

		template<typename T, std::size_t N>
		constexpr long long index_of(const T& t, const Array<T, N>& arr)
		{
			const auto iter = std::find(arr.begin(), arr.end(), t);
			if (iter != arr.end()) {
				return std::distance(arr.begin(), iter);
			}
			return -1ll;
		}

		//static_assert(index_of(4, Array{ 1, 2, 4, 5, 6, 7 }) == 2);
		//static_assert(index_of(8, Array{ 1, 2, 4, 5, 6, 7 }) == -1);


		/////////////////   map

		namespace detail_map {
			template<class T, std::size_t N, Callable<T> F, std::size_t... I>
			constexpr auto impl(F f, const Array<T, N>& input, std::index_sequence<I...>)
			{ 
				using ResT = decltype(f(std::declval<T>()));
				return Array<ResT, N>{ f(input[I])... };
			}
		} //detail_map

		template<typename T, std::size_t N, Callable<T> F>
		constexpr auto map(F f, const Array<T, N>& input)
		{
			return detail_map::impl(f, input, std::make_index_sequence<N>{});
		}

		//static_assert(arr::map([](auto x) { return x + 3; }, Array{ 1, 2, 3 }) == Array{ 4, 5, 6 });


		//taken and adapted from https://en.cppreference.com/w/cpp/container/array/to_array
		namespace detail_make {
			template<class T, std::size_t N, std::size_t... I>
			constexpr Array<T, N> impl(const T(&a)[N], std::index_sequence<I...>) { return { a[I]... }; }
		} //detail_make

		template <class T, std::size_t N>
		constexpr Array<T, N> make(const T(&a)[N])
		{
			return detail_make::impl(a, std::make_index_sequence<N>{});
		}

		static_assert(make<int>({ 1 }) == Array{ 1 });

	} //namespace arr

} //namespace bmath::intern