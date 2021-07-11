#pragma once

#include <utility>
#include <concepts>
#include <iterator>
#include <algorithm>
#include <array>

#include "meta.hpp"
#include "vector.hpp"

namespace simp {

	namespace detail_sort {
		template<typename T> //half open range [start, stop)
		struct Range 
		{ 
			T* start; T* stop;
			constexpr bool empty() const noexcept { return this->start == this->stop; }
		};

		//assumes fst and snd to be sorted, returns to + fst.length() + snd.length()
		template<typename T, std::strict_weak_order<T, T> Smaller>
		constexpr T* merge(T* to, Range<T> fst, Range<T> snd, Smaller smaller)
		{
			while (!fst.empty() && !snd.empty()) {
				*to++ = smaller(*snd.start, *fst.start) ?
					std::move(*snd.start++) :
					std::move(*fst.start++);
			}
			while (!fst.empty()) { *to++ = std::move(*fst.start++); }
			while (!snd.empty()) { *to++ = std::move(*snd.start++); }
			return to;
		}

		//unsorted elements are found in range [from, from + length), and sorted into range [to, to + length)
		//returns pointer at to + length
		template<typename T, std::strict_weak_order<T, T> Smaller>
		constexpr T* stable_sort_move(T* to, T* from, const std::size_t length, Smaller smaller);

		//buffer is assumed to point at array with at least (length / 2) many elements
		//sorts elements in range [to_sort, to_sort + length) using buffer as buffer
		//returns pointer at to_sort + length
		template<typename T, std::strict_weak_order<T, T> Smaller>
		constexpr T* stable_sort_in_place(T* const buffer, T* const to_sort, const std::size_t length, Smaller smaller)
		{
			assert(buffer != to_sort);
			if (length < 2) {
				return to_sort + length;
			}
			const std::size_t fst_length = length / 2;
			const std::size_t snd_length = length - fst_length;
			T* const fst_start = buffer;
			T* const snd_start = to_sort + fst_length;
			T* const snd_end = stable_sort_in_place(buffer, snd_start, snd_length, smaller);
			T* const fst_end = stable_sort_move(buffer, to_sort, fst_length, smaller);
			return merge(to_sort, { fst_start, fst_end }, { snd_start, snd_end }, smaller);
		}

		template<typename T, std::strict_weak_order<T, T> Smaller>
		constexpr T* stable_sort_move(T* const to, T* const from, const std::size_t length, Smaller smaller)
		{
			assert(from != to);
			if (length < 2) {
				*to = std::move(*from);
				return to + 1;
			}
			const std::size_t fst_length = length / 2;
			const std::size_t snd_length = length - fst_length;
			T* const fst_start = from;
			T* const snd_start = from + fst_length;
			T* const fst_end = stable_sort_in_place(to, fst_start, fst_length, smaller);
			T* const snd_end = stable_sort_in_place(to, snd_start, snd_length, smaller);
			return merge(to, { fst_start, fst_end }, { snd_start, snd_end }, smaller);
		}		
	} //namespace detail_sort

	template<typename T, std::size_t N, std::strict_weak_order<T, T> Smaller>
	constexpr void stable_sort(std::array<T, N>& arr, Smaller smaller)
	{
		std::array<T, N / 2> buffer;
		detail_sort::stable_sort_in_place(buffer.data(), arr.data(), N, smaller);
	}

	template<typename T, std::strict_weak_order<T, T> Smaller>
	constexpr void stable_sort(T* const start, T* const stop, Smaller smaller)
	{
		StupidBufferVector<T, 64> buffer;
		buffer.expand_to_size((stop - start) / 2);
		detail_sort::stable_sort_in_place(buffer.data(), start, stop - start, smaller);
	}


} //namespace simp