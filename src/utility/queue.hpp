#pragma once

#include <utility>
#include <type_traits>
#include <memory>
#include <cassert>
#include <algorithm>

namespace simp {

	template<typename Value_T, std::size_t BufferSize>
	class Queue
	{
		static_assert(std::is_trivially_destructible_v<Value_T>);
		static_assert(BufferSize > 0);

		struct Iter
		{
			Value_T* pos;
			Value_T* const start_data;
			Value_T* const end_data;

			//http://www.cplusplus.com/reference/iterator/RandomAccessIterator/
			using value_type = Value_T;
			using difference_type = std::ptrdiff_t;
			using pointer = Value_T*;
			using reference = Value_T&;
			using iterator_category = std::forward_iterator_tag;

			constexpr Iter& operator++() noexcept
			{
				if (++this->pos == this->end_data) [[unlikely]] {
					this->pos = start_data;
				}
				return *this;
			}

			constexpr Iter operator++(int) noexcept { const Iter old = *this; this->operator++(); return old; }

			constexpr Value_T& operator*() noexcept { return *this->pos; }
			constexpr Value_T* operator->() noexcept { return this->pos; }

			constexpr bool operator==(const Iter&) const noexcept = default;
		}; //struct Iter

		Value_T local_data[BufferSize];

		Value_T* start_data;
		Value_T* end_data;

		Value_T* start_used;
		Value_T* end_used;

		std::size_t size_;

		constexpr std::size_t capacity() const noexcept { return this->end_data - this->start_data; }

		constexpr Iter make_iter(Value_T* const pos) noexcept { return { pos, this->start_data, this->end_data }; };

		constexpr Iter begin() noexcept { return this->make_iter(this->start_used); }
		constexpr Iter end()   noexcept { return this->make_iter(this->end_used); }

		constexpr void unsave_change_capacity(const std::size_t new_cap) noexcept
		{
			assert(this->capacity() < new_cap);
			Value_T* const new_data = new Value_T[new_cap];

			assert(this->size_ >= 1);
			*new_data = *this->start_used;
			//due to queues circular nature, this->begin() can start at end, but not be invalid.
			//that case occurs precisely when this->capacity() == this->size_
			std::copy(this->make_iter(this->start_used + 1), this->end(), new_data + 1); 

			if (this->start_data != this->local_data) {
				delete[] this->start_data;
			}

			this->start_data = new_data;
			this->end_data = new_data + new_cap;

			this->start_used = new_data;
			this->end_used = new_data + this->size_;
		} //unsave_change_capacity

	public:
		constexpr std::size_t size() const noexcept { return this->size_; }

		constexpr Queue() noexcept 
			:start_data(&this->local_data[0]),
			end_data   (&this->local_data[0] + BufferSize),
			start_used (&this->local_data[0]),
			end_used   (&this->local_data[0]),
			size_(0)
		{}

		constexpr ~Queue() noexcept 
		{
			if (this->start_data != this->local_data) {
				delete[] this->start_data;
			}
		}

		template<typename... Args>
		constexpr void emplace_back(Args&&... args) noexcept
		{
			if (this->size_ == this->capacity()) [[unlikely]] {
				assert(this->capacity() > 0);
				this->unsave_change_capacity(this->capacity() * 2);
			}
			++this->size_;

			new (this->end_used) Value_T{ std::forward<Args>(args)... };

			if (++this->end_used == this->end_data) [[unlikely]] {
				this->end_used = this->start_data;
			}
		}

		constexpr Value_T pop_front()
		{
			assert(this->size_ > 0);
			--this->size_;

			Value_T* const res_pos = this->start_used;
			if (++this->start_used == this->end_data) [[unlikely]] {
				this->start_used = this->start_data;
			}
			return *res_pos;
		}
	}; //class Queue

} //namespace simp
