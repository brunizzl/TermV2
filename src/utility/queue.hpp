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

		Value_T local_data[BufferSize];

		Value_T* start_data;
		Value_T* end_data;

		Value_T* start_used;
		Value_T* end_used;

		std::size_t size_;

		constexpr std::size_t capacity() const noexcept { return this->end_data - this->start_data; }

		constexpr void incr(Value_T*& pos) const noexcept { if (++pos == this->end_data) [[unlikely]] { pos = this->start_data; } }

		constexpr void unsave_change_capacity(const std::size_t new_cap) noexcept
		{
			assert(this->capacity() < new_cap);
			Value_T* const new_data = new Value_T[new_cap];

			assert(this->size_ >= 1);
			//due to queues circular nature, from can start at this->end_used, but not be invalid.
			//that case occurs precisely when this->capacity() == this->size_
			Value_T* from = this->start_used;
			Value_T* to = new_data;
			do {
				*to = *from;
				this->incr(from);
			} while (from != this->end_used);

			if (this->start_data != this->local_data) {
				delete[] this->start_data;
			}

			this->start_data = new_data;
			this->end_data = new_data + new_cap;

			this->start_used = new_data;
			this->end_used = to;
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

			this->incr(this->end_used);
		}

		constexpr Value_T pop_front()
		{
			assert(this->size_ > 0);
			--this->size_;

			Value_T* const res_pos = this->start_used;
			this->incr(this->start_used);
			return *res_pos;
		}
	}; //class Queue

} //namespace simp
