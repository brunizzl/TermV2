#pragma once

#include <exception>
#include <algorithm>
#include <cassert>
#include <array>
#include <vector>

namespace bmath::intern {

	constexpr void throw_if(bool cond, const char* const msg)
	{
		if (cond) [[unlikely]] {
			throw std::exception(msg);
		}
	}

	template<typename Exception_T = std::exception, typename... Args>
	constexpr void throw_if(bool cond, Args&&... args)
	{
		if (cond) [[unlikely]] {
			throw Exception_T{ std::forward<Args>(args)... };
		}
	}

	//stolen from Jason Turner: https://www.youtube.com/watch?v=INn3xa4pMfg
	//(but also modified slightly)
	template <typename Key, typename Value, std::size_t Size>
	[[nodiscard]] constexpr Value find(
		const std::array<std::pair<Key, Value>, Size>& data, const Key key) noexcept
	{
		const auto itr = std::find_if(begin(data), end(data),
			[&key](const auto &v) { return v.first == key; });
		if (itr == end(data)) {
			assert(false);
		}
		return itr->second;
	}

	template <typename Key, typename Value, std::size_t Size>
	[[nodiscard]] constexpr Value search(
		const std::array<std::pair<Key, Value>, Size>& data, 
		const Key key, const Value null_value) noexcept 
	{
		const auto itr = std::find_if(begin(data), end(data),
			[&key](const auto &v) { return v.first == key; });
		if (itr != end(data)) {
			return itr->second;
		} else {
			return null_value;
		}
	}

	template<typename Val>
	std::ostream& operator<<(std::ostream& str, const std::vector<Val>& vec)
	{
		str << "{ ";
		bool first = true;
		for (const auto& elem : vec) {
			if (!std::exchange(first, false)) {
				str << ", ";
			}
			str << elem;
		}
		str << '}';
	}

	template<typename Value_T, std::size_t BufferSize>
	class StupidBufferVec
	{
		static_assert(BufferSize > 0);
		static_assert(std::is_trivially_copyable_v<Value_T>);     //big part of the "Stupid" in the name
		static_assert(std::is_trivially_destructible_v<Value_T>); //big part of the "Stupid" in the name
		static_assert(std::is_default_constructible_v<Value_T>);  //big part of the "Stupid" in the name

		std::size_t size_;
		Value_T* data_;
		union
		{
			std::size_t capacity;
			Value_T local_data[BufferSize];
		};

	public:
		constexpr std::size_t size() const noexcept { return this->size_; }
		constexpr const Value_T* data() const noexcept { return this->data_; }

		constexpr StupidBufferVec() :size_(0), data_(local_data) {}

		~StupidBufferVec()
		{
			if (this->size_ > BufferSize) [[unlikely]] {
				delete[] this->data_;
			}
		}

		StupidBufferVec(const StupidBufferVec&) = delete;
		StupidBufferVec(StupidBufferVec&&) = delete;
		StupidBufferVec& operator=(const StupidBufferVec&) = delete;
		StupidBufferVec& operator=(StupidBufferVec&&) = delete;

		constexpr void push_back(Value_T elem)
		{
			if (this->size_ == BufferSize) [[unlikely]] {
				Value_T * new_data = new Value_T[BufferSize * 2];
				std::copy(this->data_, this->data_ + BufferSize, new_data);
				this->data_ = new_data;
				this->capacity = BufferSize * 2;
			}
			else if (this->size_ > BufferSize && this->size_ == this->capacity) [[unlikely]] {
				Value_T * new_data = new Value_T[this->capacity * 2];
				std::copy(this->data_, this->data_ + this->capacity, new_data);
				delete[] this->data_;
				this->data_ = new_data;
				this->capacity *= 2;
			}
			this->data_[this->size_++] = elem;
		}

		constexpr const Value_T& operator[](std::size_t where) const noexcept { return this->data_[where]; }
		constexpr Value_T& operator[](std::size_t where) noexcept { return this->data_[where]; }

		constexpr const Value_T* begin() const noexcept { return this->data__; }
		constexpr Value_T* begin() noexcept { return this->data_; }
		constexpr const Value_T* end() const noexcept { return this->data_ + this->size_; }
		constexpr Value_T* end() noexcept { return this->data_ + this->size_; }

	};

} //namespace bmath::intern