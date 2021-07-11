#pragma once

#include <span>
#include <compare>

#include "meta.hpp"

namespace simp {
    
    

	template<typename Value_T, std::size_t BufferSize>
	class [[nodiscard]] StupidBufferVector
	{
		static_assert(BufferSize > 0u);
		static_assert(std::is_trivially_copyable_v<Value_T>);     //thus the "Stupid" in the name
		static_assert(std::is_trivially_destructible_v<Value_T>); //thus the "Stupid" in the name

		std::size_t size_ = 0u;
		Value_T* data_ = &u.local_data[0];
		union U
		{
			std::size_t capacity;
			Value_T local_data[BufferSize];

			constexpr U() :capacity(0u) {}
		} u = {};

		template<typename SndValue_T, std::size_t SndBuffer>
		friend class StupidBufferVector;

		constexpr void unsave_reallocate(const std::size_t new_capacity) noexcept
		{
			assert(new_capacity > BufferSize && (this->data_ == this->u.local_data || new_capacity > this->u.capacity) && 
				"no reallocation neccesairy");

			Value_T* const new_data = new Value_T[new_capacity];
			std::copy_n(this->data_, this->size_, new_data);
			if (this->data_ != this->u.local_data) {
				delete[] this->data_; //only works for trivially destructible Value_T
			}
			this->data_ = new_data;
			this->u.capacity = new_capacity;
		}

	public:
		using value_type = Value_T;

		constexpr std::size_t size() const noexcept { return this->size_; }
		constexpr const Value_T* data() const noexcept { return this->data_; }
		constexpr Value_T* data() noexcept { return this->data_; }

		constexpr StupidBufferVector() noexcept :size_(0u) {}

		constexpr StupidBufferVector(const std::initializer_list<Value_T> init) noexcept :size_(init.size())
		{
			if (init.size() > BufferSize) [[unlikely]] {
				this->data_ = new Value_T[init.size()];
				this->u.capacity = init.size();
			}
			std::copy(init.begin(), init.end(), this->data_);
		}

		constexpr ~StupidBufferVector() noexcept
		{
			if (this->data_ != this->u.local_data) {
				delete[] this->data_; //only works for trivially destructible Value_T
			}
		}

		constexpr void reserve(const std::size_t new_capacity) noexcept
		{
			if (new_capacity > BufferSize && 
				(this->data_ == this->u.local_data || new_capacity > this->u.capacity)) 
			{
				this->unsave_reallocate(new_capacity);
			}
		}

		StupidBufferVector(const StupidBufferVector&) = delete;

		template<std::size_t SndBuffer>
		constexpr StupidBufferVector& operator=(const StupidBufferVector<Value_T, SndBuffer>& snd) noexcept
		{
			assert(false && "please be aware this function is not yet tested");
			this->clear();
			this->reserve(snd.size_);
			std::copy(snd.data_, snd.data_ + snd.size_, this->data_);
		}

		template<std::size_t SndBuffer>
		constexpr StupidBufferVector& operator=(StupidBufferVector<Value_T, SndBuffer>&& snd) noexcept
		{
			assert(false && "please be aware this function is not yet tested");
			if (snd.data_ != snd.u.local_data) { //only works for trivially destructible Value_T
				if (this->data_ != this->u.local_data) {
					delete[] this->data_;
				}
				this->data_ = std::exchange(snd.data_, nullptr);
				this->u.capacity = std::exchange(snd.u.capacity, 0u);
				this->size_ = std::exchange(snd.size_, 0u);
			}
			else {
				this->reserve(snd.size_);
				this->size_ = snd.size_;
				std::copy_n(snd.data_, snd.size_, this->data_);
				snd.clear();
			} 
			return *this;
		}

		constexpr StupidBufferVector(StupidBufferVector&& snd) noexcept :size_(std::exchange(snd.size_, 0u))
		{
			if (snd.data_ != snd.u.local_data) {
				this->data_ = std::exchange(snd.data_, nullptr);
				this->u.capacity = std::exchange(snd.u.capacity, 0u);
			}
			else {
				std::move(snd.data_, snd.data_ + this->size_, this->data_);
			}
		}

		template<typename... Args>
		constexpr Value_T& emplace_back(Args&&... args) noexcept
		{
			if (this->size_ > BufferSize) {
				if (this->size_ == this->u.capacity) [[unlikely]] {
					this->unsave_reallocate(this->u.capacity * 2u);
				}
			}
			else if (this->size_ == BufferSize) [[unlikely]] {
				this->unsave_reallocate(BufferSize * 2u);
			}

			Value_T* const addr = &this->data_[this->size_++];
			new (addr) Value_T{ args... };
			return  *addr;
		}

		constexpr Value_T& push_back(const Value_T& elem) noexcept { return this->emplace_back(elem); }

		constexpr Value_T pop_back() noexcept
		{ 
			assert(this->size_ > 0u && "tried popping on empty vector");
			return this->data_[--this->size_]; //only works for trivially destructible Value_T
		}

		constexpr void shorten_to(Value_T* const new_end) noexcept
		{
			const std::size_t new_size = new_end - this->data_;
			assert(new_size <= this->size_);
			this->size_ = new_size; //only works for trivially destructible Value_T
		}

		//may be used with caution, as this does no initialisation
		constexpr void expand_to_size(const std::size_t new_size) noexcept
		{
			assert(this->size_ <= new_size);
			this->reserve(new_size);
			this->size_ = new_size;
		}

		constexpr void clear() noexcept { this->size_ = 0u; } //only works for trivially destructible Value_T

		constexpr const Value_T& operator[](std::size_t where) const noexcept { return this->data_[where]; }
		constexpr Value_T& operator[](std::size_t where) noexcept { return this->data_[where]; }

		constexpr const Value_T& front() const noexcept { return this->data_[0]; }
		constexpr const Value_T& back() const noexcept { return this->data_[this->size_ - 1u]; }
		constexpr Value_T& front() noexcept { return this->data_[0]; }
		constexpr Value_T& back() noexcept { return this->data_[this->size_ - 1u]; }

		constexpr const Value_T* begin() const noexcept { return this->data_; }
		constexpr const Value_T* end() const noexcept { return this->data_ + this->size_; }
		constexpr Value_T* begin() noexcept { return this->data_; }
		constexpr Value_T* end() noexcept { return this->data_ + this->size_; }

		constexpr std::span<const Value_T> range() const noexcept { return { this->data_, this->size_ }; }
		constexpr std::span<Value_T> range() noexcept { return { this->data_, this->size_ }; }

	}; //class StupidBufferVector



	template<typename Value_T, std::size_t MaxSize>
	class [[nodiscard]] ShortVector
	{
		std::size_t size_;
		Value_T data_[MaxSize];

	public:
		using value_type = Value_T;

		constexpr std::size_t size() const noexcept { return this->size_; }
		constexpr const Value_T* data() const noexcept { return this->data_; }
		constexpr Value_T* data() noexcept { return this->data_; }

		constexpr ShortVector() noexcept :size_(0) {}

		constexpr ShortVector(const std::initializer_list<Value_T> init) noexcept :size_(init.size())
		{
			assert(init.size() <= MaxSize && "initializer length exeeds MaxSize");
			std::copy(init.begin(), init.end(), this->data_);
		}

		ShortVector(const ShortVector&) = delete;
		ShortVector& operator=(const ShortVector&) = delete;
		ShortVector& operator=(ShortVector&&) = delete;
		ShortVector(ShortVector&& snd) = delete;

		template<typename... Args>
		constexpr Value_T& emplace_back(Args&&... args) noexcept
		{
			assert(this->size_ < MaxSize && "tried pushing on full vector");
			Value_T* const addr = &this->data_[this->size_++];
			new (addr) Value_T{ args... };
			return *addr;
		}

		constexpr Value_T& push_back(const Value_T& elem) noexcept { return this->emplace_back(elem); }

		constexpr void pop_back() noexcept
		{ 
			assert(this->size_ > 0u && "tried popping on empty vector");
			this->data_[--this->size_].~Value_T();
		}

		constexpr void clear() noexcept { 
			if constexpr (std::is_trivially_destructible_v<Value_T>) { 
				this->size_ = 0u; 
			}
			else { 
				while (this->size_) { this->pop_back(); } 
			}
		}

		constexpr const Value_T& operator[](const std::size_t where) const noexcept { return this->data_[where]; }
		constexpr Value_T& operator[](const std::size_t where) noexcept { return this->data_[where]; }

		constexpr const Value_T& front() const noexcept {  return this->data_[0]; }
		constexpr const Value_T& back() const noexcept {  return this->data_[this->size_ - 1u]; }
		constexpr Value_T& front() noexcept { return this->data_[0]; }
		constexpr Value_T& back() noexcept { return this->data_[this->size_ - 1u]; }

		constexpr const Value_T* begin() const noexcept { return this->data_; }
		constexpr const Value_T* end() const noexcept { return this->data_ + this->size_; }
		constexpr Value_T* begin() noexcept { return this->data_; }
		constexpr Value_T* end() noexcept { return this->data_ + this->size_; }

		constexpr std::span<const Value_T> range() const noexcept { return { this->data_, this->size_ }; }
		constexpr std::span<Value_T> range() noexcept { return { this->data_, this->size_ }; }

		constexpr friend auto operator<=>(const ShortVector&, const ShortVector&) = default;
		constexpr friend bool operator==(const ShortVector&, const ShortVector&) = default;

	}; //class ShortVector

	template<typename V, typename T>
	concept VectorOf = ContainerOf<V, T> && requires (V v, T t) {
		{ v.push_back(t) };
		{ v.pop_back() };
	};


    
} //namespace simp