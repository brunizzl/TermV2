#pragma once

#include <exception>
#include <algorithm>
#include <cassert>
#include <array>
#include <vector>
#include <span>
#include <type_traits>

namespace bmath::intern {

	constexpr void throw_if(bool cond, const char* const msg)
	{
		if (cond) [[unlikely]] {
			throw std::exception(msg);
		}
	}

	template<typename Exception_T, typename... Args>
	constexpr void throw_if(bool cond, Args&&... args)
	{
		if (cond) [[unlikely]] {
			throw Exception_T{ std::forward<Args>(args)... };
		}
	}

	//idea stolen from Jason Turner: https://www.youtube.com/watch?v=INn3xa4pMfg
	template <typename Key_T, typename Value_T, std::size_t Size>
	[[nodiscard]] constexpr Value_T find(
		const std::array<std::pair<Key_T, Value_T>, Size>& data, const Key_T key) noexcept
	{
		const auto itr = std::find_if(begin(data), end(data),
			[&key](const auto &v) { return v.first == key; });
		if (itr == end(data)) {
			assert(false);
		}
		return itr->second;
	}

	template <typename Key_T, typename Value_T, std::size_t Size>
	[[nodiscard]] constexpr Value_T search(
		const std::array<std::pair<Key_T, Value_T>, Size>& data, const Key_T key, const Value_T null_value) noexcept 
	{
		const auto itr = std::find_if(begin(data), end(data),
			[&key](const auto &v) { return v.first == key; });
		if (itr != end(data)) {
			return itr->second;
		} else {
			return null_value;
		}
	}

	template<typename Value_T, std::size_t BufferSize>
	class StupidBufferVector
	{
		static_assert(BufferSize > 0u);
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
		constexpr Value_T* data() noexcept { return this->data_; }

		constexpr StupidBufferVector() :size_(0u), data_(local_data) {}

		~StupidBufferVector()
		{
			if (this->size_ > BufferSize) [[unlikely]] {
				delete[] this->data_;
			}
		}

		StupidBufferVector(const StupidBufferVector&) = delete;
		StupidBufferVector(StupidBufferVector&&) = delete;
		StupidBufferVector& operator=(const StupidBufferVector&) = delete;
		StupidBufferVector& operator=(StupidBufferVector&&) = delete;

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
			this->data_[this->size_++] = std::move(elem);
		}

		constexpr Value_T pop_back() noexcept
		{ 
			assert(this->size_ > 0u && "tried popping on empty vector");
			return std::move(this->data_[--this->size_]); 
		}

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
	class ShortVector
	{
		std::size_t size_ = 0u;
		Value_T data_[MaxSize];

	public:
		constexpr std::size_t size() const noexcept { return this->size_; }
		constexpr const Value_T* data() const noexcept { return this->data_; }
		constexpr Value_T* data() noexcept { return this->data_; }

		constexpr ShortVector() = default;

		constexpr ShortVector(std::initializer_list<Value_T> init) :size_(init.size())
		{
			static_assert(std::is_trivially_copyable_v<Value_T>);
			assert(init.size() <= MaxSize && "initializer length exeeds static vector limit");
			std::copy(init.begin(), init.end(), this->data_);
		}

		constexpr void push_pack(Value_T elem)
		{
			assert(this->size_ < MaxSize && "tried pushing on full vector");
			this->data_[this->size_++] = std::move(elem);
		}

		constexpr Value_T pop_back() 
		{ 
			assert(this->size_ > 0u && "tried popping on empty vector");
			return std::move(this->data_[--this->size_]); 
		}

		constexpr const Value_T& operator[](std::size_t where) const noexcept { return this->data_[where]; }
		constexpr Value_T& operator[](std::size_t where) noexcept { return this->data_[where]; }

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

	}; //class ShortVector

	template<typename Value>
	std::ostream& operator<<(std::ostream& str, const std::vector<Value>& vec)
	{
		const char* spacer = "{ ";
		for (const auto& elem : vec) {
			str << std::exchange(spacer, ", ") << elem;
		}
		return str << " }";
	}

	template<typename Value_T, std::size_t BufferSize>
	std::ostream& operator<<(std::ostream& str, const StupidBufferVector<Value_T, BufferSize>& vec)
	{
		const char* spacer = "{ ";
		for (const auto& elem : vec) {
			str << std::exchange(spacer, ", ") << elem;
		}
		return str << " }";
	}

	template<typename Value_T, std::size_t MaxSize>
	std::ostream& operator<<(std::ostream& str, const ShortVector<Value_T, MaxSize>& vec)
	{
		const char* spacer = "{ ";
		for (const auto& elem : vec) {
			str << std::exchange(spacer, ", ") << elem;
		}
		return str << " }";
	}

	template<typename... Enums>
	class SumEnum;

	//template<typename... Enums>
	//using SumEnum2 = SumEnum<Enums...>;

	template<>
	class SumEnum<>
	{
	protected:
		enum class Value :unsigned {} value;
		static constexpr unsigned next_offset = 0u;
		constexpr SumEnum(Value e) :value(e) {}

	public:
		constexpr operator Value() const { return this->value; }
		explicit constexpr operator unsigned() const { return static_cast<unsigned>(this->value); }
	};

	template<typename Enum, typename... TailEnums>
	class SumEnum<Enum, TailEnums...> :public SumEnum<TailEnums...>
	{
		using Base = SumEnum<TailEnums...>;
		static constexpr unsigned this_offset = Base::next_offset;

	protected:
		using Value = typename Base::Value;
		static constexpr unsigned next_offset = this_offset + static_cast<unsigned>(Enum::COUNT) + 1u;

	public:
		using Base::Base;
		constexpr SumEnum(Enum e) :Base(static_cast<Value>(this_offset + static_cast<unsigned>(e))) {}
		explicit constexpr operator Enum() const { return static_cast<Enum>(this->value); }

		constexpr bool operator==(const SumEnum&) const = default;      //only relevant for outhermost instance
		static constexpr Value COUNT = static_cast<Value>(next_offset); //only relevant for outhermost instance
	};

	template<typename T, auto V> struct Pair; //pair of Type and Value

	template<typename Enum, auto Count, typename... TailEnums>
	class SumEnum<Pair<Enum, Count>, TailEnums...> :public SumEnum<TailEnums...>
	{
		using Base = SumEnum<TailEnums...>;
		static constexpr unsigned this_offset = Base::next_offset;

	protected:
		using Value = typename Base::Value;
		static constexpr unsigned next_offset = this_offset + static_cast<unsigned>(Count) + 1u;

	public:
		using Base::Base;
		constexpr SumEnum(Enum e) :Base(static_cast<Value>(this_offset + static_cast<unsigned>(e))) {}
		explicit constexpr operator Enum() const { return static_cast<Enum>(this->value); }

		constexpr bool operator==(const SumEnum&) const = default;      //only relevant for outhermost instance   
		static constexpr Value COUNT = static_cast<Value>(next_offset); //only relevant for outhermost instance   
	};

} //namespace bmath::intern