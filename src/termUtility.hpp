#pragma once

#include <exception>
#include <algorithm>
#include <cassert>
#include <array>
#include <vector>
#include <span>
#include <type_traits>
#include <bit>
#include <complex>

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


	template<const auto x, const auto... xs, typename T>
	constexpr bool is_one_of(const T y) noexcept
	{ 
		static_assert(sizeof(T) <= 8, "this function copies the values, ya dummie!"); //although i assume this to be inlined.
		if constexpr (!sizeof...(xs)) { return x == y; }
		else                          { return x == y || is_one_of<xs...>(y); }
	}	

	template <typename Fst_T, typename Snd_T, std::size_t Size>
	[[nodiscard]] constexpr Snd_T find_snd(
		const std::array<std::pair<Fst_T, Snd_T>, Size>& data, const Fst_T key) noexcept
	{
		const auto itr = std::find_if(begin(data), end(data), [&key](const auto &v) { return v.first == key; });
		const bool valid = itr != end(data);
		assert(valid);
		return itr->second;
	}

	template <typename Fst_T, typename Snd_T, std::size_t Size>
	[[nodiscard]] constexpr Snd_T search_snd(
		const std::array<std::pair<Fst_T, Snd_T>, Size>& data, const Fst_T key, const Snd_T null_value) noexcept 
	{
		const auto itr = std::find_if(begin(data), end(data), [&key](const auto &v) { return v.first == key; });
		return itr != end(data) ? itr->second : null_value;
	}

	template <typename Fst_T, typename Snd_T, std::size_t Size>
	[[nodiscard]] constexpr Snd_T find_fst(
		const std::array<std::pair<Fst_T, Snd_T>, Size>& data, const Snd_T key) noexcept
	{
		const auto itr = std::find_if(begin(data), end(data), [&key](const auto &v) { return v.second == key; });
		const bool valid = itr != end(data);
		assert(valid);
		return itr->first;
	}

	template <typename Fst_T, typename Snd_T, std::size_t Size>
	[[nodiscard]] constexpr Fst_T search_fst(
		const std::array<std::pair<Fst_T, Snd_T>, Size>& data, const Snd_T key, const Fst_T null_value) noexcept 
	{
		const auto itr = std::find_if(begin(data), end(data), [&key](const auto &v) { return v.second == key; });
		return itr != end(data) ? itr->first : null_value;
	}



	template<typename Value_T, std::size_t BufferSize, std::size_t FirstHeapSize = BufferSize * 2>
	class [[nodiscard]] StupidBufferVector
	{
		static_assert(BufferSize > 0u);
		static_assert(std::is_trivially_copyable_v<Value_T>);     //big part of the "Stupid" in the name
		static_assert(std::is_trivially_destructible_v<Value_T>); //big part of the "Stupid" in the name
		static_assert(std::is_default_constructible_v<Value_T>);  //big part of the "Stupid" in the name

		std::size_t size_;
		Value_T* data_;
		union
		{
			std::size_t capacity = 0u;
			Value_T local_data[BufferSize];
		};

	public:
		constexpr std::size_t size() const noexcept { return this->size_; }
		constexpr const Value_T* data() const noexcept { return this->data_; }
		constexpr Value_T* data() noexcept { return this->data_; }

		constexpr StupidBufferVector() noexcept :size_(0u), data_(local_data) {}

		constexpr StupidBufferVector(const std::initializer_list<Value_T> init) noexcept : size_(init.size()), data(local_data)
		{
			if (init.size() > BufferSize) {
				this->data_ = new Value_T[init.size()];
				this->capacity = init.size();
			}
			std::copy(init.begin(), init.end(), this->data_);
		}

		~StupidBufferVector() noexcept
		{
			if (this->size_ > BufferSize) [[unlikely]] {
				delete[] this->data_;
			}
		}

		StupidBufferVector(const StupidBufferVector&) = delete;
		StupidBufferVector& operator=(const StupidBufferVector&) = delete;
		StupidBufferVector& operator=(StupidBufferVector&&) = delete;

		StupidBufferVector(StupidBufferVector&& snd) noexcept :size_(std::exchange(snd.size_, 0u)), data_(local_data)
		{
			if (snd.data_ != snd.local_data) {
				this->data_ = std::exchange(snd.data_, nullptr);
				this->capacity = std::exchange(snd.capacity, 0u);
			}
			else {
				std::copy(snd.data_, snd.data_ + this->size_, this->data_);
			}
		}

		constexpr void push_back(const Value_T elem) noexcept
		{
			if (this->size_ == BufferSize) [[unlikely]] {
				Value_T *const new_data = new Value_T[FirstHeapSize];
				std::copy(this->data_, this->data_ + BufferSize, new_data);
				this->data_ = new_data;
				this->capacity = FirstHeapSize;
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
	class [[nodiscard]] ShortVector
	{
		static_assert(std::is_trivially_copyable_v<Value_T>);     //big part of the "Stupid" in the name
		static_assert(std::is_trivially_destructible_v<Value_T>); //big part of the "Stupid" in the name

		std::size_t size_ = 0u;
		Value_T data_[MaxSize] = {};

	public:
		constexpr std::size_t size() const noexcept { return this->size_; }
		constexpr const Value_T* data() const noexcept { return this->data_; }
		constexpr Value_T* data() noexcept { return this->data_; }

		constexpr ShortVector() noexcept = default;

		constexpr ShortVector(const std::initializer_list<Value_T> init) :size_(init.size()) noexcept
		{
			static_assert(std::is_trivially_copyable_v<Value_T>);
			assert(init.size() <= MaxSize && "initializer length exeeds MaxSize");
			std::copy(init.begin(), init.end(), this->data_);
		}

		constexpr void push_pack(const Value_T elem) noexcept
		{
			assert(this->size_ < MaxSize && "tried pushing on full vector");
			this->data_[this->size_++] = std::move(elem);
		}

		constexpr Value_T pop_back() noexcept
		{ 
			assert(this->size_ > 0u && "tried popping on empty vector");
			return std::move(this->data_[--this->size_]); 
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



	template<typename... Enums>
	class [[nodiscard]] SumEnum;

	template<>
	class [[nodiscard]] SumEnum<>
	{
	protected:
		enum class Value :unsigned {} value; //only data held by SumEnum
		static constexpr unsigned next_offset = 0u;

	public:
		constexpr SumEnum(const Value e) noexcept :value(e) {}
		constexpr operator Value() const noexcept { return this->value; }
		explicit constexpr SumEnum(const unsigned u) noexcept :value(static_cast<Value>(u)) {}
		explicit constexpr operator unsigned() const noexcept { return static_cast<unsigned>(this->value); }

		template<typename E> constexpr bool is() const noexcept
		{ static_assert(false, "method is<E>(): requested type E not part of SumEnum"); return false; }
	}; //class SumEnum<>

	template<typename Enum, typename... TailEnums>
	class [[nodiscard]] SumEnum<Enum, TailEnums...> :public SumEnum<TailEnums...>
	{
		static_assert(!std::is_integral_v<Enum>, "only expect actual enum or SumEnum /WrapEnum in template parameters");

		using Base = SumEnum<TailEnums...>;
		static constexpr unsigned this_offset = Base::next_offset;

	protected:
		using Value = typename Base::Value;
		static constexpr unsigned next_offset = unsigned(Enum::COUNT) + this_offset + 1u;

	public:
		using Base::Base;
		constexpr SumEnum(const Enum e) noexcept :Base(static_cast<Value>(unsigned(e) + this_offset)) {}

		template<typename E, std::enable_if_t<std::is_convertible_v<E, Enum> && !std::is_same_v<E, Enum>, void*> = nullptr>
		constexpr SumEnum(const E e) noexcept : Base(unsigned(Enum(e)) + this_offset) {} //Enum itself is SumEnum<...> and can be build from E

		explicit constexpr operator Enum() const noexcept { return static_cast<Enum>(unsigned(this->value) - this_offset); }

		template<typename E, std::enable_if_t<std::is_convertible_v<E, Base> && !std::is_same_v<E, Enum>, void*> = nullptr> 
		constexpr bool is() const noexcept //default case: search in parent types
		{
			static_assert(!std::is_integral_v<E>);
			return static_cast<const Base>(*this).is<E>(); 
		}

		template<typename E, std::enable_if_t<!std::is_convertible_v<E, Base> && !std::is_same_v<E, Enum>, void*> = nullptr> 
		constexpr bool is() const noexcept //Enum itself is SumEnum<...> and can be build from E -> hand over to Enum
		{
			static_assert(!std::is_integral_v<E>);
			return this->operator Enum().is<E>(); 
		}

		template<typename E, std::enable_if_t<std::is_same_v<E, Enum>, void*> = nullptr> 
		constexpr bool is() const noexcept //E is same as Enum -> just check if value is between offsets
		{ 
			static_assert(!std::is_integral_v<E>);
			return unsigned(this->value) >= this_offset && unsigned(this->value) < next_offset; 
		}

		constexpr friend std::strong_ordering operator<=>(const SumEnum&, const SumEnum&) = default;
		constexpr friend bool operator==(const SumEnum&, const SumEnum&) = default;
		static constexpr Value COUNT = static_cast<Value>(next_offset); //only relevant for outhermost instanciation
	}; //class SumEnum<Enum, TailEnums...>

	template<typename E, E Count>
	struct [[nodiscard]] WrapEnum //allows to use SumEnum with enums not having their last member named COUNT
	{
		static_assert(std::is_enum_v<E>);

		E value;
		constexpr WrapEnum(const E e) noexcept :value(e) {}
		explicit constexpr WrapEnum(const unsigned u) noexcept :value(static_cast<E>(u)) {}
		constexpr operator E() const noexcept { return this->value; }
		explicit constexpr operator unsigned() const noexcept { return static_cast<unsigned>(this->value); }
		static constexpr E COUNT = Count;
	}; //struct WrapEnum 



	//remove if c++20 libraries have catched up
	template<typename T>
	constexpr std::strong_ordering compare_arrays(const T* lhs, const T* rhs, std::size_t size)
	{
		while (size --> 1u && *lhs == *rhs) {
			lhs++;
			rhs++;
		}
		return *lhs <=> *rhs;
	}





	template<typename UInt_T>
	class [[nodiscard]] IntBitSet
	{
		static_assert(std::is_unsigned_v<UInt_T>);
		static constexpr std::size_t size = sizeof(UInt_T) * 8u;

	public:
		UInt_T data;

		constexpr IntBitSet() noexcept :data(0u) {}
		constexpr IntBitSet(const UInt_T new_data) noexcept :data(new_data) {}
		constexpr operator UInt_T() const noexcept { return this->data; }

		constexpr void   set(const std::size_t pos) noexcept { this->data |=  (UInt_T(1) << pos); }
		constexpr void reset(const std::size_t pos) noexcept { this->data &= ~(UInt_T(1) << pos); }
		constexpr void   set(const std::size_t pos, const bool val) noexcept { val ? this->set(pos) : this->reset(pos); }

		constexpr bool  test(const std::size_t pos) const noexcept { return this->data & (UInt_T(1) << pos); }
		constexpr bool  all() const noexcept { return !(~this->data); }
		constexpr bool  any() const noexcept { return this->data; }
		constexpr bool none() const noexcept { return !this->data; }

		constexpr void flip(const std::size_t pos) { this->set(pos, !this->test(pos)); }

		constexpr std::size_t count() const noexcept
		{
			//return std::popcount(this->data);
			std::size_t result = 0u;
			for (std::size_t i = 0u; i < size; i++) {
				result += this->test(i);
			}
			return result;
		}

		constexpr std::size_t find_first_true() const noexcept
		{
			//return std::countr_zero(this->data);
			for (std::size_t i = 0u; i < size; i++) {
				if (this->test(i)) { return i; }
			}
			return size;
		}

		constexpr std::size_t find_first_false() const noexcept
		{
			//return std::countr_one(this->data);
			for (std::size_t i = 0u; i < size; i++) {
				if (!this->test(i)) { return i; }
			}
			return size;
		}
	}; //class IntBitSet

	using BitSet8  = IntBitSet<std::uint8_t >;
	using BitSet16 = IntBitSet<std::uint16_t>;
	using BitSet32 = IntBitSet<std::uint32_t>;
	using BitSet64 = IntBitSet<std::uint64_t>;

	template<std::size_t Bits, std::enable_if_t<Bits % 64u == 0u, void*> = nullptr>
	class [[nodiscard]] BitSet
	{
		static constexpr std::size_t array_size = Bits / 64u;

		template<typename Pred>
		constexpr bool test_all(Pred pred) const noexcept { return std::all_of(this->data, this->data + array_size, pred); }

	public:
		BitSet64 data[array_size];

		constexpr std::size_t size() const noexcept { return Bits; }

		constexpr BitSet() noexcept :data() {}
		constexpr BitSet(const std::uint64_t new_data) noexcept : data{ new_data } {}

		constexpr void   set(const std::size_t pos) noexcept { this->data[pos / 64u].set(pos % 64u); }
		constexpr void reset(const std::size_t pos) noexcept { this->data[pos / 64u].reset(pos % 64u); }
		constexpr void   set(const std::size_t pos, const bool val) noexcept { this->data[pos / 64u].set(pos % 64u, val); }

		constexpr bool  test(const std::size_t pos) const noexcept { return this->data[pos / 64u].test(pos % 64u); }
		constexpr bool  all() const noexcept { return this->test_all([](const BitSet64 x) { return x.all(); }); }
		constexpr bool  any() const noexcept { return !this->none(); }
		constexpr bool none() const noexcept { return this->test_all([](const BitSet64 x) { return x.none(); }); }

		constexpr void flip(const std::size_t pos) { this->data[pos / 64u].flip(pos % 64u); }

		constexpr std::size_t count() const noexcept
		{
			std::size_t result = 0u;
			for (std::size_t i = 0u; i < array_size; i++) {
				result += this->data[i].count();
			}
			return result;
		}

		constexpr std::size_t find_first_true() const noexcept
		{
			for (std::size_t i = 0u; i < array_size; i++) {
				const std::size_t bit_in_i = this->data[i].find_first_true();
				if (bit_in_i != 64u) { 
					return i * 64u + bit_in_i; 
				}
			}
			return Bits;
		}

		constexpr std::size_t find_first_false() const noexcept
		{
			for (std::size_t i = 0u; i < array_size; i++) {
				const std::size_t bit_in_i = this->data[i].find_first_false();
				if (bit_in_i != 64u) { 
					return i * 64u + bit_in_i; 
				}
			}
			return Bits;
		}
	}; //class BitSet


	//can be used like std::optional<double>, but with extra double functionality
	struct OptDouble
	{
		static_assert(std::numeric_limits<double>::has_quiet_NaN);
		static constexpr double no_value = std::numeric_limits<double>::quiet_NaN();

		double val = no_value;

		constexpr OptDouble(const double new_val) noexcept :val(new_val) {}
		constexpr OptDouble() noexcept = default;

		bool has_value() const noexcept { return !std::isnan(this->val); }
		explicit operator bool() const noexcept { return this->has_value(); }
		double value_or(double snd) const noexcept { return *this ? this->val : snd; }

		constexpr double& operator*() noexcept { return this->val; }
		constexpr const double& operator*() const noexcept { return this->val; }

		constexpr OptDouble operator+(const OptDouble snd) const noexcept { return this->val + snd.val; }
		constexpr OptDouble operator-(const OptDouble snd) const noexcept { return this->val - snd.val; }
		constexpr OptDouble operator*(const OptDouble snd) const noexcept { return this->val * snd.val; }
		constexpr OptDouble operator/(const OptDouble snd) const noexcept { return this->val / snd.val; }

		constexpr OptDouble operator+=(const OptDouble snd) noexcept { this->val += snd.val; return *this; }
		constexpr OptDouble operator-=(const OptDouble snd) noexcept { this->val -= snd.val; return *this; }
		constexpr OptDouble operator*=(const OptDouble snd) noexcept { this->val *= snd.val; return *this; }
		constexpr OptDouble operator/=(const OptDouble snd) noexcept { this->val /= snd.val; return *this; }

		constexpr bool operator==(const double snd) const noexcept { return this->val == snd; }
		constexpr bool operator!=(const double snd) const noexcept { return !(*this == snd); }
	}; //struct OptDouble

	//can be used like std::optional<std::complex<double>>, but with extra direct complex functionality
	struct OptComplex
	{
		static_assert(std::numeric_limits<double>::has_quiet_NaN);
		static constexpr double no_value = std::numeric_limits<double>::quiet_NaN();

		std::complex<double> val = no_value;

		constexpr OptComplex(const std::complex<double>& new_val) noexcept :val(new_val) {}
		constexpr OptComplex() noexcept = default;

		bool has_value() const noexcept { return !std::isnan(this->val.real()); }
		explicit operator bool() const noexcept { return this->has_value(); }
		std::complex<double> value_or(std::complex<double> snd) const noexcept { return *this ? this->val : snd; }

		constexpr std::complex<double>& operator*() noexcept { return this->val; }
		constexpr const std::complex<double>& operator*() const noexcept { return this->val; }
		constexpr std::complex<double>* operator->() noexcept { return &this->val; }
		constexpr const std::complex<double>* operator->() const noexcept { return &this->val; }

		constexpr OptComplex operator+(const OptComplex& snd) const noexcept { return this->val + snd.val; }
		constexpr OptComplex operator-(const OptComplex& snd) const noexcept { return this->val - snd.val; }
		constexpr OptComplex operator*(const OptComplex& snd) const noexcept { return this->val * snd.val; }
		constexpr OptComplex operator/(const OptComplex& snd) const noexcept { return this->val / snd.val; }		

		constexpr const OptComplex& operator+=(const OptComplex& snd) noexcept { this->val += snd.val; return *this; }
		constexpr const OptComplex& operator-=(const OptComplex& snd) noexcept { this->val -= snd.val; return *this; }
		constexpr const OptComplex& operator*=(const OptComplex& snd) noexcept { this->val *= snd.val; return *this; }
		constexpr const OptComplex& operator/=(const OptComplex& snd) noexcept { this->val /= snd.val; return *this; }
	};

} //namespace bmath::intern