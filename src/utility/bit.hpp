#pragma once

#include <algorithm>
#include <cassert>
#include <bit>
#include <functional>

namespace simp {

	template<typename UInt_T>
	struct [[nodiscard]] IntBitSet
	{
		static_assert(std::is_unsigned_v<UInt_T>);

		static constexpr std::size_t npos = std::countr_zero(UInt_T(0));
		static constexpr std::size_t max_pos = npos - 1u;

		UInt_T data;

		constexpr IntBitSet() noexcept :data(0u) {}
		constexpr IntBitSet(const UInt_T new_data) noexcept :data(new_data) {}
		constexpr operator UInt_T() const noexcept { return this->data; }

		constexpr void  flip(const std::uint32_t pos) noexcept { this->data ^=  (UInt_T(1) << pos); }
		constexpr void reset(const std::uint32_t pos) noexcept { this->data &= ~(UInt_T(1) << pos); }
		constexpr void   set(const std::uint32_t pos) noexcept { this->data |=  (UInt_T(1) << pos); }
		constexpr void   set(const std::uint32_t pos, const bool val) noexcept { val ? this->set(pos) : this->reset(pos); }

		constexpr bool  test(const std::uint32_t pos) const noexcept { return this->data & (UInt_T(1) << pos); }

		constexpr void  flip_back() noexcept { this->flip(max_pos); }
		constexpr void reset_back() noexcept { this->reset(max_pos); }
		constexpr void   set_back() noexcept { this->set(max_pos); }

		constexpr bool  test_back() const noexcept { return this->test(max_pos); }

		constexpr bool none() const noexcept { return   !this->data;  }
		constexpr bool  any() const noexcept { return    this->data;  }
		constexpr bool  all() const noexcept { return !(~this->data); }

		constexpr std::uint32_t count() const noexcept { 
			return std::popcount(this->data); 
		}
		constexpr std::uint32_t find_first_true() const noexcept { return std::countr_zero(this->data); }
		constexpr std::uint32_t find_first_false() const noexcept { return std::countr_one(this->data); }

		constexpr void operator&=(const IntBitSet snd) noexcept { this->data &= snd.data; }
		constexpr void operator|=(const IntBitSet snd) noexcept { this->data |= snd.data; }
		constexpr void operator^=(const IntBitSet snd) noexcept { this->data ^= snd.data; }
	}; //class IntBitSet

	using BitSet8  = IntBitSet<std::uint8_t >;
	using BitSet16 = IntBitSet<std::uint16_t>;
	using BitSet32 = IntBitSet<std::uint32_t>;
	using BitSet64 = IntBitSet<std::uint64_t>;

	static_assert(BitSet16::npos == 16u);
	static_assert(BitSet64::npos == 64u);

	template<std::size_t Bits, std::enable_if_t<(Bits % 64u == 0u), void*> = nullptr>
	class [[nodiscard]] BitSet
	{
		static constexpr std::size_t array_size = Bits / 64u;

		template<typename Pred>
		constexpr bool test_all(const Pred pred) const noexcept { return std::all_of(this->data, this->data + array_size, pred); }

	public:
		BitSet64 data[array_size];

		constexpr std::size_t size() const noexcept { return Bits; }

		constexpr BitSet() noexcept :data{ 0u } {}
		constexpr BitSet(const std::uint64_t new_data) noexcept : data{ new_data } {}

		constexpr void  flip(const std::size_t pos) noexcept { this->data[pos / 64u].flip(pos % 64u); }
		constexpr void reset(const std::size_t pos) noexcept { this->data[pos / 64u].reset(pos % 64u); }
		constexpr void   set(const std::size_t pos) noexcept { this->data[pos / 64u].set(pos % 64u); }
		constexpr void   set(const std::size_t pos, const bool val) noexcept { this->data[pos / 64u].set(pos % 64u, val); }

		constexpr bool  test(const std::size_t pos) const noexcept { return this->data[pos / 64u].test(pos % 64u); }

		constexpr bool  all() const noexcept { return this->test_all([](const BitSet64& x) { return x.all(); }); }
		constexpr bool none() const noexcept { return this->test_all([](const BitSet64& x) { return x.none(); }); }
		constexpr bool  any() const noexcept { return !this->none(); }

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
				if (bit_in_i != BitSet64::npos) { 
					return i * 64u + bit_in_i; 
				}
			}
			return Bits;
		}

		constexpr std::size_t find_first_false() const noexcept
		{
			for (std::size_t i = 0u; i < array_size; i++) {
				const std::size_t bit_in_i = this->data[i].find_first_false();
				if (bit_in_i != BitSet64::npos) { 
					return i * 64u + bit_in_i; 
				}
			}
			return Bits;
		}
	}; //class BitSet


	//similar to std::vector<bool>, but with 128 bit local buffer
	class BitVector
	{
		std::size_t size_; //unit is bit, not BitSet64!
		constexpr std::size_t bitset_end_idx() const noexcept { return (this->size_ + 63u) / 64u; }

		BitSet64* data_ = &local_data[0];

		union
		{
			std::size_t capacity; //unit is bit, not BitSet64!
			BitSet64 local_data[2];
		};
		static constexpr std::size_t local_max_size = 64u * 2; //number of bits held by local_data

		void unsave_reallocate(const std::size_t new_capacity) noexcept //new_capacity is counted in bit
		{
			assert(new_capacity > local_max_size && (this->data_ == this->local_data || new_capacity > this->capacity) && 
				"no reallocation neccesairy");
			assert(new_capacity % 64u == 0u && "realocation may be to small");

			BitSet64* const new_data = new BitSet64[new_capacity / 64u](); //zero initialized
			const std::size_t old_end = this->bitset_end_idx();
			std::copy_n(this->data_, old_end, new_data);
			if (this->data_ != this->local_data) {
				delete[] this->data_;
			}
			this->data_ = new_data;
			this->capacity = new_capacity;
		}

	public:
		constexpr std::size_t size() const noexcept { return this->size_; }

		constexpr BitVector() noexcept :size_(0u), local_data{ 0ull, 0ull } {}

		constexpr BitVector(std::size_t size) noexcept :size_(size), local_data{ 0ull, 0ull } 
		{ 
			if (size > local_max_size) [[unlikely]] {
				const std::size_t new_capacity = std::bit_ceil(size);
				this->data_ = new BitSet64[new_capacity / 64u](); //zero initialized
				this->capacity = new_capacity;
			}
		}

		BitVector(BitVector&& snd) noexcept 
			:size_(std::exchange(snd.size_, 0u)), local_data{ snd.local_data[0], snd.local_data[1] }
		{
			if (snd.data_ != snd.local_data) {
				this->data_ = std::exchange(snd.data_, &snd.local_data[0]);
			}
		}

		BitVector(const BitSet64* const begin, const BitSet64* const end, const std::size_t new_size) noexcept 
			:size_(new_size), local_data{ 0ull, 0ull }
		{
			const std::size_t needed_capacity = (end - begin) * 64u;
			assert(needed_capacity >= new_size);
			if (needed_capacity > local_max_size) {
				this->unsave_reallocate(needed_capacity);
			}
			std::copy(begin, end, this->data_);
		}

		//not yet needed -> not yet thought about
		BitVector& operator=(const BitVector&) = delete;

		BitVector& operator=(BitVector&& snd) noexcept
		{
			this->~BitVector();
			new (this) BitVector(std::move(snd));
			return *this;
		}

		~BitVector() noexcept
		{
			if (this->data_ != this->local_data) {
				delete[] this->data_;
			}
		}
		
		void reserve(const std::size_t new_capacity) noexcept
		{
			if (new_capacity > local_max_size && 
				(this->data_ == this->local_data || new_capacity > this->capacity)) 
			{
				this->unsave_reallocate(std::bit_ceil(new_capacity));
			}
		}

		constexpr void reset_all() noexcept 
		{
			for (std::size_t i = 0u; i < this->bitset_end_idx(); i++) {
				this->data_[i] = 0ull;
			}
		}

		constexpr void clear() noexcept 
		{ 
			this->reset_all();
			this->size_ = 0u; 
		}

		constexpr void  flip(const std::size_t pos) noexcept { this->data_[pos / 64u].flip(pos % 64u); }
		constexpr void reset(const std::size_t pos) noexcept { this->data_[pos / 64u].reset(pos % 64u); }
		constexpr void   set(const std::size_t pos) noexcept { this->data_[pos / 64u].set(pos % 64u); }
		constexpr void   set(const std::size_t pos, const bool val) noexcept { this->data_[pos / 64u].set(pos % 64u, val); }

		constexpr bool  test(const std::size_t pos) const noexcept { return this->data_[pos / 64u].test(pos % 64u); }

		constexpr bool  all() const noexcept 
		{ 
			if (this->size_ == 0u) return true;
			const bool all_but_last = std::all_of(this->data_, this->data_ + this->bitset_end_idx() - 1u, 
				[](const BitSet64& x) { return x.all(); }); 
			const bool last = this->data_[this->bitset_end_idx() - 1u].count() == (this->size_ % 64u);
			return all_but_last && last;
		}

		constexpr bool none() const noexcept 
		{ 
			return std::all_of(this->data_, this->data_ + this->bitset_end_idx(), 
				[](const BitSet64& x) { return x.none(); }); 
		}

		constexpr bool  any() const noexcept { return !this->none(); }

		void push_false() noexcept 
		{  
			if (this->size_ > local_max_size) {
				if (this->size_ == this->capacity) [[unlikely]] {
					this->unsave_reallocate(this->capacity * 2u);
				}
			}
			else if (this->size_ == local_max_size) [[unlikely]] {
				this->unsave_reallocate(local_max_size * 2u);
			}
			this->size_++;
		}

		void push_true() noexcept
		{
			const std::size_t old_size = this->size_;
			this->push_false();
			this->set(old_size);
		}

		void push_back(const bool bit)
		{
			const std::size_t old_size = this->size_;
			this->push_false();
			if (bit) {
				this->set(old_size);
			}
		}

		constexpr void pop_back() noexcept
		{ 
			assert(this->size_ > 0u && "tried popping on empty vector");
			this->reset(--this->size_);
		}

		constexpr std::size_t count() const noexcept
		{
			std::size_t result = 0u;
			for (std::size_t i = 0u; i < this->bitset_end_idx(); i++) {
				result += this->data_[i].count();
			}
			return result;
		}

		constexpr std::size_t find_first_true() const noexcept
		{
			if (this->size_ == 0u) return -1ull;
			std::size_t array_idx = 0u;
			for (; array_idx < this->bitset_end_idx() - 1u; array_idx++) {
				const std::size_t bit_idx = this->data_[array_idx].find_first_true();
				if (bit_idx != BitSet64::npos) { 
					return array_idx * 64u + bit_idx; 
				}
			}
			const std::size_t bit_idx = this->data_[array_idx].find_first_true();
			const std::size_t res_idx = array_idx * 64u + bit_idx;
			return (res_idx < this->size_) ? res_idx : -1ull;
		}

		constexpr std::size_t find_first_false() const noexcept
		{
			if (this->size_ == 0u) return -1ull;
			std::size_t array_idx = 0u;
			for (; array_idx < this->bitset_end_idx() - 1u; array_idx++) {
				const std::size_t bit_idx = this->data_[array_idx].find_first_false();
				if (bit_idx != BitSet64::npos) { 
					return array_idx * 64u + bit_idx; 
				}
			}
			const std::size_t bit_idx = this->data_[array_idx].find_first_false();
			const std::size_t res_idx = array_idx * 64u + bit_idx;
			return (res_idx < this->size_) ? res_idx : -1ull;
		}

		template<std::invocable<BitSet64, BitSet64> Operation>
		constexpr BitVector& combine_with(const BitVector& snd) noexcept
		{
			assert(this->size_ == snd.size_);
			for (std::size_t array_idx = 0u; array_idx < this->bitset_end_idx(); array_idx++) {
				this->data_[array_idx] = Operation{}(this->data_[array_idx], snd.data_[array_idx]);
			}
			return *this;
		}

		constexpr BitVector& operator|=(const BitVector& snd) noexcept { return this->combine_with<std::bit_or<>>(snd); }
		constexpr BitVector& operator&=(const BitVector& snd) noexcept { return this->combine_with<std::bit_and<>>(snd); }
		constexpr BitVector& operator^=(const BitVector& snd) noexcept { return this->combine_with<std::bit_xor<>>(snd); }

		constexpr BitVector& invert() noexcept
		{
			std::size_t array_idx = 0u;
			for (; array_idx < this->bitset_end_idx() - 1u; array_idx++) {
				this->data_[array_idx] = ~this->data_[array_idx];
			}
			const std::uint64_t last_mask = -1ull >> (this->size_ % 64u);
			this->data_[array_idx] = ~this->data_[array_idx] && last_mask;
			return *this;
		}
	}; //class BitVector



    
} //namespace simp