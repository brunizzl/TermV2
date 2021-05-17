#pragma once

#include <vector>
#include <cstdint>
#include <algorithm>
#include <limits>

#include "termStore.hpp"
#include "utility/bit.hpp"


namespace simp {


	template<typename Payload_T, template<typename> class Alloc_T = std::allocator>
	class ResizeableArray
	{
		static_assert(bmath::intern::Allocator<Alloc_T>);
		static_assert(std::is_trivially_destructible_v<Payload_T>);

		struct Memory :Alloc_T<Payload_T>
		{
			Payload_T* data_ = nullptr;
		} memory = {};

		std::size_t size_ = 0u;

	public:
		constexpr       Payload_T* data()       noexcept { return this->memory.data_; }
		constexpr const Payload_T* data() const noexcept { return this->memory.data_; }

		constexpr std::size_t size() const noexcept { return this->size_; }

		constexpr ResizeableArray() noexcept = default;

		ResizeableArray(const ResizeableArray&) = delete;
		ResizeableArray(ResizeableArray&&) = delete;
		ResizeableArray& operator=(ResizeableArray) = delete;

		constexpr ~ResizeableArray() noexcept
		{
			if (this->memory.data_ != nullptr) {
				this->memory.deallocate(this->memory.data_, this->size_);
			}
		}

		constexpr void enlarge(const std::size_t new_cap) noexcept
		{
			assert(new_cap > this->size_);

			Payload_T* const new_data = this->memory.allocate(std::bit_ceil(new_cap));
			BMATH_IF_DEBUG(std::fill_n(new_data, std::bit_ceil(new_cap), Payload_T{}));
			std::copy_n(this->memory.data_, this->size_, new_data);

			if (this->memory.data_ != nullptr) {
				this->memory.deallocate(this->memory.data_, this->size_);
			}
			this->memory.data_ = new_data;
			this->size_ = std::bit_ceil(new_cap);
		} //change_capacity

		constexpr Payload_T& operator[](const std::size_t n) noexcept
		{
			assert(n < this->size_);
			return this->memory.data_[n];
		}

		constexpr const Payload_T& operator[](const std::size_t n) const noexcept
		{
			assert(n < this->size_);
			return this->memory.data_[n];
		}
	}; //class ResizeableArray



	struct RefCount
	{
		std::int8_t data = 0;

		constexpr operator std::int8_t() const noexcept { return this->data; }

		constexpr RefCount& operator=(const std::int8_t i) noexcept { this->data = i; return *this; }

		constexpr std::int8_t sign() const noexcept { return this->data < 0 ? -1 : 1; }

		constexpr RefCount& operator++() noexcept { this->data += this->sign(); return *this; }
		constexpr RefCount& operator--() noexcept { this->data -= this->sign(); return *this; }
	}; //struct RefCount



	//adds reference counting to bmath::intern::BasicStore
	template<typename Payload_T, template<typename> class Alloc_T = std::allocator>
	class BasicCountingStore
	{
		bmath::intern::BasicStore<Payload_T, Alloc_T> store;

		//an allocated block starting at i of size n is counted at count[i], the follwing n-1 count entries are undefined
		ResizeableArray<RefCount, Alloc_T> count;

	public:
		using value_type = Payload_T;

		constexpr BasicCountingStore() noexcept :store(), count() {}

		template<typename MemoryRecource>
		constexpr BasicCountingStore(MemoryRecource r) noexcept :store(r), count() {}

		constexpr [[nodiscard]]       Payload_T& at(const std::size_t n)       noexcept { return this->store.at(n); }
		constexpr [[nodiscard]] const Payload_T& at(const std::size_t n) const noexcept { return this->store.at(n); }
		constexpr [[nodiscard]]       Payload_T* data()                        noexcept { return this->store.data(); }
		constexpr [[nodiscard]] const Payload_T* data()                  const noexcept { return this->store.data(); }
		constexpr [[nodiscard]]      std::size_t size()                  const noexcept { return this->store.size(); }
		constexpr [[nodiscard]]      std::size_t nr_used_slots()         const noexcept { return this->store.nr_used_slots(); }
		constexpr [[nodiscard]]      std::size_t nr_free_slots()         const noexcept { return this->store.nr_free_slots(); }

		constexpr [[nodiscard]] bmath::intern::BitVector storage_occupancy() const noexcept { return this->store.storage_occupancy(); }

		constexpr bool valid_idx(const std::size_t n) const noexcept { return this->store.valid_idx(n) && this->count[n] != 0; }

		constexpr std::size_t allocate_one() noexcept
		{
			const std::size_t res = this->store.allocate_one();
			if (res + 1u > this->count.size()) [[unlikely]] {
				this->count.enlarge(res + 1u);
			}
			assert(this->count[res] == 0);
			this->count[res] = 1;
			return res;
		} //allocate_one

		constexpr std::size_t allocate_n(const std::size_t n) noexcept
		{
			const std::size_t res = this->store.allocate_n(n);
			if (res + n > this->count.size()) [[unlikely]] {
				this->count.enlarge(res + n);
			}
			assert(this->count[res] == 0);
			this->count[res] = 1;
			return res;
		} //allocate_n

		constexpr void free_one(const std::size_t idx) noexcept
		{
			assert(std::abs(this->count[idx]) == 0);
			this->store.free_one(idx);
		} //free_one

		constexpr void free_n(const std::size_t idx, const std::size_t n) noexcept
		{
			assert(std::abs(this->count[idx]) == 0);
			this->store.free_n(idx, n);
		} //free_n

		constexpr void free_all() noexcept 
		{ 
			this->store.free_all();
			BMATH_IF_DEBUG(std::fill_n(this->count.data(), this->count.size(), 0));
		} //free_all

		constexpr       RefCount& count_at(const std::size_t n)       noexcept { assert(this->valid_idx(n)); return this->count[n]; }
		constexpr const RefCount& count_at(const std::size_t n) const noexcept { assert(this->valid_idx(n)); return this->count[n]; }

	}; //class BasicCountingStore

} //namespace simp
