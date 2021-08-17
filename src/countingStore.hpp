#pragma once

#include <vector>
#include <cstdint>
#include <algorithm>
#include <limits>

#include "termStore.hpp"
#include "utility/bit.hpp"


namespace simp {

	//unlike std::vector, this class can double in size if needed.
	//also unlike std::vector, there is no difference between size and capacity.
	//this in however an unsave operation, because the new elements are not initialized by enlarge
	template<typename Payload_T, template<typename> class Alloc_T = std::allocator>
	class ResizeableArray
	{
		static_assert(Allocator<Alloc_T>);
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



	template<typename Index_T>
	struct NodeManageEntry
	{
		Index_T replacement_index = Index_T();
		std::conditional_t<(sizeof(Index_T) > 4), std::int32_t, std::int16_t> ref_count = 0;
		bool final_form = false; //true, if no more rules are applicabel to this node or any subnode
	}; //struct NodeManageEntry



	//adds reference counting to BasicStore
	template<typename Payload_T, typename Index_T, template<typename> class Alloc_T = std::allocator>
	class BasicCountingStore
	{
		BasicStore<Payload_T, Alloc_T> store;

		//an allocated block starting at i of size n is reference counted at management[i].ref_count, the follwing n-1 count entries are undefined
		//if a match for this block is found, the old version of the block is kept, as long as .ref_count > 0.
		//the .ref_count shall only be incremented if the node has not moved, meaning .new_index == Index_T()
		ResizeableArray<NodeManageEntry<Index_T>, Alloc_T> management;
		static_assert(sizeof(Index_T) != 4 || sizeof(NodeManageEntry<Index_T>) == 8); //a 32 bit index causes whole management to allign to 64 bit

	public:
		using value_type = Payload_T;

		constexpr BasicCountingStore() noexcept :store(), management() {}

		template<typename MemoryRecource>
		constexpr BasicCountingStore(MemoryRecource r) noexcept :store(r), management() {}

		constexpr [[nodiscard]]       Payload_T& at(const std::size_t n)       noexcept { return this->store.at(n); }
		constexpr [[nodiscard]] const Payload_T& at(const std::size_t n) const noexcept { return this->store.at(n); }
		constexpr [[nodiscard]]       Payload_T* data()                        noexcept { return this->store.data(); }
		constexpr [[nodiscard]] const Payload_T* data()                  const noexcept { return this->store.data(); }
		constexpr [[nodiscard]]      std::size_t size()                  const noexcept { return this->store.size(); }
		constexpr [[nodiscard]]      std::size_t nr_used_slots()         const noexcept { return this->store.nr_used_slots(); }
		constexpr [[nodiscard]]      std::size_t nr_free_slots()         const noexcept { return this->store.nr_free_slots(); }
		constexpr [[nodiscard]]             auto storage_occupancy()     const noexcept { return this->store.storage_occupancy(); }

		constexpr bool valid_idx(const std::size_t n) const noexcept { return this->store.valid_idx(n) && this->management[n].ref_count > 0; }

		constexpr std::size_t allocate_one() noexcept
		{
			const std::size_t res = this->store.allocate_one();
			if (res + 1u > this->management.size()) [[unlikely]] {
				this->management.enlarge(res + 1u);
			}
			assert(this->management[res].ref_count == 0);
			this->management[res] = { .replacement_index = Index_T(), .ref_count = 1, .final_form = false };
			return res;
		} //allocate_one

		constexpr std::size_t allocate_n(const std::size_t n) noexcept
		{
			const std::size_t res = this->store.allocate_n(n);
			if (res + n > this->management.size()) [[unlikely]] {
				this->management.enlarge(res + n);
			}
			assert(this->management[res].ref_count == 0);
			this->management[res] = { .replacement_index = Index_T(), .ref_count = 1, .final_form = false };
			return res;
		} //allocate_n

		constexpr void free_one(const std::size_t idx) noexcept
		{
			assert(this->management[idx].ref_count == 0);
			this->store.free_one(idx);
		} //free_one

		constexpr void free_n(const std::size_t idx, const std::size_t n) noexcept
		{
			assert(this->management[idx].ref_count == 0);
			this->store.free_n(idx, n);
		} //free_n

		constexpr void free_all() noexcept 
		{ 
			this->store.free_all();
			BMATH_IF_DEBUG(std::fill_n(this->management.data(), this->management.size(), NodeManageEntry<Index_T>{}));
		} //free_all


		constexpr std::int32_t decr_at(const std::size_t idx) noexcept
		{
			assert(this->valid_idx(idx));
			return --this->management[idx].ref_count;
		} //decr_at

		constexpr std::int32_t incr_at(const std::size_t idx) noexcept
		{
			assert(this->valid_idx(idx));
			assert(this->management[idx].replacement_index == Index_T());
			return ++this->management[idx].ref_count;
		} //incr_at

		constexpr std::int32_t ref_count_at(const std::size_t idx) const noexcept
		{
			return this->management[idx].ref_count;
		} //ref_cout_at

		constexpr const Index_T& replacement_index(const std::size_t idx) const noexcept
		{
			assert(this->valid_idx(idx));
			return this->management[idx].replacement_index;
		} //new_index

		constexpr Index_T& replacement_index(const std::size_t idx) noexcept
		{
			assert(this->valid_idx(idx));
			return this->management[idx].replacement_index;
		} //new_index

		constexpr void mark_final(const std::size_t idx, const bool val = true) noexcept 
		{
			assert(this->valid_idx(idx));
			this->management[idx].final_form = val;
		}

		constexpr bool is_final(const std::size_t idx) const noexcept
		{
			assert(this->valid_idx(idx));
			return this->management[idx].final_form;
		}

	}; //class BasicCountingStore

} //namespace simp
