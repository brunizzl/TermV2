#pragma once

#include <vector>
#include <type_traits>
#include <array>

#include "utility/bit.hpp"

#include "typedIndex.hpp"


namespace bmath::intern {

	template<template <typename> class T>
	concept Allocator = requires (T<int> a, std::size_t n, int* pos) {
		{a.allocate(n)} -> std::convertible_to<int*>;
		{a.deallocate(pos, n)};
	};
	static_assert(Allocator<std::allocator>);

	//this is a the result, if an allocator had a child with std::vector
	//a StoreLike behaves kinda like an allocator, only that the allocated space is not guaranteed to stay at the same memory address
	//thus an allocation returns no address, but an index for later access via method at().
	template<typename T>
	concept StoreLike = 
		requires (const T a, std::size_t n) {
		{a.at(n)} -> std::same_as<typename T::value_type const&>; 
		{a.data()} -> std::same_as<typename T::value_type const*>; 
		{a.size()} -> std::convertible_to<std::size_t>;
		{a.valid_idx(n)} -> std::same_as<bool>;
	} && (std::is_const_v<T> || 
		requires (T a, std::size_t n) {
		{a.at(n)} -> std::same_as<typename T::value_type&>; 
		{a.data()} -> std::same_as<typename T::value_type*>; 
		{a.allocate_one()} -> std::convertible_to<std::size_t>;
		{a.allocate_n(n)} -> std::convertible_to<std::size_t>;
	});



	template <typename Payload_T, template <typename> class Alloc_T = std::allocator> 
		requires Allocator<Alloc_T>
	class [[nodiscard]] BasicStore
	{
		static_assert(std::is_trivially_destructible_v<Payload_T>, 
			"reallocation and destruction requires trivial destructability of payload");

		static constexpr std::size_t bits_per_table = sizeof(Payload_T) * 8u;
		static_assert(bits_per_table % 64u == 0u);

		static constexpr std::size_t required_table_count(const std::size_t capacity) noexcept
		{ //returns smallest number of tables having not fewer bits than capacity many.
			return (capacity + bits_per_table - 1u) / bits_per_table;
		}

		static constexpr std::size_t required_bitset_count(const std::size_t capacity) noexcept
		{ //returns smallest number of BitSet64 having not fewer bits than capacity many.
			return required_table_count(capacity) * bits_per_table / 64u;
		}

		static constexpr std::size_t required_element_count(const std::size_t capacity) noexcept
		{ //returns smallest valid size of VecElem array to allocate for capacity many payload elements
			return capacity + required_table_count(capacity);
		}

		struct Table
		{
			BitSet64 data[bits_per_table / 64u] = {};
		};
		static_assert(sizeof(Table) == sizeof(Payload_T));

		union [[nodiscard]] VecElem
		{
			Payload_T payload;
			Table table;

			constexpr VecElem(const Payload_T& new_payload) noexcept :payload(new_payload) {}
			constexpr VecElem(const Table& new_table)       noexcept :table(new_table)     {}
			constexpr VecElem(const VecElem& snd)           noexcept :table(snd.table)     {} //bitwise copy of snd
			constexpr VecElem()                             noexcept :table()              {} //zero-initialize
		};

		//nonstatic members:

		//tracks both size of payload array and size of occupancy array exact
		std::size_t size_ = 0u; 

		//tracks capacity of payload array exact and is lower bound for number of bits in occupancy array
		std::size_t capacity = 0u; 

		static constexpr std::size_t free_pos_buffer_size = 2u;
		//free_pos_buffer[i] contains lower bound for index of first bitset containing 2^i consecutive free elements
		//this allows to allocate n new elements of size 2^i in O(n), as following searches for free space can always start where the last one finished.
		struct FreePosBuffer :std::array<std::uint32_t, free_pos_buffer_size> 
		{
			//all buffer entries correspoding to free space of at least size n are set to at least new_idx
			constexpr void enlarge(const std::size_t new_bitset_idx, const std::size_t n) noexcept 
			{
				assert(n > 0u);
				for (std::size_t i = std::bit_width(n) - 1u; i < free_pos_buffer_size; i++) {
					(*this)[i] = std::max(new_bitset_idx, (std::size_t)(*this)[i]);
				}
			}

			//all buffer entries are set to at most new_idx
			constexpr void shrink(const std::size_t new_bitset_idx) noexcept
			{
				for (std::size_t i = 0; i < free_pos_buffer_size; i++) {
					(*this)[i] = std::min(new_bitset_idx, (std::size_t)(*this)[i]);
				}
			}
		} free_pos_buffer = {};

		struct Memory :Alloc_T<VecElem> 
		{
			//pointer to array of VecElem with [0 .. this->capacity) used as payload and [this->capacity .. end) used as occupancy tables
			VecElem* combined_data = nullptr;

			constexpr Memory() noexcept = default;

			template<typename MemoryResource>
			constexpr Memory(MemoryResource r) noexcept :Alloc_T<VecElem>(r) {}

			constexpr Memory(Memory&& snd) noexcept
				:Alloc_T<VecElem>(std::move(snd)),
				combined_data(std::exchange(snd.combined_data, nullptr)) {}
		} memory = {};
		static_assert(sizeof(Memory) == sizeof(VecElem*) || !std::is_empty_v<Alloc_T<VecElem>>); //use empty base class optimisation for Alloc_T == std::allocator

		//returns pointer to begin of management array
		constexpr inline BitSet64* occupancy_data() noexcept { return this->memory.combined_data[this->capacity].table.data; }
		constexpr inline const BitSet64* occupancy_data() const noexcept { return this->memory.combined_data[this->capacity].table.data; }


		//unsave, because if new_capacity is smaller than current this->size_, the last elements are lost and size_ is not shrunk -> possible to access invalid memory
		void unsave_change_capacity(const std::size_t new_capacity) noexcept
		{
			assert(new_capacity > this->capacity);

			//old capacity of payload data == this->capacity
			const std::size_t new_tables_count = required_table_count(new_capacity);
			const std::size_t old_tables_count = required_table_count(this->capacity);

			VecElem* const new_data = this->memory.allocate(new_capacity + new_tables_count); 
			std::copy_n(this->memory.combined_data, this->capacity, new_data); //copy old payload array into front of new payload array

			VecElem* new_tables_begin = new_data + new_capacity;
			std::copy_n(this->memory.combined_data + this->capacity, old_tables_count, new_tables_begin); //copy old tables into front of new tables
			std::fill_n(new_tables_begin + old_tables_count, new_tables_count - old_tables_count, VecElem{}); //set unused tables at back to 0

			if (this->memory.combined_data != nullptr) [[likely]] {
				this->memory.deallocate(this->memory.combined_data, required_element_count(this->capacity));
			}

			this->memory.combined_data = new_data;
			this->capacity = new_capacity;
		} //unsave_change_capacity()


		//appends spacer_size + n uninitialized VecElems at end of payload_data, returns index of first of n new VecElem's
		//the spacer_size may elems in front of returned index are still free (duh.)
		[[nodiscard]] std::size_t at_back_allocate_alligned(std::size_t n) noexcept
		{
			const std::size_t alligned_n_size = std::min(std::bit_ceil(n), 64ull);
			const std::size_t spacer_size  = alligned_n_size - ((this->size_ - 1u) % alligned_n_size) - 1u;
			const std::size_t result_index = this->size_ + spacer_size;
			this->size_ += spacer_size + n;
			if (this->size_ > this->capacity) {
				this->unsave_change_capacity(std::max(this->capacity * 2u, std::bit_ceil(this->size_)));
			}

			std::size_t mask_bitset_index = result_index / 64u;
			for (; mask_bitset_index < (this->size_ - 1u) / 64u; mask_bitset_index++) {
				this->occupancy_data()[mask_bitset_index] = -1ull; //set all in first (n / 64u) tables responsible for the n returned elements 
			}
			const std::uint64_t last_mask = -1ull >> (64u - (n % 64u)); //first n % 64u bits set or if (n % 64u == 0u) all 64u bit set
			this->occupancy_data()[mask_bitset_index] |= last_mask << (result_index % 64u);
			return result_index;
		} //at_back_allocate_alligned()

	public:
		using value_type = Payload_T;

		//returns pointer to begin of payload array (same adress as combinded_data, but new type)
		constexpr inline Payload_T* data() noexcept { return &this->memory.combined_data->payload; }
		constexpr inline const Payload_T* data() const noexcept { return &this->memory.combined_data->payload; }

		constexpr std::size_t size() const noexcept { return this->size_; }

		constexpr bool valid_idx(std::size_t idx) const noexcept 
		{
			return (idx < this->size_) && this->occupancy_data()[idx / 64u].test(idx % 64u);
		}

		constexpr BasicStore() noexcept = default;

		template<typename MemoryRecource>
		constexpr BasicStore(MemoryRecource r) noexcept :memory(r) {}

		BasicStore(const BasicStore& snd) noexcept
		{
			this->unsave_change_capacity(snd.capacity);
			std::copy_n(snd.memory.combined_data, 
				required_element_count(this->capacity), 
				this->memory.combined_data);
			this->size_ = snd.size_;
		}

		constexpr BasicStore(BasicStore&& snd) noexcept
			:size_(std::exchange(snd.size_, 0u))
			,capacity(std::exchange(snd.capacity, 0u))
			,memory(std::move(snd.memory))
		{}

		//not yet done
		BasicStore operator=(const BasicStore& snd) = delete;
		BasicStore operator=(BasicStore&& snd) = delete;

		~BasicStore() noexcept 
		{ 
			this->memory.deallocate(this->memory.combined_data, 
				required_element_count(this->capacity)); 
		}

		void reserve(const std::size_t new_capacity) noexcept 
		{ 
			if (new_capacity > this->capacity) {
				this->unsave_change_capacity(std::bit_ceil(new_capacity));
			}
		}

		constexpr inline [[nodiscard]] Payload_T& at(const std::size_t idx) noexcept
		{
			assert(this->valid_idx(idx));
			return this->data()[idx];
		}

		constexpr inline [[nodiscard]] const Payload_T& at(const std::size_t idx) const noexcept
		{
			assert(this->valid_idx(idx));
			return this->data()[idx];
		}

		//unlike unsave_change_capacity, this allocates space for a (single) Payload_T for the user, instead of (always) resizing the underlying array
		[[nodiscard]] std::size_t allocate_one() noexcept
		{
			const auto find_first_free_index = [this]() -> std::size_t {
				const std::size_t bitset_end_index = (this->size_ + 63u) / 64u;

				//as unused bits after logical end of bit array are guaranteed to be zero (see unsave_change_capacity), 
				//  no special case for the last bitset is required
				for (std::size_t bitset_index = this->free_pos_buffer[0]; bitset_index < bitset_end_index; bitset_index++) {
					const std::size_t bit_index = this->occupancy_data()[bitset_index].find_first_false();
					if (bit_index != BitSet64::npos) { 
						this->free_pos_buffer.enlarge(bitset_index, 1u);
						return bitset_index * 64u + bit_index; //may also be equal to this->size_
					}
				}
				return this->size_;
			};

			const std::size_t result_index = find_first_free_index();
			if (result_index >= this->capacity) [[unlikely]] {
				this->unsave_change_capacity(std::max(4ull, 2u * this->capacity)); //std::max, because capacity may be 0
			}
			if (result_index == this->size_) {
				this->size_++;
			}
			this->occupancy_data()[result_index / 64u].set(result_index % 64u);
			return result_index;
		} //allocate_one()

		constexpr void free_one(const std::size_t idx) noexcept
		{
			assert(this->valid_idx(idx));
			const std::size_t bitset_idx = idx / 64u;
			this->free_pos_buffer.shrink(bitset_idx);
			this->occupancy_data()[bitset_idx].reset(idx % 64u);
		}

		[[nodiscard]] std::size_t allocate_n(const std::size_t n) noexcept
		{
			assert(n != 0u);
			if (n == 1u) { //quite a lot faster for that case
				return this->allocate_one(); 
			}
			if (n < 64u) { //finds the first n consecutive free bits contained in a single bitset (thus better working for small n)				
				//every bit set to true will also set the (n-1) bits below. (the nonexisting 65'th bit acts as true)
				const auto spread_truth = [n](std::uint64_t to_spread) {
					std::uint64_t result = -1ull << (65u - n); //start with the (n-1) last bits already set. note: requires n >= 2
					for (std::size_t power = 1u; power <= n; power *= 2u) { //go through all powers of 2 not larger than n
						if ((n & power)) { //if the current power is used to compose n, use it to compose the result
							result |= to_spread;
							to_spread >>= power;
						}
						to_spread |= (to_spread >> power);
					}
					return result;
				};

				const std::size_t bitset_end_index = (this->size_ + 63u) / 64u;
				//as unused bits after logical end of bit array are guaranteed to be zero, no special case for the last bitset is required
				static_assert(free_pos_buffer_size == 2u, "else this->free_pos_buffer[1u] has access at index depended on n");
				for (std::size_t bitset_index = this->free_pos_buffer[1u]; bitset_index < bitset_end_index; bitset_index++) {
					const BitSet64 search_area = spread_truth(this->occupancy_data()[bitset_index]);
					const std::size_t bit_index = search_area.find_first_false();
					if (bit_index != BitSet64::npos) { 
						const std::uint64_t first_n_bit = -1ull >> (64u - n);
						this->occupancy_data()[bitset_index] |= first_n_bit << bit_index; //set bit in table

						const std::size_t result_index = bitset_index * 64u + bit_index;
						const std::size_t end_of_allocated = result_index + n;
						if (end_of_allocated > this->capacity) {
							this->unsave_change_capacity(std::bit_ceil(end_of_allocated));
						}
						this->size_ = std::max(this->size_, end_of_allocated);
						static_assert(free_pos_buffer_size == 2u, "else enlarge's second argument has to depend on n");
						this->free_pos_buffer.enlarge(bitset_index, 2u);
						return result_index;
					}
				}
			}
			//no fitting space found (or n to large) -> allocate at end
			return this->at_back_allocate_alligned(n);
		} //allocate_n()

		constexpr void free_n(const std::size_t start, const std::size_t n) noexcept
		{
			this->free_pos_buffer.shrink(start / 64u);
			if (n < 64u) {
				const std::uint64_t first_n_bit = -1ull >> (64u - n);
				const std::uint64_t mask = first_n_bit << (start % 64u);
				BitSet64& bitset = this->occupancy_data()[start / 64u];
				assert((bitset & mask) == mask); //check if all elements actually where used
				bitset &= ~mask;
			}
			else {
				assert(start % std::min(std::bit_ceil(n), 64ull) == 0u); //check if start is alligned

				std::size_t bit_index = start;
				for (; bit_index + 64u < start + n; bit_index += 64u) { //reset completely set bitsets 
					assert(this->occupancy_data()[bit_index / 64u].all()); //check if all elements actually where used
					this->occupancy_data()[bit_index / 64u] = 0ull; //reset all in first (n / 64u) tables responsible for the n freed elements 
				}
				const std::uint64_t last_mask = (-1ull >> (64u - (n % 64u))) << (start % 64u);
				BitSet64& bitset = this->occupancy_data()[bit_index / 64u];
				assert((bitset & last_mask) == last_mask); //check if all elements actually where used
				bitset &= ~last_mask;
			}
		} //free_n()

		//returns copy of BitSets managing storage (bit is set -> position is currently in use) 
		[[nodiscard]] BitVector storage_occupancy() const noexcept
		{
			const auto begin = this->occupancy_data();
			const auto end = begin + required_bitset_count(this->size_);
			return BitVector(begin, end, this->size_);
		}

		[[nodiscard]] std::size_t nr_used_slots() const noexcept
		{
			std::size_t pop_count = 0u; 
			for (std::size_t bitset_index = 0u; bitset_index < (this->size_ + 63u) / 64u; bitset_index++) {
				pop_count += this->occupancy_data()[bitset_index].count();
			}
			return pop_count;
		} //nr_used_slots

		[[nodiscard]] std::size_t nr_free_slots() const noexcept
		{
			return this->size() - this->nr_used_slots();
		} //nr_free_slots

	}; //class BasicStore








} //namespace bmath::intern