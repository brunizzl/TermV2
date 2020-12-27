#pragma once

#include <vector>
#include <type_traits>
#include <array>

#include <iostream>

#include "termUtility.hpp"
#include "typedIndex.hpp"

namespace bmath::intern {

	template <typename Payload_T>
	class [[nodiscard]] BasicStore
	{
		static_assert(std::is_trivially_destructible_v<Payload_T>, "reallocation and destruction requires this");

		static constexpr std::size_t bits_per_table = sizeof(Payload_T) * 8u;
		static_assert(bits_per_table % 64u == 0u);

		static constexpr std::size_t tables_per_capacity(const std::size_t cap) noexcept
		{ 
			return (cap + bits_per_table - 1u) / bits_per_table;
		}

		static constexpr std::size_t bitsets_per_capacity(const std::size_t cap) noexcept
		{
			return tables_per_capacity(cap) * bits_per_table / 64u;
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
			constexpr VecElem(const Table& new_table)     noexcept :table(new_table)     {}
			constexpr VecElem(const VecElem& snd)         noexcept :table(snd.table)     {} //bitwise copy of snd
			constexpr VecElem()                           noexcept :table()              {} //zero-initialize
		};

		//nonstatic members:

		//tracks both size of payload array and size of occupancy array exact
		std::size_t size_ = 0u; 

		//tracks capacity of payload array exact and is lower bound for number of bits in occupancy array
		std::size_t capacity = 0u; 

		//pointer to array of VecElem with [0 .. this->capacity) used as payload and [this->capacity .. end) used as occupancy tables
		VecElem* combined_data = nullptr; 

		//returns pointer to begin of management array
		constexpr inline BitSet64* occupancy_data() noexcept { return this->combined_data[this->capacity].table.data; }
		constexpr inline const BitSet64* occupancy_data() const noexcept { return this->combined_data[this->capacity].table.data; }


		//unsave, because if new_capacity is smaller than current this->size_, the last elements are lost and size_ is not shrunk -> possible to access invalid memory
		void unsave_change_capacity(const std::size_t new_capacity) noexcept
		{
			assert(new_capacity > this->capacity);
			assert(this->size_ <= this->capacity);

			//old capacity of payload data == this->capacity
			const std::size_t new_tables_count = tables_per_capacity(new_capacity);
			const std::size_t old_tables_count = tables_per_capacity(this->capacity);

			VecElem* const new_data = new VecElem[new_capacity + new_tables_count](); //zero-initialized
			std::copy_n(this->combined_data, this->size_, new_data); //copy old payload array into front of new payload array
			std::copy_n(this->combined_data + this->capacity, old_tables_count, new_data + new_capacity); //copy old tables into front of new tables
			delete[] this->combined_data;

			this->combined_data = new_data;
			this->capacity = new_capacity;
		} //unsave_change_capacity()

		//appends spacer_size + n uninitialized VecElems at end of payload_data, returns index of first of n new VecElem's
		//the spacer_size may elems in front of returned index are still free (duh.)
		[[nodiscard]] std::size_t at_back_allocate_alligned(std::size_t n) noexcept
		{
			const std::size_t alligned_n_size = std::min(std::bit_ceil(n), 64ull);
			const std::size_t spacer_size  = alligned_n_size - ((this->size_ - 1u) % alligned_n_size) - 1u;
			const std::size_t result_index = this->size_ + spacer_size;
			const std::size_t new_size     = this->size_ + spacer_size + n;
			if (new_size > this->capacity) {
				this->unsave_change_capacity(std::max(this->capacity * 2u, std::bit_ceil(new_size)));
			}
			this->size_ = new_size;

			{
				std::size_t mask_bitset_index = result_index / 64u;
				for (; mask_bitset_index < (new_size - 1u) / 64u; mask_bitset_index++) {
					this->occupancy_data()[mask_bitset_index] = -1ull; //set all in first (n / 64u) tables responsible for the n returned elements 
				}
				const std::uint64_t last_mask = -1ull >> (64u - (n % 64u)); //first n % 64u bits set or if (n % 64u == 0u) all 64u bit set
				this->occupancy_data()[mask_bitset_index] |= last_mask << (result_index % 64u);

				/*
				//example 1: 
				//result_index == 144, n == 10 -> new_size == 154
				//-> mask_bitset_index initialized to result_index / 64u == 2u
				//-> mask_bitset_index is not smaller than (new_size - 1u) / 64u == 2u
				//-> for loop never executed
				//-> bitset representing elements starting at mask_bitset_index * 64u == 128 
				//       gets 10 elements starting at bit index result_index % 64u == 16u set to true
				//  (these represent elements starting at 128 + 16u == 144 -> these are the n new elements)

				//example 2: 
				//result_index == 512, n == 100 -> new_size == 612
				//-> mask_bitset_index initialized to result_index / 64u == 8u
				//-> mask_bitset_index is smaller than (new_size - 1u) / 64u == 9u
				//-> for loop executed once and 64 bits representing elements starting at mask_bitset_index * 64u == 512u (ending bevore 576) are set
				//-> mask_bitset_index now == 9u
				//-> last_mask has first 100 % 64 == 36 bits set
				//-> bitset representing elements starting at mask_bitset_index * 64u == 576
				//       gets first (first because result_index % 64u == 0u) 36 bits set
				//-> success.

				//example 3: 
				//result_index == 512, n == 512 -> new_size == 1024
				//-> mask_bitset_index initialized to result_index / 64u == 8u
				//-> mask_bitset_index is smaller than (new_size - 1u) / 64u == 15u
				//-> for loop executed seven times and 64u * 7u bits representing elements starting at mask_bitset_index * 64u == 512u (ending bevore 960) are set
				//-> mask_bitset_index now == 15u
				//-> last_mask has all 64 bits set
				//-> bitset representing elements starting at mask_bitset_index * 64u == 960
				//       gets first (first because result_index % 64u == 0u) 64 bits set
				//-> we won, mr. stark.
				*/
			}
			return result_index;
		} //at_back_allocate_alligned()

	public:
		//returns pointer to begin of payload array (same adress as combinded_data, but new type)
		constexpr inline Payload_T* data() noexcept { return &this->combined_data->payload; }
		constexpr inline const Payload_T* data() const noexcept { return &this->combined_data->payload; }

		constexpr std::size_t size() const noexcept { return this->size_; }

		constexpr bool valid_idx(std::size_t idx) const noexcept 
		{
			return (idx < this->size_) && this->occupancy_data()[idx / 64u].test(idx % 64u);
		}

		constexpr BasicStore() noexcept = default;

		BasicStore(const BasicStore& snd) noexcept
		{
			this->unsave_change_capacity(snd.capacity);
			std::copy_n(snd.combined_data, this->capacity + tables_per_capacity(this->capacity), this->combined_data);
			this->size_ = snd.size_;
		}

		constexpr BasicStore(BasicStore&& snd) noexcept
			:size_(std::exchange(snd.size_, 0u))
			,capacity(std::exchange(snd.capacity, 0u))
			,combined_data(std::exchange(snd.combined_data, nullptr))
		{}

		//not yet done
		BasicStore operator=(const BasicStore& snd) = delete;
		BasicStore operator=(BasicStore&& snd) = delete;

		~BasicStore() noexcept { delete[] this->combined_data; }

		void reserve(const std::size_t new_capacity) noexcept 
		{ 
			if (new_capacity > this->capacity) {
				this->unsave_change_capacity(std::bit_ceil(new_capacity));
			}
		}

		constexpr inline [[nodiscard]] Payload_T& at(const std::size_t idx) noexcept
		{
			ASSERT(this->valid_idx(idx));
			return this->data()[idx];
		}

		constexpr inline [[nodiscard]] const Payload_T& at(const std::size_t idx) const noexcept
		{
			ASSERT(this->valid_idx(idx));
			return this->data()[idx];
		}

		//unlike unsave_change_capacity, this allocates space for a (single) Payload_T for the user, instead of (always) resizing the underlying array
		[[nodiscard]] std::size_t allocate() noexcept
		{
			const auto find_first_free_index = [this]() -> std::size_t {
				const std::size_t bitset_end_index = (this->size_ + 63u) / 64u;

				//as unused bits after logical end of bit array are guaranteed to be zero, no special case for the last bitset is required
				for (std::size_t bitset_index = 0u; bitset_index < bitset_end_index; bitset_index++) {
					const std::size_t bit_index = this->occupancy_data()[bitset_index].find_first_false();
					if (bit_index != BitSet64::npos) { 
						return bitset_index * 64u + bit_index; //may be equal to this->size_
					}
				}
				return this->size_;
			};

			const std::size_t result_index = find_first_free_index();
			if (result_index >= this->capacity) {
				this->unsave_change_capacity(std::max(4ull, 2u * this->capacity)); //std::max, because capacity may be 0
			}
			if (result_index == this->size_) {
				this->size_++;
			}
			this->occupancy_data()[result_index / 64u].set(result_index % 64u);
			return result_index;
		} //allocate()

		constexpr void free(const std::size_t idx) noexcept
		{
			assert(this->valid_idx(idx));
			this->occupancy_data()[idx / 64u].reset(idx % 64u);
		}

		[[nodiscard]] std::size_t allocate_n(const std::size_t n) noexcept
		{
			assert(n != 0u);
			if (n == 1u) { //quite a lot faster for that case
				return this->allocate(); 
			}
			if (n < 64u) { //finds the first n consecutive free bits contained in a single bitset (thus better working for small n)				
				//every bit set to true will also set the (n-1) bits below. (the nonexisting 65'th bit is assumed as true)
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
				for (std::size_t bitset_index = 0u; bitset_index < bitset_end_index; bitset_index++) {
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
						return result_index;
					}
				}
			}
			//no fitting space found (or n to large) -> allocate at end
			return this->at_back_allocate_alligned(n);
		} //allocate_n()

		constexpr void free_n(const std::size_t start, const std::size_t n) noexcept
		{
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

		//returns copy of BitSets managing storage (bit is set) means (position is currently in use) 
		[[nodiscard]] BitVector storage_occupancy() const noexcept
		{
			const auto begin = this->occupancy_data();
			const auto end = begin + bitsets_per_capacity(this->size_);
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



	template<typename Payload_T, typename Vec_T = std::vector<Payload_T>>
	class BasicMonotonicStore
	{
		Vec_T vector;

	public:
		BasicMonotonicStore() noexcept = default;

		constexpr void reserve(const std::size_t ammount) noexcept { this->vector.reserve(ammount); }

		[[nodiscard]] std::size_t insert(const Payload_T& new_elem)
		{
			const std::size_t new_pos = this->vector.size();
			this->vector.emplace_back(new_elem);
			return new_pos;
		}

		[[nodiscard]] std::size_t allocate() noexcept { return this->insert(Payload_T()); }

		[[nodiscard]] Payload_T& at(const std::size_t idx) noexcept { return this->vector[idx]; }
		[[nodiscard]] const Payload_T& at(const std::size_t idx) const noexcept { return this->vector[idx]; }

		[[nodiscard]] std::size_t size() const noexcept { return vector.size(); }
	}; //struct BasicMonotonicStore







	enum class Const :bool { no = false, yes = true };

	template<typename Union_T, typename Own_T, Const is_const>
	struct BasicNodeRef;

	//as any algorithm accessing an element of a term needs also access to its store, both store and TypedIdx info
	//  are neatly bundled as a package here
	template<typename Union_T, typename Type_T, Const is_const = Const::yes>
	struct BasicRef
	{
		using Store_T = std::conditional_t<(bool)is_const, const BasicStore<Union_T>, BasicStore<Union_T>>;
		Store_T* const store; //actual pointer to have shallow constness
		std::uint32_t index;
		Type_T type;

		constexpr BasicRef(Store_T& new_store, const BasicTypedIdx<Type_T> elem) noexcept
			:store(&new_store), index(elem.get_index()), type(elem.get_type()) {}

		constexpr BasicRef(Store_T& new_store, const std::uint32_t new_index) noexcept
			:store(&new_store), index(new_index), type(Type_T::COUNT) {}

		constexpr BasicRef(const BasicRef<Union_T, Type_T, Const::no>& ref) noexcept 
			:store(ref.store), index(ref.index), type(ref.type) {} //allow both const and mut to be initialized from mut

		constexpr auto& operator*() const { return store->at(index); }
		constexpr auto* operator->() const { return &store->at(index); }

		constexpr void set(const BasicTypedIdx<Type_T> elem) noexcept 
		{ 
			this->index = elem.get_index(); 
			this->type = elem.get_type(); 
		}

		constexpr BasicRef new_at(const BasicTypedIdx<Type_T> elem) const noexcept { return BasicRef(*this->store, elem); }

		template<typename Own_T, Const result_const = is_const>
		constexpr auto cast() const noexcept { return BasicNodeRef<Union_T, Own_T, result_const>(*this); }

		template<typename Own_T, Const result_const = is_const>
		constexpr auto new_as(const std::uint32_t new_index) const noexcept 
		{ 
			return BasicNodeRef<Union_T, Own_T, result_const>(*this->store, new_index); 
		}

		constexpr BasicTypedIdx<Type_T> typed_idx() const noexcept { return BasicTypedIdx<Type_T>(this->index, this->type); }
	}; //struct BasicRef

	template<typename Union_T, typename Type_T>
	using BasicMutRef = BasicRef<Union_T, Type_T, Const::no>;


	//in contrast to BasicRef, this struct only stands for the single type Own_T in that Union_T thingy
	template<typename Union_T, typename Own_T, Const is_const>
	struct BasicNodeRef
	{
		static_assert(std::is_convertible_v<Union_T, Own_T>);

		using Store_T = std::conditional_t<(bool)is_const, const BasicStore<Union_T>, BasicStore<Union_T>>;
		using Const_Own_T = std::conditional_t<(bool)is_const, const Own_T, Own_T>;
		Store_T* const store; //actual pointer to have shallow constness
		std::uint32_t index;

		constexpr BasicNodeRef(Store_T& new_store, const std::uint32_t new_index)
			:store(&new_store), index(new_index) {}

		template<typename Type_T>
		constexpr BasicNodeRef(const BasicRef<Union_T, Type_T, is_const>& ref) :store(ref.store), index(ref.index) {
		
		}

		constexpr auto new_at(const std::size_t new_index) const noexcept { return BasicNodeRef(*this->store, new_index); }

		constexpr auto& operator*() const { return static_cast<Const_Own_T&>(store->at(index)); }
		constexpr auto* operator->() const { return &static_cast<Const_Own_T&>(store->at(index)); }
	};

} //namespace bmath::intern