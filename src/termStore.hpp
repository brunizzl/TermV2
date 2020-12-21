#pragma once

#include <vector>
#include <type_traits>
#include <array>

#include <iostream>

#include "termUtility.hpp"
#include "typedIndex.hpp"

namespace bmath::intern {

	namespace store_detail {

		//every table_dist'th element in vec will not store actual term content, but a table of which of 
		//the next (table_dist -1) slots are still free. 
		//thus this union can act as both.
		template<typename Union_T>
		union [[nodiscard]] TableVecElem
		{
			static constexpr std::size_t table_dist = sizeof(Union_T) * 8;	//also number of elements each table keeps track of
			using OccupancyTable = BitSet<table_dist>; 

			Union_T value;
			OccupancyTable table;
			static_assert(sizeof(Union_T) == sizeof(OccupancyTable), "union VecElem assumes equal member size");

			TableVecElem(const Union_T& new_value) noexcept :value(new_value) {}
			TableVecElem(unsigned long init)       noexcept :table(init)      {}
			TableVecElem(const TableVecElem& snd)  noexcept :table(snd.table) {} //bitwise copy of snd
		}; //union TableVecElem

	} //namespace store_detail

	  //possibly preferred version for debugging
	template <typename Union_T, typename Vec_T = std::vector<store_detail::TableVecElem<Union_T>>>
	class [[nodiscard]] BasicStore_Table
	{
		static_assert(std::is_trivially_destructible_v<Union_T>, "required to allow Union_T to be used in VecElem union");
		static_assert(std::is_trivially_copyable_v<Union_T>, "dunno, feels like a sane thing.");

		using VecElem = typename store_detail::TableVecElem<Union_T>;
		static constexpr std::size_t table_dist = VecElem::table_dist;
		using Table = typename VecElem::OccupancyTable;

		Vec_T vector;

	public:
		//debugging function to ensure only valid accesses
		bool valid_idx(const std::size_t idx) const noexcept
		{
			if (this->vector.size() < idx + 1u) return false; //index not currently owned by vector
			if (idx % table_dist == 0u) return false; //index is not value position, but points at table
			const Table& table = this->vector[idx  - (idx % table_dist)].table;
			if (!table.test(idx % table_dist)) return false; //index not currently populated
			return true;
		}

		constexpr BasicStore_Table() noexcept = default;

		constexpr void reserve(const std::size_t ammount) noexcept { this->vector.reserve(ammount); }

		//never construct recursively using insert, as this will break if vector has to reallocate
		[[nodiscard]] std::size_t insert(const Union_T& new_elem) noexcept
		{
			for (std::size_t table_pos = 0; table_pos < this->vector.size(); table_pos += table_dist) {
				Table& table = this->vector[table_pos].table;
				if (!table.all()) [[likely]] {	//currently optimizes for case with only one table present
					const std::size_t relative_pos = table.find_first_false();
					table.set(relative_pos);
					const std::size_t found_pos = table_pos + relative_pos;
					if (found_pos >= this->vector.size()) {	//put new element in vector
						this->vector.emplace_back(new_elem);
					}
					else {	//reuse old element in vector
						new (&this->vector[found_pos]) VecElem(new_elem);
					}
					return found_pos;
				}
			}
			//first bit is set, as table itself occupies that slot, second as new element is emplaced afterwards.
			this->vector.emplace_back(0x3);	
			this->vector.emplace_back(new_elem);
			return this->vector.size() - 1u;	//index of just inserted element
		} //insert

		[[nodiscard]] std::size_t allocate() noexcept { return this->insert(Union_T()); }

		void free(const std::size_t idx) noexcept
		{
			assert(this->valid_idx(idx));	
			Table& table = this->vector[idx  - (idx % table_dist)].table;
			table.reset(idx % table_dist);
		}

		[[nodiscard]] Union_T& at(const std::size_t idx)
		{
			if (!this->valid_idx(idx)) __debugbreak();
			assert(this->valid_idx(idx));	
			return this->vector[idx].value;
		}

		[[nodiscard]] const Union_T& at(const std::size_t idx) const
		{
			assert(this->valid_idx(idx));		
			return this->vector[idx].value;
		}

		[[nodiscard]] std::size_t size() const noexcept { return vector.size(); }

		[[nodiscard]] std::vector<std::size_t> enumerate_free_slots() const noexcept
		{
			std::vector<std::size_t> result;
			result.reserve(this->nr_free_slots());
			for (std::size_t table_pos = 0; table_pos < this->vector.size(); table_pos += table_dist) {
				if (this->vector[table_pos].table.all()) [[unlikely]] {	//currently optimizes for case with only one table present
					continue;
				}
				else {
					const Table table = this->vector[table_pos].table;
					for (std::size_t relative_pos = 1; relative_pos < table_dist; relative_pos++) {	//first bit encodes position of table -> start one later
						if (table_pos + relative_pos == this->vector.size()) [[unlikely]] {
							break;
						}
							if (!table.test(relative_pos)) {
								result.push_back(table_pos + relative_pos);
							}
					}
				}
			}
			return result;
		} //enumerate_free_slots

		[[nodiscard]] std::size_t nr_used_slots() const noexcept
		{
			std::size_t pop_count = 0; 
			for (std::size_t table_pos = 0; table_pos < this->vector.size(); table_pos += table_dist) {
				const Table& table = this->vector[table_pos].table;
				pop_count += table.count();
			}
			return pop_count;
		} //nr_used_slots

		[[nodiscard]] std::size_t nr_free_slots() const noexcept
		{
			return this->vector.size() - nr_used_slots();
		} //nr_free_slots

	};	//class BasicStore_Table






	template <typename Union_T>
	class [[nodiscard]] BasicStore_SingleTable
	{
		static_assert(std::is_trivially_destructible_v<Union_T>, "reallocation and destruction requires this");

		static constexpr std::size_t bits_per_table = sizeof(Union_T) * 8u;
		static_assert(bits_per_table % 64u == 0u);

		static constexpr std::size_t tables_per_capacity(const std::size_t cap) 
		{ 
			return (cap + bits_per_table - 1u) / bits_per_table;
		}

		struct Table
		{
			BitSet64 data[bits_per_table / 64u] = {};
		};
		static_assert(sizeof(Table) == sizeof(Union_T));

		union [[nodiscard]] VecElem
		{
			Union_T payload;
			Table table;

			constexpr VecElem(const Union_T& new_payload) noexcept :payload(new_payload) {}
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

		//returns pointer to begin of payload array (same adress as combinded_data, but new type)
		constexpr inline Union_T* payload_data() noexcept { return &this->combined_data->payload; }
		constexpr inline const Union_T* payload_data() const noexcept { return &this->combined_data->payload; }


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
			const std::size_t alligned_n_size = std::min(std::bit_ceil(n), 64u);
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
		constexpr std::size_t size() const noexcept { return this->size_; }

		constexpr bool valid_idx(std::size_t idx) const noexcept 
		{
			return (idx < this->size_) && this->occupancy_data()[idx / 64u].test(idx % 64u);
		}

		constexpr BasicStore_SingleTable() noexcept = default;

		BasicStore_SingleTable(const BasicStore_SingleTable& snd) noexcept
		{
			this->unsave_change_capacity(snd.capacity);
			std::copy_n(snd.combined_data, this->capacity + tables_per_capacity(this->capacity), this->combined_data);
			this->size_ = snd.size_;
		}

		constexpr BasicStore_SingleTable(BasicStore_SingleTable&& snd) noexcept
			:size_(std::exchange(snd.size_, 0u))
			,capacity(std::exchange(snd.capacity, 0u))
			,combined_data(std::exchange(snd.combined_data, nullptr))
		{}

		//not yet done
		BasicStore_SingleTable operator=(const BasicStore_SingleTable& snd) = delete;
		BasicStore_SingleTable operator=(BasicStore_SingleTable&& snd) = delete;

		~BasicStore_SingleTable() noexcept { delete[] this->combined_data; }

		void reserve(const std::size_t new_capacity) noexcept 
		{ 
			if (new_capacity > this->capacity) {
				this->unsave_change_capacity(std::bit_ceil(new_capacity));
			}
		}

		constexpr inline [[nodiscard]] Union_T& at(const std::size_t idx) noexcept
		{
			if (!this->valid_idx(idx)) {
				__debugbreak();
			}
			assert(this->valid_idx(idx));
			return this->payload_data()[idx];
		}

		constexpr inline [[nodiscard]] const Union_T& at(const std::size_t idx) const noexcept
		{
			if (!this->valid_idx(idx)) {
				__debugbreak();
			}
			assert(this->valid_idx(idx));
			return this->payload_data()[idx];
		}

		//unlike unsave_change_capacity, this allocates space for a (single) Union_T for the user, instead of (always) resizing the underlying array
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
				this->unsave_change_capacity(std::max(8ull, 2u * this->capacity)); //std::max, because capacity may be 0
			}
			if (result_index == this->size_) {
				this->size_++;
			}
			this->occupancy_data()[result_index / 64u].set(result_index % 64u);
			return result_index;
		} //allocate()

		[[nodiscard]] std::size_t insert(const Union_T& new_elem) noexcept
		{
			const std::size_t result_index = this->allocate();
			new (&this->at(result_index)) Union_T(new_elem);
			return result_index;
		}

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
			if (n > 64u) { //allows mask to be single std::uint64_t
				return this->at_back_allocate_alligned(n);
			}
			{
				const std::size_t n_ceil = std::bit_ceil(n);
				const std::uint64_t mask = -1ull >> (64u - n);
				std::size_t bitset_index = 0u;
				for (; bitset_index < this->size_ / 64u; bitset_index++) { //test all but last bitset (also last if all of it is used)
					const BitSet64 current = this->occupancy_data()[bitset_index];
					if (current.all()) {
						continue;
					}
					for (std::size_t offset = 0u; offset < 64u; offset += n_ceil) {
						if (!(current & (mask << offset))) {
							this->occupancy_data()[bitset_index] |= (mask << offset);
						}
					}
				}
				for (std::size_t offset = 0u; offset < this->size_ % 64u; offset += n_ceil) { //test used part of last bitset
					if (!(this->occupancy_data()[bitset_index] & (mask << offset))) {
						this->occupancy_data()[bitset_index] |= (mask << offset);
						return bitset_index * 64u + offset;
					}
				}
			}
			//no fitting space found -> allocate at end
			return this->at_back_allocate_alligned(n);
		} //allocate_n()

		constexpr void free_n(const std::size_t start, const std::size_t n) noexcept
		{
			assert(start % std::min(std::bit_ceil(n), 64ull) == 0u); //check if start is alligned

			std::size_t bit_index = start;
			for (; bit_index + 64u < start + n; bit_index += 64u) { //reset completely set bitsets 
				assert(this->occupancy_data()[bit_index / 64u].all()); //check if all elements actually where used
				this->occupancy_data()[bit_index / 64u] = 0ull; //reset all in first (n / 64u) tables responsible for the n freed elements 
			}
			const std::uint64_t last_mask = (-1ull >> (64u - (n % 64u))) << (start % 64u);
			assert((this->occupancy_data()[bit_index / 64u] & last_mask) == last_mask); //check if all elements actually where used
			this->occupancy_data()[bit_index / 64u] &= ~last_mask;
		} //free_n()

		[[nodiscard]] std::vector<std::size_t> enumerate_free_slots() const noexcept
		{
			std::vector<std::size_t> result;
			for (std::size_t i = 0u; i < this->size_; i++) {
				if (!this->occupancy_data()[i / 64u].test(i % 64u)) {
					result.push_back(i);
				}
			}
			return result;
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

	}; //class BasicStore_SingleTable





	namespace store_detail {

		struct [[nodiscard]] FreeList
		{
			static constexpr std::size_t start_idx = 0;
			std::size_t next;
		};

		//at this->vector[0] always resides a node of the FreeList. all currently unused elements are listed from there.
		template <typename Union_T>
		union [[nodiscard]] FreeListVecElem
		{
			Union_T value;
			FreeList free_list;
			static_assert(sizeof(FreeList) <= sizeof(Union_T), "size of union VecElem may not be defined by FreeList");

			FreeListVecElem(const Union_T& new_value) noexcept :value(new_value) {}
			FreeListVecElem(const FreeList& node)         noexcept :free_list(node)  {}
			FreeListVecElem(const FreeListVecElem& snd)   noexcept :value(snd.value) {} //bitwise copy of snd
		}; //union FreeListVecElem

	} //namespace store_detail

	//possibly faster version, but also with less access checks, thus with no (internal) memory savety	
	template <typename Union_T, typename Vec_T = std::vector<store_detail::FreeListVecElem<Union_T>>>
	class [[nodiscard]] BasicStore_FreeList
	{
		static_assert(std::is_trivially_destructible_v<Union_T>, "required to allow Union_T to be used in VecElem union");
		static_assert(std::is_trivially_copyable_v<Union_T>, "dunno, feels like a sane thing.");

		using VecElem = store_detail::FreeListVecElem<Union_T>;
		using FreeList = store_detail::FreeList;

		//first elem is guaranteed to be free_list, the rest may vary.
		Vec_T vector;

		//assumes vector to already hold first element
		[[nodiscard]] std::size_t get_free_position() noexcept
		{
			FreeList& first = this->vector[FreeList::start_idx].free_list;
			if (first.next == FreeList::start_idx) {	//no free elements available (besides first)
				return FreeList::start_idx;
			}
			else {	//at least one free element available (besides first)
				const std::size_t second_idx = first.next;
				const std::size_t third_idx = this->vector[second_idx].free_list.next;

				first.next = third_idx;
				return second_idx;
			}
		}

	public:
		bool valid_idx(const std::size_t idx) const noexcept { return true; } //that boring testability is task of Table version

		constexpr BasicStore_FreeList() noexcept = default;

		//never construct recursively using insert, as this will break if vector has to reallocate
		[[nodiscard]] std::size_t insert(const Union_T& new_elem)
		{
			if (this->vector.size() == 0) [[unlikely]] {
				this->vector.reserve(2u); //reserve for both first free_list node and the new element
			this->vector.emplace_back(FreeList{ FreeList::start_idx });
			this->vector.emplace_back(new_elem);
			return 1u; //new element is at second position -> index 1
			}			
			else if (const std::size_t free_pos = this->get_free_position(); free_pos != FreeList::start_idx) {
				new (&this->vector[free_pos]) VecElem(new_elem);
				return free_pos;
			}
			else {
				const std::size_t new_pos = this->vector.size();
				this->vector.emplace_back(new_elem);
				return new_pos;
			}
		}

		[[nodiscard]] std::size_t allocate() noexcept { return this->insert(Union_T()); }

		void free(const std::size_t idx) noexcept
		{
			//there is currently no test if the start was freed previously (because expensive). if so, freeing again would break the list.
			FreeList& first = this->vector[FreeList::start_idx].free_list;
			const std::size_t second_idx = first.next;
			FreeList& new_node = this->vector[idx].free_list;

			new_node.next = second_idx;				  //Union_T guaranteed to be trivially destructable -> just override with FreeList
			first.next = idx;
		}

		//no tests if a free_list is accessed, as only position of the first node is known anyway.
		[[nodiscard]] Union_T& at(const std::size_t idx) noexcept { return this->vector[idx].value; }
		[[nodiscard]] const Union_T& at(const std::size_t idx) const noexcept { return this->vector[idx].value; }

		[[nodiscard]] std::size_t size() const noexcept { return vector.size(); }

		[[nodiscard]] std::vector<std::size_t> enumerate_free_slots() const noexcept
		{
			std::vector<std::size_t> result;
			if (this->vector.size()) {
				FreeList node = this->vector[FreeList::start_idx].free_list;
				while (node.next != FreeList::start_idx) {
					result.push_back(node.next);
					node = this->vector[node.next].free_list;
				}
			}
			return result;
		} //enumerate_free_slots

		[[nodiscard]] std::size_t nr_used_slots() const noexcept
		{
			return this->vector.size() - this->nr_free_slots();
		} //nr_used_slots

		[[nodiscard]] std::size_t nr_free_slots() const noexcept
		{
			std::size_t result = 0;
			if (this->vector.size()) {
				FreeList node = this->vector[FreeList::start_idx].free_list;
				while (node.next != FreeList::start_idx) {
					result++;
					node = this->vector[node.next].free_list;
				}
			}
			return result;
		} //nr_free_slots

	};	//class BasicStore_FreeList

	template<typename Union_T>
	using BasicStore = BasicStore_SingleTable<Union_T>;
	//using BasicStore = BasicStore_Table<Union_T>;
	//using BasicStore = BasicStore_FreeList<Union_T>;



	template<typename Union_T, typename Vec_T = std::vector<Union_T>>
	class BasicMonotonicStore
	{
		Vec_T vector;

	public:
		BasicMonotonicStore() noexcept = default;

		constexpr void reserve(const std::size_t ammount) noexcept { this->vector.reserve(ammount); }

		[[nodiscard]] std::size_t insert(const Union_T& new_elem)
		{
			const std::size_t new_pos = this->vector.size();
			this->vector.emplace_back(new_elem);
			return new_pos;
		}

		[[nodiscard]] std::size_t allocate() noexcept { return this->insert(Union_T()); }

		[[nodiscard]] Union_T& at(const std::size_t idx) noexcept { return this->vector[idx]; }
		[[nodiscard]] const Union_T& at(const std::size_t idx) const noexcept { return this->vector[idx]; }

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