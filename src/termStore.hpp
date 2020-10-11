#pragma once

#include <vector>
#include <bitset>
#include <type_traits>

#include "termUtility.hpp"

namespace bmath::intern {

	//possibly preferred version for debugging
	template <typename TermUnion_T>
	class [[nodiscard]] TermStore_Table
	{
		static_assert(std::is_trivially_destructible_v<TermUnion_T>, "required to allow TermUnion_T to be used in VecElem union");
		static_assert(std::is_trivially_copyable_v<TermUnion_T>, "dunno, feels like a sane thing.");

		static constexpr std::size_t table_dist = sizeof(TermUnion_T) * 8;	//also number of elements each table keeps track of
		using OccupancyTable = std::bitset<table_dist>;

		//every table_dist'th element in vec will not store actual term content, but a table of which of 
		//the next (table_dist -1) slots are still free. 
		//thus this union can act as both.
		union [[nodiscard]] VecElem
		{
			TermUnion_T value;
			OccupancyTable table;
			static_assert(sizeof(TermUnion_T) == sizeof(OccupancyTable), "union VecElem assumes equal member size");

			VecElem(TermUnion_T new_value) :value(new_value) {}
			VecElem(unsigned long init) :table(init) {}
			VecElem(const VecElem& snd) :table(snd.table) {} //bitwise copy of snd
		};

		std::vector<VecElem> vector;

		static constexpr std::size_t no_index_found = -1;
		[[nodiscard]] std::size_t find_index_and_set_table() noexcept {
			for (std::size_t table_pos = 0; table_pos < this->vector.size(); table_pos += table_dist) {
				if (this->vector[table_pos].table.all()) [[unlikely]] {	//currently optimizes for case with only one table present
					continue;
				}
				else {
					OccupancyTable& table = this->vector[table_pos].table;
					for (std::size_t relative_pos = 1; relative_pos < table_dist; relative_pos++) {	//first bit encodes position of table -> start one later
						if (!table.test(relative_pos)) {
							table.set(relative_pos);
							return table_pos + relative_pos;
						}
					}
				}
			}
			return no_index_found;
		} //find_index_and_set_table

		//debugging function to ensure only valid accesses
		void check_index_validity(std::size_t idx) const
		{
			throw_if(this->vector.size() < idx + 1, "TermStore supposed to access/free unowned slot");
			const OccupancyTable& table = this->vector[idx / table_dist].table;
			throw_if(!table[idx % table_dist], "TermStore supposed to access/free already freed slot");
		}

	public:

		TermStore_Table(std::size_t reserve = 0) :vector()
		{
			vector.reserve(reserve);
		}

		//never construct recursively using emplace_new, as this will break if vector has to reallocate
		[[nodiscard]] std::size_t insert(TermUnion_T new_elem)
		{
			const std::size_t new_pos = this->find_index_and_set_table();
			if (new_pos == no_index_found) [[unlikely]] {
				throw_if(this->vector.size() % table_dist != 0, 
					"TermStore's insert() found no free vector index, \
					yet the next element to append to vector next is not an occupancy_table");
				//first bit is set, as table itself occupies that slot, second as new element is emplaced afterwards.
				this->vector.emplace_back(0x3);	
				this->vector.emplace_back(new_elem);
				return this->vector.size() - 1;	//index of just inserted element
			}
			else {
				if (new_pos >= this->vector.size()) {	//put new element in vector
					this->vector.emplace_back(new_elem);
				}
				else {	//reuse old element in vector
					new (&this->vector[new_pos]) VecElem(new_elem);
				}
				return new_pos;
			}
		} //insert

		void free(std::size_t idx)
		{
			check_index_validity(idx);
			OccupancyTable& table = this->vector[idx / table_dist].table;
			table.reset(idx % table_dist);
		}

		[[nodiscard]] TermUnion_T& at(std::size_t idx)
		{
			check_index_validity(idx);	
			return this->vector.at(idx).value;
		}

		[[nodiscard]] const TermUnion_T& at(std::size_t idx) const
		{
			check_index_validity(idx);		
			return this->vector.at(idx).value;
		}

		[[nodiscard]] std::size_t size() const noexcept { return vector.size(); }

		[[nodiscard]] std::vector<std::size_t> free_slots() const noexcept
		{
			std::vector<std::size_t> result;
			for (std::size_t table_pos = 0; table_pos < this->vector.size(); table_pos += table_dist) {
				if (this->vector[table_pos].table.all()) [[unlikely]] {	//currently optimizes for case with only one table present
					continue;
				}
				else {
					const OccupancyTable table = this->vector[table_pos].table;
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
		} //free_slots

		[[nodiscard]] std::size_t count_free_slots() const noexcept
		{
			std::size_t result = 0;
			for (std::size_t table_pos = 0; table_pos < this->vector.size(); table_pos += table_dist) {
				if (this->vector[table_pos].table.all()) [[unlikely]] {	//currently optimizes for case with only one table present
					continue;
				}
				else {
					const OccupancyTable table = this->vector[table_pos].table;
					for (std::size_t relative_pos = 1; relative_pos < table_dist; relative_pos++) {	//first bit encodes position of table -> start one later
						if (table_pos + relative_pos == this->vector.size()) [[unlikely]] {
							break;
						}
						if (!table.test(relative_pos)) {
							result++;
						}
					}
				}
			}
			return result;
		} //count_free_slots

	};	//class TermStore_Table




	//possibly faster version, but also with less access checks, thus with no (internal) memory savety
	template <typename TermUnion_T, typename TypedIdx_T = std::size_t>
	class [[nodiscard]] TermStore_FreeList
	{
		static_assert(std::is_trivially_destructible_v<TermUnion_T>, "required to allow TermUnion_T to be used in VecElem union");
		static_assert(std::is_trivially_copyable_v<TermUnion_T>, "dunno, feels like a sane thing.");

		struct [[nodiscard]] FreeList
		{
			static constexpr TypedIdx_T start_idx = TypedIdx_T(0);
			TypedIdx_T next;
			TypedIdx_T prev;
		};

		//at this->vector[0] always resides a node of the FreeList. all currently unused elements are listed from there.
		union [[nodiscard]] VecElem
		{
			TermUnion_T value;
			FreeList free_list;
			static_assert(sizeof(FreeList) <= sizeof(TermUnion_T), "size of union VecElem may not be defined by FreeList");

			VecElem(TermUnion_T new_value) :value(new_value) {}
			VecElem(FreeList node) :free_list(node) {}
			VecElem(const VecElem& snd) :value(snd.value) {} //bitwise copy of snd
		};

		//first elem is guaranteed to be free_list, the rest may vary.
		std::vector<VecElem> vector;

		//assumes vector to already hold first element
		[[nodiscard]] TypedIdx_T get_free_position() noexcept
		{
			FreeList& first = this->vector[FreeList::start_idx].free_list;
			if (first.next == FreeList::start_idx) {	//no free elements available (besides first)
				return FreeList::start_idx;
			}
			else {	//at least one free element available (besides first)
				const TypedIdx_T second_idx = first.next;
				const TypedIdx_T third_idx = this->vector[second_idx].free_list.next;
				FreeList& third = this->vector[third_idx].free_list;

				third.prev = FreeList::start_idx;
				first.next = third_idx;

				return second_idx;
			}
		}

	public:

		TermStore_FreeList(std::size_t reserve = 0) :vector()
		{
			vector.reserve(reserve);
		}

		//never construct recursively using emplace_new, as this will break if vector has to reallocate
		[[nodiscard]] std::size_t insert(TermUnion_T new_elem)
		{
			if (this->vector.size() == 0) [[unlikely]] {
				vector.emplace_back(FreeList{ FreeList::start_idx, FreeList::start_idx });	//free_list is only (free) element in vector -> points to itself
			}
			
			if (const TypedIdx_T free_pos = this->get_free_position(); free_pos != FreeList::start_idx) {
				new (&this->vector[free_pos]) VecElem(new_elem);
				return free_pos;
			}
			else {
				const std::size_t new_pos = this->vector.size();
				this->vector.emplace_back(new_elem);
				return new_pos;
			}
		}

		void free(std::size_t idx) noexcept
		{
			//there is currently no test if the idx was freed previously (because expensive). if so, freeing again would break the list.
			FreeList& first = this->vector[FreeList::start_idx].free_list;
			const TypedIdx_T second_idx = first.next;
			FreeList& second = this->vector[second_idx].free_list;

			FreeList& new_node = this->vector[idx].free_list;
			new_node.prev = FreeList::start_idx;	  //TermUnion_T guaranteed to be trivially destructable -> just override with FreeList
			new_node.next = second_idx;				  //TermUnion_T guaranteed to be trivially destructable -> just override with FreeList

			first.next = idx;
			second.prev = idx;
		}

		//no tests if a free_list is accessed, as only position of the first node is known anyway.
		[[nodiscard]] TermUnion_T& at(std::size_t idx) noexcept { return this->vector[idx].value; }
		[[nodiscard]] const TermUnion_T& at(std::size_t idx) const noexcept { return this->vector[idx].value; }

		[[nodiscard]] std::size_t size() const noexcept { return vector.size(); }

		[[nodiscard]] std::vector<std::size_t> free_slots() const noexcept
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
		} //free_slots

		[[nodiscard]] std::size_t count_free_slots() const noexcept
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
		} //count_free_slots

	};	//class TermStore_FreeList

	template<typename TermUnion_T>
	using TermStore = TermStore_Table<TermUnion_T>;
	//using TermStore = TermStore_FreeList<TermUnion_T>;
}