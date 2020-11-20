#pragma once

#include <vector>
#include <type_traits>
#include <array>

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

		constexpr BasicStore_Table() noexcept :vector() {}

		constexpr void reserve(const std::size_t ammount) noexcept { this->vector.reserve(ammount); }

		//never construct recursively using insert, as this will break if vector has to reallocate
		[[nodiscard]] std::size_t insert(const Union_T& new_elem) noexcept
		{
			for (std::size_t table_pos = 0; table_pos < this->vector.size(); table_pos += table_dist) {
				if (this->vector[table_pos].table.all()) [[unlikely]] {	//currently optimizes for case with only one table present
					continue;
				}
				else {
					Table& table = this->vector[table_pos].table;
					const std::size_t relative_pos = table.find_first_false(); //guaranteed to exist, as full table is handled above
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

		void free(const std::size_t idx) noexcept
		{
			assert(this->valid_idx(idx));	
			Table& table = this->vector[idx  - (idx % table_dist)].table;
			table.reset(idx % table_dist);
		}

		[[nodiscard]] std::size_t allocate() noexcept { return this->insert(Union_T()); }

		[[nodiscard]] Union_T& at(const std::size_t idx)
		{
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

		constexpr BasicStore_FreeList(std::size_t reserve = 0) :vector() { vector.reserve(reserve); }

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

		void free(const std::size_t idx) noexcept
		{
			//there is currently no test if the idx was freed previously (because expensive). if so, freeing again would break the list.
			FreeList& first = this->vector[FreeList::start_idx].free_list;
			const std::size_t second_idx = first.next;
			FreeList& new_node = this->vector[idx].free_list;

			new_node.next = second_idx;				  //Union_T guaranteed to be trivially destructable -> just override with FreeList
			first.next = idx;
		}

		[[nodiscard]] std::size_t allocate() noexcept { return this->insert(Union_T()); }

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
	using BasicStore = BasicStore_Table<Union_T>;
	//using BasicStore = BasicStore_FreeList<Union_T>;






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
		constexpr BasicNodeRef(const BasicRef<Union_T, Type_T, is_const>& ref) :store(ref.store), index(ref.index) {}

		constexpr auto new_at(const std::size_t new_index) const noexcept { return BasicNodeRef(*this->store, new_index); }

		constexpr auto& operator*() const { return static_cast<Const_Own_T&>(store->at(index)); }
		constexpr auto* operator->() const { return &static_cast<Const_Own_T&>(store->at(index)); }
	};

} //namespace bmath::intern