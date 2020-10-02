#pragma once

#include <type_traits>
#include <vector>
#include <algorithm>
#include <span>
#include <cassert>
#include <compare>
#include <memory_resource>

#include "termStore.hpp"
#include "termUtility.hpp"

namespace bmath::intern {

	//single linked colony is similar to a single linked list, but each node holds a whole array containing ArraySize Value_T,
	//not only a single Value_T (as an ordinary list would). TermSLC directly represents the node, there is no extra head or management.
	//TermSLC is build to reside inside a TermStore, thus it works with TypedIdx_T to access later nodes, not with pointers.
	template <typename Index_T, typename Value_T, std::size_t ArraySize>
	struct TermSLC
	{
		static_assert(std::is_unsigned_v<Index_T>);
		static_assert(std::is_trivially_destructible_v<Value_T>);
		static_assert(std::is_trivially_copyable_v<Value_T>);

		static constexpr Index_T null_index = Index_T();	//assumed to hold a value invalid as an actual index in a TermStore
		static constexpr Value_T null_value = Value_T();
		static constexpr std::size_t array_size = ArraySize; //make visible to outside

		Index_T next_idx;	//index of next SLC block in TermStore
		Value_T values[ArraySize];

		TermSLC(Value_T fst_elem = null_value) :next_idx(null_index)
		{
			this->values[0] = fst_elem;
			for (std::size_t i = 1; i < ArraySize; i++) {
				this->values[i] = null_value;
			}
		}

		//can only fill up to single block
		template<typename Container>
		TermSLC(Container new_values, Index_T next_block) :next_idx(next_block)
		{
			assert(new_values.size() <= ArraySize);
			std::memcpy(this->values, new_values.data(), new_values.size() * sizeof(Value_T));
			for (std::size_t i = new_values.size(); i < ArraySize; i++) {
				values[i] = null_value;
			}
		}

		//can only fill up to single block
		TermSLC(std::initializer_list<Value_T> new_values) :next_idx(null_index)
        {
			assert(new_values.size() <= ArraySize);
			std::memcpy(this->values, new_values.begin(), new_values.size() * sizeof(Value_T));
			for (std::size_t i = new_values.size(); i < ArraySize; i++) {
				values[i] = null_value;
			}
        }

		//convinience for that ugly reinterpret_cast thingy
		template<typename TermUnion_T>
		static TermSLC* ptr_at(TermStore<TermUnion_T>& store, std::size_t idx) 
		{ return reinterpret_cast<TermSLC*>(&store.at(idx)); }

		template<typename TermUnion_T>
		static const TermSLC* const_ptr_at(const TermStore<TermUnion_T>& store, std::size_t idx) 
		{ return reinterpret_cast<const TermSLC*>(&store.at(idx)); }

		//also allows SLC::null_index to be passed in
		template<typename TermUnion_T>
		static void free_slc(TermStore<TermUnion_T>& store, Index_T slc_idx)
		{
			Index_T last_idx;
			while (slc_idx != null_index) {
				last_idx = slc_idx;
				slc_idx = const_ptr_at(store, slc_idx)->next_idx;
				store.free(last_idx);
			};
		} //free_slc

		//returns position of node where insert happened.
		//this allows easy insertion of n elements in O(n), instead of O(n^2)
		template<typename TermUnion_T>
		static std::size_t insert_new(TermStore<TermUnion_T>& store, std::size_t slc_idx, Value_T elem)
		{
			while (true) {
				const auto slc_ptr = ptr_at(store, slc_idx);
				for (std::size_t i = 0; i < array_size; i++) {
					if (slc_ptr->values[i] == null_value) {
						slc_ptr->values[i] = elem;
						return slc_idx;
					}
				}
				if (slc_ptr->next_idx != null_index) {
					slc_idx = slc_ptr->next_idx;
				}
				else {
					const std::size_t new_idx = store.insert(TermSLC(elem));
					ptr_at(store, slc_idx)->next_idx = new_idx;
					return new_idx;
				}
			}
		} //insert_new

		//returns position of previously last node
		//this allows easy appending of n slcs of length O(1) in O(n), instead of O(n^2)
		template<typename TermUnion_T>
		static [[nodiscard]] std::size_t append(TermStore<TermUnion_T>& store, 
			Index_T this_idx, const Index_T append_idx)
		{
			//static_assert(!std::is_same_v<UnionToSLC::Result, TermString128>, "append is not meant for strings");
			auto* this_ptr = ptr_at(store, this_idx);
			while (this_ptr->next_idx != null_index) {
				this_idx = this_ptr->next_idx;
				this_ptr = ptr_at(store, this_idx);
			}
			this_ptr->next_idx = append_idx;
			return this_idx;
		} //append

		//a reference to just a TermSLC has the problem, that the adress of the next block depends not only on next_idx,
		//but also on the Store holding the TermSLC. Thus this store-aware type is used to iterate over any TermSLC.
		template<typename TermStore_T>
		struct Range
		{
			TermStore_T& store;
			Index_T slc_idx;

			Range(TermStore_T& new_store, Index_T new_slc_idx) :store(new_store), slc_idx(new_slc_idx) {}

			struct Iterator
			{
				static constexpr bool is_const = std::is_const_v<TermStore_T>;

				using value_type      = std::remove_const_t<Value_T>;
				using difference_type = void;	//no random access
				using pointer         = std::conditional_t<is_const, const value_type*, value_type*>;
				using reference       = std::conditional_t<is_const, const value_type&, value_type&>;
				using iterator_category = std::forward_iterator_tag;

				using SLC_T = std::conditional_t<is_const, const TermSLC, TermSLC>;

				//convinience for that ugly reinterpret_cast thingy
				SLC_T* store_at(std::size_t idx) { return reinterpret_cast<SLC_T*>(&this->store.at(idx)); }

				TermStore_T& store;
				Index_T current_idx;			// == 0, if whole object represents end()
				std::uint32_t array_idx;		// == SLC::array_size, if whole object represents end()

				Iterator(TermStore_T& new_store, Index_T new_current_idx, std::uint32_t new_array_idx)
					:store(new_store), current_idx(new_current_idx), array_idx(new_array_idx)
				{}

				Iterator(const Iterator& other)
					:store(other.store), current_idx(other.current_idx), array_idx(other.array_idx)
				{}

				Iterator& operator=(const Iterator& other)
				{
					this->store = other.store;
					this->current_idx = other.current_idx;
					this->array_idx = other.array_idx;
					return *this;
				}

				enum class IncrementEffect { same_node, new_node, end };

				IncrementEffect unchecked_increment()
				{
					if (++this->array_idx == SLC_T::array_size) [[unlikely]] {
						auto& current = *this->store_at(current_idx);
						if (current.next_idx == SLC_T::null_index) [[unlikely]] {
							this->current_idx = 0; //mark this as end
							return IncrementEffect::end;
						}
						else {
							this->current_idx = current.next_idx;
							this->array_idx = 0;
							return IncrementEffect::new_node;
						}
					}
					return IncrementEffect::same_node;
				}

				Iterator& operator++()
				{
					IncrementEffect what = this->unchecked_increment();
					while (what != IncrementEffect::end) {
						auto& current = *this->store_at(this->current_idx);
						if (current.values[this->array_idx] != SLC_T::null_value) {
							return *this;
						}
						what = this->unchecked_increment();
					}
					return *this;
				}

				Iterator operator++(int)
				{
					Iterator result = *this;
					this->operator++();
					return result;
				}

				[[nodiscard]] auto& operator*() 
				{ 
					auto* const current = this->store_at(this->current_idx);
					return current->values[array_idx]; 
				}

				bool operator==(const Iterator& other) const noexcept
				{
					assert(&this->store == &other.store && "only comparison between iterators in same term makes sense");
					return this->array_idx == other.array_idx && this->current_idx == other.current_idx;
				}

				bool operator!=(const Iterator& other) const noexcept { return !(*this == other); }
			};	//struct Iterator

			//not meant to normally iterate through values, as also null_value might occur at begin
			Iterator unchecked_begin() noexcept
			{
				return Iterator(this->store, this->slc_idx, 0);
			}

			Iterator begin() 
			{ 
				auto iter = this->unchecked_begin();
				const auto current = const_ptr_at(this->store, this->slc_idx);
				if (current->values[0] == null_value) [[unlikely]] {
					++iter;
				}
				return iter; 
			}

			Iterator end() noexcept { return Iterator(this->store, 0, array_size); }
		}; //struct Range

		template<typename TermUnion_T, typename Compare>
		static void sort(TermStore<TermUnion_T>& store, const Index_T slc_idx, Compare compare)
		{
			StupidBufferVector<Value_T, 12> all_values;
			auto range = Range<TermStore<TermUnion_T>>(store, slc_idx);
			for (const Value_T elem : range) {
				all_values.push_back(elem);
			}
			std::sort(all_values.begin(), all_values.end(), compare);

			auto iter = range.unchecked_begin(); //also expose empty slots
			for (std::size_t i = 0; i < all_values.size(); i++) { //copy sorted elements back (leave no gaps)
				*iter = all_values[i];
				iter.unchecked_increment(); //also expose empty slots
			}

			//clean up possible rest space (if previous SLC had gaps)
			if (iter != range.end()) {
				const auto current = ptr_at(store, iter.current_idx);
				while (iter.array_idx < array_size) {
					current->values[iter.array_idx++] = null_value;
				}
				free_slc(store, current->next_idx);
				current->next_idx = null_index;
			}
		} //sort

	};	//struct TermSLC 	


	using TermString128 = TermSLC<std::uint32_t, char, 12>;
	static_assert(sizeof(TermString128) * 8 == 128);

	template<typename TermUnion_T>
	[[nodiscard]] std::size_t insert_string(TermStore<TermUnion_T>& store, std::string_view str)
	{
		std::uint32_t prev_inserted_at = TermString128::null_index;
		{
			const std::size_t last_substr_length = str.length() % TermString128::array_size;
			if (last_substr_length > 0) {
				const std::string_view last_view = str.substr(str.length() - last_substr_length);
				prev_inserted_at = store.insert(TermString128(last_view, prev_inserted_at));
				str.remove_suffix(last_substr_length);
			}
		}
		assert((str.length() % TermString128::array_size == 0) && "last shorter bit should have been cut off already");
		while (str.length()) {
			const std::string_view last_view = str.substr(str.length() - TermString128::array_size);
			prev_inserted_at = store.insert(TermString128(last_view, prev_inserted_at));
			str.remove_suffix(TermString128::array_size);
		}
		return prev_inserted_at;
	} //insert_string

	template<typename TermUnion_T>
	void read(const TermStore<TermUnion_T>& store, std::size_t source_idx, std::string& dest)
	{
		auto current = TermString128::const_ptr_at(store, source_idx);
		while (current->next_idx != TermString128::null_index) {
			dest.append(current->values, TermString128::array_size);
			current = TermString128::const_ptr_at(store, current->next_idx);
		}
		for (std::size_t i = 0; i < TermString128::array_size; i++) {
			if (current->values[i] == '\0') [[unlikely]] {
				return;
			}
			dest.push_back(current->values[i]);
		}			
	} //read

	template<typename TermUnion_T1, typename TermUnion_T2>
	std::strong_ordering string_compare(const TermStore<TermUnion_T1>& store_1, 
		const TermStore<TermUnion_T2>& store_2, std::uint32_t idx_1, std::uint32_t idx_2)
	{
		const auto comp_array = [](const char* const lhs, const char* const rhs) -> std::strong_ordering {
			for (std::size_t i = 0; i < TermString128::array_size; i++) { //string_view not yet supports <=>
				if (lhs[i] != rhs[i]) {
					static_assert(('a' <=> 'a') == std::strong_ordering::equal); //dont wanna mix with std::strong_ordering::equivalent
					return lhs[i] <=> rhs[i];
				}
			}
			return std::strong_ordering::equal;
		};

		auto current_1 = TermString128::const_ptr_at(store_1, idx_1);
		auto current_2 = TermString128::const_ptr_at(store_2, idx_2);
		while (current_1->next_idx != TermString128::null_index &&
		       current_2->next_idx != TermString128::null_index) 
		{
			const auto cmp = comp_array(current_1->values, current_2->values);
			if (cmp != std::strong_ordering::equal) {
				return cmp;
			}
			else {
				current_1 = TermString128::const_ptr_at(store_1, current_1->next_idx);
				current_2 = TermString128::const_ptr_at(store_2, current_2->next_idx);
			}
		}
		if (current_1->next_idx == TermString128::null_index &&
			current_2->next_idx == TermString128::null_index) 
		{
			return comp_array(current_1->values, current_2->values);
		}
		else {
			return current_1->next_idx == TermString128::null_index ?
				std::strong_ordering::less : 
				std::strong_ordering::greater;
		}
	}

} //namespace bmath::intern