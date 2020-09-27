#pragma once

#include <type_traits>
#include <vector>
#include <algorithm>
#include <span>
#include <cassert>

#include <forward_list>

#include "termStore.hpp"

namespace bmath::intern {

	//single linked colony is similar to a single linked list, but each node holds a whole array containing ArraySize Value_T,
	//not only a single Value_T (as an ordinary list would). TermSLC directly represents the node, there is no extra head or management.
	//TermSLC is build to reside inside a TermStore, thus it works with TypedIdx_T to access later nodes, not with pointers.
	template <typename Index_T, typename Value_T, std::size_t ArraySize>
	struct TermSLC
	{
		typedef Value_T Value_T; //make visible to outside
		typedef Index_T Index_T; //make visible to outside
		static_assert(std::is_unsigned_v<Index_T>);
		static_assert(std::is_trivially_destructible_v<Value_T>);
		static_assert(std::is_trivially_copyable_v<Value_T>);

		static constexpr Index_T null_index = Index_T(0);	//assumed to hold a value invalid as an actual index in a TermStore
		static constexpr Value_T null_value = Value_T(0);
		static constexpr std::size_t array_size = ArraySize;

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
	};	//struct TermSLC 

	//a reference to just a TermSLC has the problem, that the adress of the next block depends not only on next_idx,
	//but also on the Store holding the TermSLC. Thus this store-aware type is used to iterate over any TermSLC.
	template<typename UnionToSLC, typename TermStore_T, typename SLC_T, typename Index_T>
	struct SLCRef
	{
		static_assert(std::is_unsigned_v<Index_T>);

		TermStore_T& store;
		Index_T slc_idx;

		struct RangeIterator
		{
			static constexpr bool is_const = std::is_const_v<TermStore_T>;

			using value_type      = std::remove_const_t<decltype(SLC_T::null_value)>;
			using difference_type = void;	//no random access
			using pointer         = std::conditional_t<is_const, const value_type*, value_type*>;
			using reference       = std::conditional_t<is_const, const value_type&, value_type&>;
			using iterator_category = std::forward_iterator_tag;

			TermStore_T& store;
			Index_T current_idx;			// == 0, if whole object represents end()
			std::uint32_t array_idx;		// == SLC::array_size, if whole object represents end()

			RangeIterator(TermStore_T& new_store, std::uint32_t new_current_idx, std::uint32_t new_array_idx)
				:store(new_store), current_idx(new_current_idx), array_idx(new_array_idx)
			{}

			RangeIterator(const RangeIterator& other)
				:store(other.store), current_idx(other.current_idx), array_idx(other.array_idx)
			{}

			RangeIterator& operator=(const RangeIterator& other)
			{
				this->store = other.store;
				this->current_idx = other.current_idx;
				this->array_idx = other.array_idx;
				return *this;
			}

			RangeIterator& operator++()
			{
				auto& i = this->array_idx;
				i++;
				auto* current = &UnionToSLC::apply(this->store.at(this->current_idx));
				while (true) {
					for (; i < SLC_T::array_size; i++) {
						if (current->values[i] != SLC_T::null_value) [[likely]] {
							return *this;
						}
					}
					if (current->next_idx != SLC_T::null_index) { //no valid position -> go to next block
						this->current_idx = current->next_idx;
						current = &UnionToSLC::apply(this->store.at(this->current_idx));
						i = 0;
					}
					else { //neither valid position, nor valid block -> this becomes end()
						this->current_idx = 0;
						return *this;
					}
				}
			}

			RangeIterator operator++(int)
			{
				RangeIterator result = *this;
				++(*this);
				return result;
			}

			[[nodiscard]] auto& operator*() noexcept 
			{ 
				auto* const current = &UnionToSLC::apply(this->store.at(this->current_idx));
				return current->values[array_idx]; 
			}

			bool operator==(const RangeIterator& other) const noexcept
			{
				assert(&this->store == &other.store && "only comparison between iterators in same term makes sense");
				return this->array_idx == other.array_idx && this->current_idx == other.current_idx;
			}

			bool operator!=(const RangeIterator& other) const noexcept { return !(*this == other); }
		};	//struct RangeIterator

		RangeIterator begin() noexcept { return RangeIterator(this->store, this->slc_idx, 0); }
		RangeIterator end() noexcept { return RangeIterator(this->store, 0, SLC_T::array_size); }
	}; //struct SLCRef

	template<typename UnionToSLC, typename TermStore_T, typename Index_T>
	auto range(TermStore_T& store, Index_T slc_idx)
	{
		using SLC_T = UnionToSLC::Result;
		return SLCRef<UnionToSLC, TermStore_T, SLC_T, Index_T>{ store, slc_idx };
	}



	//also allows SLC::null_index to be passed in
	template<typename UnionToSLC, typename TermUnion_T, typename Index_T>
	void free_slc(TermStore<TermUnion_T>& store, Index_T slc_idx)
	{
		Index_T last_idx(0);
		while (slc_idx != Index_T(0)) { // TypedIdx_T(0) should be equivalent to SLC::null_index
			last_idx = slc_idx;
			slc_idx = UnionToSLC::apply(store.at(slc_idx)).next_idx;
			store.free(last_idx);
		};
	}

	template<typename UnionToSLC, typename TermStore_T>
	void insert_new(TermStore_T& store, std::size_t slc_idx, decltype(UnionToSLC::Result::null_value) elem)
	{
		using SLC_T = UnionToSLC::Result;
		while (true) {
			auto* const slc_ptr = &UnionToSLC::apply(store.at(slc_idx));
			for (std::size_t i = 0; i < SLC_T::array_size; i++) {
				if (slc_ptr->values[i] == SLC_T::null_value) {
					slc_ptr->values[i] = elem;
					return;
				}
			}
			if (slc_ptr->next_idx != SLC_T::null_index) {
				slc_idx = slc_ptr->next_idx;
			}
			else {
				const std::size_t new_idx = store.insert(SLC_T(elem));
				UnionToSLC::apply(store.at(slc_idx)).next_idx = new_idx;
				return;
			}
		}
	} //insert_new

	template<typename UnionToSLC, typename TermStore_T, typename Index_T, typename Compare>
	void sort(TermStore_T& store, Index_T slc_idx, Compare compare)
	{
		static_assert(std::is_unsigned_v<Index_T>);
		using SLC_T = UnionToSLC::Result;
		using SLC_Value_T = std::remove_const_t<decltype(SLC_T::null_value)>;

		auto* current = &UnionToSLC::apply(store.at(slc_idx));

		if (auto view = range<UnionToSLC>(store, slc_idx); std::is_sorted(view.begin(), view.end(), compare)) {
			return;
		}
		else if (current->next_idx == SLC_T::null_index) {	//only a single array to sort -> dont need dynamic allocation
			std::span<SLC_Value_T> whole_array(current->values, SLC_T::array_size);
			//sord indices directly by index and in reverse order to bring SLC::null_index to the end
			std::sort(whole_array.rbegin(), whole_array.rend());
			const auto values_end = std::find(whole_array.begin(), whole_array.end(), SLC_T::null_value);
			assert(whole_array[SLC_T::array_size - 1] == SLC_T::null_value || values_end == whole_array.end()
				&& "SLC::null_value should be smallest (acutally occuring) in order of values");
			std::sort(whole_array.begin(), values_end, compare);	//here occurs the actual sorting
		}
		else {	//general case: unsorted and spanning multiple blocks
			std::vector<SLC_Value_T> all_values;		
			all_values.reserve(2 * SLC_T::array_size);
			for (SLC_Value_T& elem : range<UnionToSLC>(store, slc_idx)) {
				all_values.push_back(elem);
			}
			std::sort(all_values.begin(), all_values.end(), compare);

			std::size_t vec_i = 0;
			bool all_copied_back = false;
			while (!all_copied_back) {
				std::size_t slc_i = 0;
				for (; slc_i < SLC_T::array_size; slc_i++) {
					current->values[slc_i] = all_values[vec_i++];
					if (vec_i == all_values.size()) {
						all_copied_back = true;
						break;
					}
				}
				if (all_copied_back) {
					for (slc_i++; slc_i < SLC_T::array_size; slc_i++) {
						current->values[slc_i] = SLC_T::null_value;
					}
					free_slc<UnionToSLC>(store, current->next_idx);
					current->next_idx = SLC_T::null_index;
				}
				else {
					current = &UnionToSLC::apply(store.at(current->next_idx));	//guaranteed to work, as all_values holds <= elements compared to slc
				}
			}		
		}
	} //sort



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

	template<typename UnionToSLC, typename TermUnion_T>
	void read(const TermStore<TermUnion_T>& store, std::size_t source_idx, std::string& dest)
	{
		const TermString128* current = &UnionToSLC::apply(store.at(source_idx));
		while (current->next_idx != TermString128::null_index) {
			dest.append(current->values, TermString128::array_size);
			current = &UnionToSLC::apply(store.at(current->next_idx));
		}
		for (std::size_t i = 0; i < TermString128::array_size; i++) {
			if (current->values[i] == '\0') [[unlikely]] {
				return;
			}
			dest.push_back(current->values[i]);
		}			
	} //read

} //namespace bmath::intern