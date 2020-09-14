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
	//TermSLC is build to reside inside a TermStore, thus it works with Index_T to access later nodes, not with pointers.
	//NullIndex is analogous to nullptr, NullValue is a placeholder to indicate that a position in values is not yet used.
	template <typename Index_T, typename Value_T, std::size_t ArraySize>
	struct TermSLC
	{
		static_assert(std::is_unsigned_v<Index_T>);
		static_assert(std::is_trivially_destructible_v<Value_T>);

		static constexpr Index_T null_index = Index_T(0);
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

		//no order is guaranteed
		template<typename UnionToSLC, typename TermUnion_T>
		void insert_new(TermStore<TermUnion_T>& store, Value_T elem)
		{
			TermSLC* current = this;
			while (true) {
				for (std::size_t i = 0; i < ArraySize; i++) {
					if (current->values[i] == null_value) {
						current->values[i] = elem;
						return;
					}
				}
				if (current->next_idx != null_index) {
					current = &UnionToSLC::apply(store.at(current->next_idx));
				}
				else {
					current->next_idx = store.emplace_new(elem);
					return;
				}
			}
		}
	};	//struct TermSLC 

	//jumps over all instances of SLC::null_value
	template<typename UnionToSLC, typename TermUnion_T, typename SLC_Index_T, typename SLC_Value_T, std::size_t SLC_ArraySize>
	struct CheckedSLCRef
	{
		using SLC = TermSLC<SLC_Index_T, SLC_Value_T, SLC_ArraySize>;
		TermStore<TermUnion_T>& store;
		SLC& slc;

		struct RangeIterator
		{
			using value_type      = SLC_Value_T;
			using difference_type = void;	//no random access
			using pointer         = SLC_Value_T*;
			using reference       = value_type&;
			using iterator_category = std::forward_iterator_tag;

			TermStore<TermUnion_T>& store;
			SLC* current;			// == nullptr, if whole object represents end()
			std::size_t array_idx;		// == SLC::array_size, if whole object represents end()

			RangeIterator& operator++()
			{
				auto& i = this->array_idx;
				i++;
				while (true) {
					for (; i < SLC::array_size; i++) {
						if (this->current->values[i] != SLC::null_value) [[likely]] {
							return *this;
						}
					}
					if (current->next_idx != SLC::null_index) { //no valid position -> go to next block
						this->current = &UnionToSLC::apply(store.at(current->next_idx));
						i = 0;
					}
					else { //neither valid position, nor valid block -> this becomes end()
						current = nullptr;
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

			[[nodiscard]] auto& operator*() noexcept { return current->values[array_idx]; }

			bool operator==(const RangeIterator& other) const noexcept
			{
				return this->array_idx == other.array_idx && this->current == other.current;
			}

			bool operator!=(const RangeIterator& other) const noexcept { return !(*this == other); }
		};	//struct RangeIterator

		RangeIterator begin() noexcept { return RangeIterator{ this->store, &this->slc, 0 }; }
		RangeIterator end() noexcept { return RangeIterator{ this->store, nullptr, SLC::array_size }; }
	}; //struct CheckedSLCRef

	//first template parameter needs to be explicit, and of form:
	// struct UnionToSLC { static SLC_TYPE& apply(TermUnion_T& val) { return val.slc_member; }};
	template<typename UnionToSLC, typename TermUnion_T, typename SLC_Index_T, typename SLC_Value_T, std::size_t SLC_ArraySize>
	auto range(TermStore<TermUnion_T>& store, TermSLC<SLC_Index_T, SLC_Value_T, SLC_ArraySize>& slc)
	{
		return CheckedSLCRef<UnionToSLC, TermUnion_T, SLC_Index_T, SLC_Value_T, SLC_ArraySize>{store, slc};
	}

	//jumps over all instances of SLC::null_value
	template<typename UnionToSLC, typename TermUnion_T, typename SLC_Index_T, typename SLC_Value_T, std::size_t SLC_ArraySize>
	struct CheckedConstSLCRef
	{
		using SLC = TermSLC<SLC_Index_T, SLC_Value_T, SLC_ArraySize>;
		const TermStore<TermUnion_T>& store;
		const SLC& slc;

		struct RangeIterator
		{
			using value_type      = SLC_Value_T;
			using difference_type = void;	//no random access
			using pointer         = const SLC_Value_T*;
			using reference       = const value_type&;
			using iterator_category = std::forward_iterator_tag;

			const TermStore<TermUnion_T>& store;
			const SLC* current;			// == nullptr, if whole object represents end()
			std::size_t array_idx;		// == SLC::array_size, if whole object represents end()

			RangeIterator& operator++()
			{
				auto& i = this->array_idx;
				i++;
				while (true) {
					for (; i < SLC::array_size; i++) {
						if (this->current->values[i] != SLC::null_value) [[likely]] {
							return *this;
						}
					}
					if (current->next_idx != SLC::null_index) { //no valid position -> go to next block
						this->current = &UnionToSLC::apply(store.at(current->next_idx));
						i = 0;
					}
					else { //neither valid position, nor valid block -> this becomes end()
						current = nullptr;
						return *this;
					}
				}
			} //operator++

			RangeIterator operator++(int)
			{
				RangeIterator result = *this;
				++(*this);
				return result;
			}

			auto operator*() noexcept { return current->values[array_idx]; } //const anyway -> faster to return by value

			bool operator==(const RangeIterator& other) const noexcept
			{
				return this->array_idx == other.array_idx && this->current == other.current;
			}

			bool operator!=(const RangeIterator& other) const noexcept { return !(*this == other); }
		};	//struct RangeIterator

		RangeIterator begin() noexcept { return RangeIterator{ this->store, &this->slc, 0 }; }
		RangeIterator end() noexcept { return RangeIterator{ this->store, nullptr, SLC::array_size }; }
	}; //struct CheckedConstSLCRef

	//first template parameter needs to be explicit, and of form:
	// struct UnionToSLC { static SLC_TYPE& apply(TermUnion_T& val) { return val.slc_member; }};
	template<typename UnionToSLC, typename TermUnion_T, typename SLC_Index_T, typename SLC_Value_T, std::size_t SLC_ArraySize>
	auto range(const TermStore<TermUnion_T>& store, const TermSLC<SLC_Index_T, SLC_Value_T, SLC_ArraySize>& slc)
	{
		return CheckedConstSLCRef<UnionToSLC, TermUnion_T, SLC_Index_T, SLC_Value_T, SLC_ArraySize>{store, slc};
	}

	//also allows SLC::null_index to be passed in
	template<typename UnionToSLC, typename TermUnion_T, typename Index_T>
	void free_slc(TermStore<TermUnion_T>& store, Index_T slc_idx)
	{
		Index_T last_idx(0);
		while (slc_idx != Index_T(0)) { // Index_T(0) should be equivalent to SLC::null_index
			last_idx = slc_idx;
			slc_idx = UnionToSLC::apply(store.at(slc_idx)).next_idx;
			store.free(last_idx);
		};
	}

	template<typename UnionToSLC, typename TermUnion_T, typename SLC_Index_T, typename SLC_Value_T, std::size_t SLC_ArraySize, typename Compare>
	void sort(TermStore<TermUnion_T>& store, TermSLC<SLC_Index_T, SLC_Value_T, SLC_ArraySize>& slc, Compare compare)
	{
		using SLC = TermSLC<SLC_Index_T, SLC_Value_T, SLC_ArraySize>;
		if (slc.next_idx == SLC::null_index) {	//only a single array to sort -> dont need dynamic allocation
			std::span<SLC_Value_T> whole_array(slc.values, SLC::array_size);
			//sord indices directly by index and in reverse order to bring SLC::null_index to the end
			std::sort(whole_array.rbegin(), whole_array.rend());
			const auto values_end = std::find(whole_array.begin(), whole_array.end(), SLC::null_value);
			assert(whole_array[SLC::array_size - 1] == SLC::null_value || values_end == whole_array.end()
				&& "SLC::null_value should be smallest (acutally occuring) in order of values");
			std::sort(whole_array.begin(), values_end, compare);	//here occurs the actual sorting
		}
		else {
			std::vector<SLC_Value_T> all_values;		
			all_values.reserve(2 * SLC::array_size);
			for (SLC_Value_T& elem : range<UnionToSLC>(store, slc)) {
				all_values.push_back(elem);
			}
			std::sort(all_values.begin(), all_values.end(), compare);

			std::size_t vec_i = 0;
			SLC* current = &slc;
			bool all_copied_back = false;
			while (!all_copied_back) {
				std::size_t slc_i = 0;
				for (; slc_i < SLC::array_size; slc_i++) {
					current->values[slc_i] = all_values[vec_i++];
					if (vec_i == all_values.size()) {
						all_copied_back = true;
						break;
					}
				}
				if (all_copied_back) {
					for (slc_i++; slc_i < SLC::array_size; slc_i++) {
						current->values[slc_i] = SLC::null_value;
					}
					free_slc<UnionToSLC>(store, current->next_idx);
					current->next_idx = SLC::null_index;
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
				prev_inserted_at = store.emplace_new(TermString128(last_view, prev_inserted_at));
				str.remove_suffix(last_substr_length);
			}
		}
		assert((str.length() % TermString128::array_size == 0) && "last shorter bit should have been cut off already");
		while (str.length()) {
			const std::string_view last_view = str.substr(str.length() - TermString128::array_size);
			prev_inserted_at = store.emplace_new(TermString128(last_view, prev_inserted_at));
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