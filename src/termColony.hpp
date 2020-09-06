#pragma once

#include <type_traits>
#include <vector>
#include <cassert>

#include "termStore.hpp"

namespace bmath::intern {

	//single linked colony is similar to a single linked list, but each node holds a whole array containing ArraySize Value_T,
	//not only a single Value_T (as an ordinary list would). TermSLC directly represents the node, there is no extra head or management.
	//TermSLC is build to reside inside a TermStore, thus it works with Index_T to access later nodes, not with pointers.
	//NullIndex is analogous to nullptr, NullValue is a placeholder to indicate that a position in values is not yet used.
	template <typename Index_T, typename Value_T, std::size_t ArraySize, Index_T NullIndex = 0, long NullValue = 0>
	struct TermSLC
	{
		static_assert(std::is_unsigned_v<Index_T>);
		static_assert(std::is_trivially_destructible_v<Value_T>);

		static constexpr Index_T null_index = Index_T(NullIndex);
		static constexpr Value_T null_value = Value_T(NullValue);
		static constexpr std::size_t array_size = ArraySize;

		Index_T next_block_idx;
		Value_T values[ArraySize];

		TermSLC(Value_T fst_elem = null_value) :next_block_idx(null_index)
		{
			this->values[0] = fst_elem;
			for (std::size_t i = 1; i < ArraySize; i++) {
				this->values[i] = null_value;
			}
		}

		template<typename Container>
		TermSLC(const Container& new_values, Index_T next_block = null_index) :next_block_idx(next_block)
		{
			assert(new_values.size() <= ArraySize);
			std::memcpy(this->values, new_values.data(), new_values.size() * sizeof(Value_T));
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
				if (current->next_block_idx != null_index) {
					current = &UnionToSLC::apply(store.at(current->next_block_idx));
				}
				else {
					current->next_block_idx = store.emplace_new(elem);
					return;
				}
			}
		}
	};	//struct TermSLC 

	template<typename UnionToSLC, typename TermUnion_T, typename SLC_Index_T, typename SLC_Value_T, std::size_t SLC_ArraySize>
	struct SLC_Ref
	{
		using SLC = TermSLC<SLC_Index_T, SLC_Value_T, SLC_ArraySize>;

		TermStore<TermUnion_T>& store;
		SLC& slc;

		struct RangeIterator
		{
			TermStore<TermUnion_T>& store;
			SLC* current_block;			// == nullptr, if whole object represents end()
			std::size_t array_idx;		// == SLC::array_size, if whole object represents end()

			void operator++()
			{
				this->array_idx++;
				while (true) {
					while (this->array_idx < SLC::array_size && this->current_block->values[array_idx] == SLC::null_value) 
					[[unlikely]] {
						this->array_idx++;
					}

					if (this->array_idx < SLC::array_size) [[likely]] { //valid position and valid element -> done
						return;
					}
					else if (current_block->next_block_idx != SLC::null_index) { //no valid position -> go to next block
						this->current_block = &UnionToSLC::apply(store.at(current_block->next_block_idx));
						this->array_idx = 0;
					}
					else { //neither valid position, nor valid block -> this becomes end()
						current_block = nullptr;
						return;
					}
				}
			}

			auto& operator*() noexcept
			{
				assert(this->current_block->values[this->array_idx] != SLC::null_value && this->array_idx < SLC::array_size);
				return current_block->values[array_idx];
			}

			bool operator==(const RangeIterator& other) const noexcept
			{
				return this->array_idx == other.array_idx && this->current_block == other.current_block;
			}
		};	//struct RangeIterator

		RangeIterator begin() { return RangeIterator{ this->store, &this->slc, 0 }; }
		RangeIterator end() { return RangeIterator{ this->store, nullptr, SLC::array_size }; }
	}; //struct SLC_Ref

	//first template parameter needs to be explicit, and of form:
	// struct UnionToSLC { static SLC_TYPE& apply(TermUnion_T& val) { return val.slc_member; }};
	template<typename UnionToSLC, typename TermUnion_T, typename SLC_Index_T, typename SLC_Value_T, std::size_t SLC_ArraySize>
	auto range(TermStore<TermUnion_T>& store, TermSLC<SLC_Index_T, SLC_Value_T, SLC_ArraySize>& slc)
	{
		return SLC_Ref<UnionToSLC, TermUnion_T, SLC_Index_T, SLC_Value_T, SLC_ArraySize>{store, slc};
	}


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
	}

	template<typename UnionToSLC, typename TermUnion_T>
	void read(const TermStore<TermUnion_T>& store, std::size_t source_idx, std::string& dest)
	{
		const TermString128* current = &UnionToSLC::apply(store.at(source_idx));
		while (current->next_block_idx != TermString128::null_index) {
			dest.append(current->values, TermString128::array_size);
			current = &UnionToSLC::apply(store.at(current->next_block_idx));
		}
		for (std::size_t i = 0; i < TermString128::array_size; i++) {
			if (current->values[i] == '\0') [[unlikely]] {
				return;
			}
			dest.push_back(current->values[i]);
		}			
	}
}