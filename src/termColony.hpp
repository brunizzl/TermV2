#pragma once

#include <type_traits>
#include <vector>
#include <array>
#include <functional>
#include <limits>
#include <cassert>

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

		Index_T next_block_idx;
		Value_T values[ArraySize];

		TermSLC() :next_block_idx(null_index)
		{
			for (std::size_t i = 0; i < ArraySize; i++) {
				values[i] = null_value;
			}
		}

		TermSLC(std::initializer_list<Value_T> list) :next_block_idx(null_index)
		{
			assert(list.size() <= ArraySize);
			std::size_t i = 0;
			for (auto& val : list) {
				this->values[i] = val;
				i++;
			}
			for (; i < ArraySize; i++) {
				this->values[i] = null_value;
			}
		}

		//no order is guaranteed
		template<typename TermUnion_T>
		void insert_new(TermStore<TermUnion_T>& store, Value_T elem)
		{
			for (std::size_t i = 0; i < ArraySize; i++) {
				if (this->values[i] != null_value) {
					this->values[i] = elem;
					return;
				}
			}

			if (this->next_block_idx != null_index) {
				TermSLC& next_block = store.at(this->next_block_idx);
				next_block.insert_new(store, elem);
			}
			else {
				this->next_block_idx = store.insert_new(TermSLC(elem));
			}
		}
	};	//struct TermSLC 

	template<typename UnionToSLC, typename TermUnion_T, typename SLC_Index_T, typename SLC_Value_T, std::size_t SLC_ArraySize>
	struct SLC_Ref
	{
		using SLC = TermSLC<SLC_Index_T, SLC_Value_T, SLC_ArraySize>;

		TermStore<TermUnion_T>& store;
		SLC& slc;
		const UnionToSLC& union_to_slc;

		struct Iterator
		{
			TermStore<TermUnion_T>& store;
			SLC* current_block;			// == nullptr, if whole object represents end()
			std::size_t array_idx;		// == SLC::array_size, if whole object represents end()
			const UnionToSLC& union_to_slc;

			void operator++()
			{
				this->array_idx++;
				while (true) {
					while (this->array_idx < SLC::array_size && this->current_block->values[array_idx] == SLC::null_value) 
						[[unlikely]] {
						this->array_idx++;
					}

					if (this->array_idx < SLC::array_size) [[likely]] {
						return;
					}
					else if (current_block->next_block_idx != SLC::null_index) {
						this->current_block = &union_to_slc(store.at(current_block->next_block_idx));
						this->array_idx = 0;
					}
					else {
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

			bool operator==(const Iterator& other) const noexcept
			{
				return this->array_idx == other.array_idx && this->current_block == other.current_block;
			}
		};	//struct Iterator

		Iterator begin()
		{
			return Iterator{ this->store, &this->slc, 0, union_to_slc };
		}

		Iterator end()
		{
			return Iterator{ this->store, nullptr, SLC::array_size, union_to_slc };
		}
	}; //struct SLC_Ref

	template<typename UnionToSLC, typename TermUnion_T, typename SLC_Index_T, typename SLC_Value_T, std::size_t SLC_ArraySize>
	auto make_iterable(TermStore<TermUnion_T>& store, TermSLC<SLC_Index_T, SLC_Value_T, SLC_ArraySize>& slc, 
		const UnionToSLC& union_to_slc)
	{
		return SLC_Ref<UnionToSLC, TermUnion_T, SLC_Index_T, SLC_Value_T, SLC_ArraySize>{store, slc, union_to_slc};
	}


	struct TermString128 : TermSLC<std::uint32_t, char, 12>
	{
		static constexpr std::size_t array_size = 12;
		static constexpr std::uint16_t null_index = 0;

		TermString128() {}	//all chars are initialized to '\0' already by TermSLC

		template<typename TermUnion_T>
		TermString128(TermStore<TermUnion_T>& store, std::string_view str)
		{
			if (str.ends_with('\0')) {	//we dont need to open a new node if all that remains is '\0' -> just cut it off
				str.remove_suffix(1);
			}

			if (str.size() > array_size) {
				std::memcpy(&this->values, str.data(), sizeof(char) * array_size);
				this->next_block_idx = store.insert_new(TermString128(store, str.substr(array_size)));
			}
			else {
				std::memcpy(&this->values, str.data(), sizeof(char) * str.size());
				this->next_block_idx = null_index;
			}
		}

		template<typename UnionToSLC, typename TermUnion_T>
		void read(TermStore<TermUnion_T>& store, const UnionToSLC& union_to_slc, std::string& dest) const
		{
			static_assert(std::is_invocable_r_v<TermString128&, UnionToSLC, TermUnion_T&>, "conversion from TermUnion_T to TermString128 must return reference");

			const TermString128* current = this;
			while (current->next_block_idx != null_index) {
				dest.append(current->values, array_size);
				current = &union_to_slc(store.at(current->next_block_idx));
			}
			for (std::size_t i = 0; i < array_size; i++) {
				if (current->values[i] == '\0') [[unlikely]] {
					return;
				}
				dest.push_back(current->values[i]);
			}			
		}

	};	//struct TermString128
	static_assert(sizeof(TermString128) * 8 == 128);
}