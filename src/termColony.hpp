#pragma once

#include <type_traits>
#include <vector>
#include <array>
#include <functional>
#include <limits>

#include "termStore.hpp"

namespace bmath::intern {

	//single linked colony is similar to a single linked list, but each node holds a whole array containing ArraySize Value_T,
	//not only a single Value_T (as an ordinary list would). TermSLC directly represents the node, there is no extra head or management.
	//TermSLC is build to reside inside a TermStore, thus it works with Index_T to access later nodes, not with pointers.
	//NullIndex is analogous to nullptr, NullValue is a placeholder to indicate that a position in values is not yet used.
	template <typename Index_T, Index_T NullIndex, typename Value_T, /*Value_T*/ unsigned int NullValue, std::size_t ArraySize>
	struct TermSLC
	{
		static_assert(std::is_unsigned_v<Index_T>);
		static_assert(std::is_trivially_destructible_v<Value_T>);

		Index_T next_block_idx;
		Value_T values[ArraySize];

		TermSLC() :next_block_idx(NullIndex)
		{
			for (std::size_t i = 0; i < ArraySize; i++) {
				values[i] = NullValue;
			}
		}

		TermSLC(Value_T elem) :next_block_idx(NullIndex)
		{
			values[0] = elem;
			for (std::size_t i = 1; i < ArraySize; i++) {
				values[i] = NullValue;
			}
		}

		//no order is guaranteed
		template<typename TermUnion_T>
		void insert_new(TermStore<TermUnion_T>& store, Value_T elem)
		{
			for (std::size_t i = 0; i < ArraySize; i++) {
				if (this->values[i] != NullValue) {
					this->values[i] = elem;
					return;
				}
			}

			if (this->next_block_idx != NullIndex) {
				TermSLC& next_block = store.at(this->next_block_idx);
				next_block.insert_new(store, elem);
			}
			else {
				this->next_block_idx = store.insert_new(TermSLC(elem));
			}
		}

		template<typename TermUnion_T, typename UnionToSLC, typename Apply>
		void for_each(TermStore<TermUnion_T>& store, const UnionToSLC& union_to_slc, const Apply& apply) 
		{
			static_assert(std::is_invocable_r_v<TermSLC&, UnionToSLC, TermUnion_T&>, "conversion from TermUnion_T to TermSLC must return reference");

			for (std::size_t i = 0; i < ArraySize; i++) {
				if (this->values[i] != NullValue) [[likely]] {
					apply(this->values[i]);
				}
			}
			if (this->next_block_idx != NullIndex) {
				TermSLC& next_block = union_to_slc(store.at(this->next_block_idx));
				next_block.for_each<TermUnion_T, UnionToSLC, Apply>(store, union_to_slc, apply);
			}
		}

		template<typename TermUnion_T, typename UnionToSLC, typename Apply>
		void for_each(const TermStore<TermUnion_T>& store, const UnionToSLC& union_to_slc, const Apply& apply) const 
		{
			static_assert(std::is_invocable_r_v<const TermSLC&, UnionToSLC, const TermUnion_T&>, "conversion from TermUnion_T to TermSLC must return reference");

			for (std::size_t i = 0; i < ArraySize; i++) {
				if (this->values[i] != NullValue) [[likely]] {
					apply(this->values[i]);
				}
			}
			if (this->next_block_idx != NullIndex) {
				const TermSLC& next_block = union_to_slc(store.at(this->next_block_idx));
				next_block.for_each<TermUnion_T, UnionToSLC, Apply>(store, union_to_slc, apply);
			}
		}

	};	//struct TermSLC 



	struct TermString128 : TermSLC<std::uint32_t, 0, char, '\0', 12>
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
		void read(TermStore<TermUnion_T>& store, const UnionToSLC& union_to_slc, std::string& dest) const noexcept
		{
			static_assert(std::is_invocable_r_v<TermString128&, UnionToSLC, TermUnion_T&>, "conversion from TermUnion_T to TermString128 must return reference");

			if (this->next_block_idx != null_index) {
				dest.append(this->values, array_size);
				const TermString128& next_block = union_to_slc(store.at(this->next_block_idx));
				next_block.read<UnionToSLC, TermUnion_T>(store, union_to_slc, dest);
			}
			else {
				for (std::size_t i = 0; i < array_size; i++) {
					if (this->values[i] == '\0') [[unlikely]] {
						return;
					}
					dest.push_back(this->values[i]);
				}
			}
		}

	};	//struct TermString128
	static_assert(sizeof(TermString128) * 8 == 128);
}