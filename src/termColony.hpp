#pragma once

#include <type_traits>
#include <vector>
#include <array>
#include <functional>
#include <limits>

#include "termStore.hpp"

namespace bmath::intern {

	//helper to allow HoldsIndex_T of TermSLC to be unsigned int types
	template<typename UnsignedInt_T>
	[[nodiscard]] std::size_t get_index(UnsignedInt_T val) noexcept
	{
		static_assert(std::is_unsigned_v<UnsignedInt_T>);
		return val;
	}

	//helper to allow HoldsIndex_T of TermSLC to be unsigned int types
	template<typename UnsignedInt_T>
	void set_index(UnsignedInt_T& val, std::size_t new_index)
	{
		static_assert(std::is_unsigned_v<UnsignedInt_T>);
		//if (new_index > static_cast<std::size_t>(std::numeric_limits<UnsignedInt_T>::max)) [[unlikely]] {
		//	throw std::exception("set_index recieved index bigger than maximum storable in UnsignedInt_T");
		//}
		val = new_index;
	}


	//single linked colony is similar to a single linked list, but each node holds a whole array containing ArraySize Value_T,
	//not only a single Value_T (as an ordinary list would). TermSLC directly represents the node, there is no extra head or management.
	//TermSLC is build to reside inside a TermStore, thus it works with HoldsIndex_T to access later nodes, not with pointers.
	//NullIndex is analogous to nullptr, NullValue is a placeholder to indicate that a position in values is not yet used.
	template <typename HoldsIndex_T, HoldsIndex_T NullIndex, typename Value_T, Value_T NullValue, std::size_t ArraySize>
	struct TermSLC
	{
		static_assert(std::is_trivially_destructible_v<HoldsIndex_T>);
		static_assert(std::is_trivially_destructible_v<Value_T>);

		HoldsIndex_T next_block_data;
		Value_T values[ArraySize];

		TermSLC() :next_block_data(NullIndex)
		{
			for (std::size_t i = 0; i < ArraySize; i++) {
				values[i] = NullValue;
			}
		}

		TermSLC(Value_T elem) :next_block_data(NullIndex)
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

			if (this->next_block_data != NullIndex) {
				TermSLC& next_block = store.at(get_index(this->next_block_data));
				next_block.insert_new(store, elem);
			}
			else {
				set_index(this->next_block_data, store.insert_new(TermSLC(elem)));
			}
		}

		template<typename TermUnion_T, typename Apply, typename UnionToSLC>
		void for_each(TermStore<TermUnion_T>& store, const Apply& apply, const UnionToSLC& union_to_slc)
		{
			for (std::size_t i = 0; i < ArraySize; i++) {
				if (this->values[i] != NullValue) {
					apply(this->values[i]);
				}
			}
			if (this->next_block_data != NullIndex) {
				TermSLC& next_block = union_to_slc(store.at(get_index(this->next_block_data)));
				next_block.for_each<UnionToSLC, Apply, TermUnion_T>(store, apply);
			}
		}

		template<typename TermUnion_T, typename Apply, typename UnionToSLC>
		void for_each(const TermStore<TermUnion_T>& store, const Apply& apply, const UnionToSLC& union_to_slc) const 
		{
			for (std::size_t i = 0; i < ArraySize; i++) {
				if (this->values[i] != NullValue) {
					apply(this->values[i]);
				}
			}
			if (this->next_block_data != NullIndex) {
				TermSLC& next_block = union_to_slc(store.at(get_index(this->next_block_data)));
				next_block.for_each<UnionToSLC, Apply, TermUnion_T>(store, apply);
			}
		}

	};	//struct TermSLC 





	struct TermString128 : TermSLC<std::uint16_t, 0, char, '\0', 14>
	{
		static constexpr std::size_t array_size = 14;
		static constexpr std::uint16_t null_index = 0;

		TermString128() :TermSLC<std::uint16_t, 0, char, '\0', 14>() {}	//all chars are initialized to '\0' already by TermSLC

		template<typename TermUnion_T>
		TermString128(TermStore<TermUnion_T>& store, std::string_view str)
		{
			if (str.size() > array_size) {
				std::memcpy(&this->values, str.data(), sizeof(char) * array_size);
				set_index(this->next_block_data, store.insert_new(TermString128(store, str.substr(array_size))));
			}
			else {
				std::memcpy(&this->values, str.data(), sizeof(char) * str.size());
				this->next_block_data = null_index;
			}
		}

		template<typename UnionToSLC, typename TermUnion_T>
		void read(TermStore<TermUnion_T>& store, std::string& dest, const UnionToSLC& union_to_slc) const noexcept
		{
			if (this->next_block_data != null_index) {
				dest.append(this->values, array_size);
				TermString128& next_block = union_to_slc(store.at(get_index(this->next_block_data)));
				next_block.read<UnionToSLC, TermUnion_T>(store, dest, union_to_slc);
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