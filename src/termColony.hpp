#pragma once

#include <type_traits>
#include <vector>
#include <array>
#include <functional>

#include "termStore.hpp"

namespace bmath::intern {

	//single linked colony is similar to a single linked list, but each node holds a whole array containing ArraySize Value_T,
	//not only a single Value_T (as an ordinary list would). TermSLC directly represents the node, there is no extra head or management.
	//TermSLC is build to reside inside a TermStore, thus it works with Index_T to access later nodes, not with pointers.
	//NullIndex is analogous to nullptr, NullValue is a placeholder to indicate that a position in values is not yet used.
	template <typename TermUnion_T, typename Index_T, Index_T NullIndex, typename Value_T, Value_T NullValue, std::size_t ArraySize>
	struct TermSLC
	{
		static_assert(std::is_trivially_destructible_v<Index_T>);
		static_assert(std::is_trivially_destructible_v<Value_T>);

		Index_T next_block_idx;
		Value_T values[ArraySize];

		TermSLC() :next_block_idx(NullIndex)
		{
			for (int i = 0; i < ArraySize; i++) {
				values[i] = NullValue;
			}
		}

		TermSLC(Value_T elem) :next_block_idx(NullIndex)
		{
			values[0] = elem;
			for (int i = 1; i < ArraySize; i++) {
				values[i] = NullValue;
			}
		}

		//no order is guaranteed
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
				this->next_block_idx = store.emplace_new(TermSLC(elem));
			}
		}

		template<typename UnionToSLC>
		void for_each(TermStore<TermUnion_T>& store, std::function<void(Value_T&)> apply)
		{
			static_assert(std::is_invocable_r_v<TermSLC&, UnionToSLC, TermUnion_T&>, "UnionToSLC should extract the current TermSLC type from Union");

			for (int i = 0; i < ArraySize; i++) {
				if (this->values[i] != NullValue) {
					apply(this->values[i]);
				}
			}
			if (this->next_block_idx != NullIndex) {
				TermSLC& next_block = UnionToSLC(store.at(this->next_block_idx));
				next_block.for_each<UnionToSLC>(store, apply);
			}
		}

		template<typename UnionToSLC>
		void for_each(const TermStore<TermUnion_T>& store, std::function<void(const Value_T&)> apply) const
		{
			static_assert(std::is_invocable_r_v<TermSLC&, UnionToSLC, TermUnion_T&>, "UnionToSLC should extract the current TermSLC type from Union");

			for (int i = 0; i < ArraySize; i++) {
				if (this->values[i] != NullValue) {
					apply(this->values[i]);
				}
			}
			if (this->next_block_idx != NullIndex) {
				TermSLC& next_block = UnionToSLC(store.at(this->next_block_idx));
				next_block.for_each<UnionToSLC>(store, apply);
			}
		}

		template<typename UnionToSLC, typename Apply>
		void for_each(TermStore<TermUnion_T>& store, Apply apply)
		{
			static_assert(std::is_invocable_r_v<TermSLC&, UnionToSLC, TermUnion_T&>, "UnionToSLC should extract the current TermSLC type from Union");
			static_assert(std::is_invocable_r_v<void, Apply, Value_T&>, "apply should be applied to Value_T");

			for (int i = 0; i < ArraySize; i++) {
				if (this->values[i] != NullValue) {
					apply(this->values[i]);
				}
			}
			if (this->next_block_idx != NullIndex) {
				TermSLC& next_block = UnionToSLC(store.at(this->next_block_idx));
				next_block.for_each<UnionToSLC>(store, apply);
			}
		}

		template<typename UnionToSLC, typename Apply>
		void for_each(const TermStore<TermUnion_T>& store, Apply apply) const
		{
			static_assert(std::is_invocable_r_v<TermSLC&, UnionToSLC, TermUnion_T&>, "UnionToSLC should extract the current TermSLC type from Union");
			static_assert(std::is_invocable_r_v<void, Apply, const Value_T&>, "apply should be applied to Value_T");

			for (int i = 0; i < ArraySize; i++) {
				if (this->values[i] != NullValue) {
					apply(this->values[i]);
				}
			}
			if (this->next_block_idx != NullIndex) {
				TermSLC& next_block = UnionToSLC(store.at(this->next_block_idx));
				next_block.for_each<UnionToSLC>(store, apply);
			}
		}
	};

}