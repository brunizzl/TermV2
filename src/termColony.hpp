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
	template <typename Index_T, typename Value_T, std::size_t ArraySize>
	struct TermSLC
	{
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
	template<typename UnionToSLC, typename TermStore_T, typename SLC_T>
	struct SLCRef
	{
		TermStore_T& store;
		SLC_T& slc;

		struct RangeIterator
		{
			static constexpr bool is_const = std::is_const_v<TermStore_T> || std::is_const_v<SLC_T>;
			static_assert(std::is_const_v<TermStore_T> == std::is_const_v<SLC_T>, "there is no reason to have more than an all-constant and all-mutable version");

			using value_type      = std::remove_const_t<decltype(SLC_T::null_value)>;
			using difference_type = void;	//no random access
			using pointer         = std::conditional_t<is_const, const value_type*, value_type*>;
			using reference       = std::conditional_t<is_const, const value_type&, value_type&>;
			using iterator_category = std::forward_iterator_tag;

			TermStore_T& store;
			SLC_T* current;			// == nullptr, if whole object represents end()
			std::size_t array_idx;		// == SLC::array_size, if whole object represents end()

			RangeIterator(TermStore_T& new_store, SLC_T* new_current, std::size_t new_idx)
				:store(new_store), current(new_current), array_idx(new_idx)
			{}

			RangeIterator(const RangeIterator& other)
				:store(other.store), current(other.current), array_idx(other.array_idx)
			{}

			RangeIterator& operator=(const RangeIterator& other)
			{
				this->store = other.store;
				this->current = other.current;
				this->array_idx = other.array_idx;
				return *this;
			}

			RangeIterator& operator++()
			{
				auto& i = this->array_idx;
				i++;
				while (true) {
					for (; i < SLC_T::array_size; i++) {
						if (this->current->values[i] != SLC_T::null_value) [[likely]] {
							return *this;
						}
					}
					if (this->current->next_idx != SLC_T::null_index) { //no valid position -> go to next block
						this->current = &UnionToSLC::apply(this->store.at(current->next_idx));
						i = 0;
					}
					else { //neither valid position, nor valid block -> this becomes end()
						this->current = nullptr;
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
				assert(&this->store == &other.store && "only comparison between iterators in same term makes sense");
				return this->array_idx == other.array_idx && this->current == other.current;
			}

			bool operator!=(const RangeIterator& other) const noexcept { return !(*this == other); }
		};	//struct RangeIterator

		RangeIterator begin() noexcept { return RangeIterator(this->store, &this->slc, 0); }
		RangeIterator end() noexcept { return RangeIterator(this->store, nullptr, SLC_T::array_size); }
	}; //struct SLCRef

	//first template parameter needs to be explicit, and of form:
	// struct UnionToSLC { static SLC_TYPE& apply(TermUnion_T& val) { return val.slc_member; }};
	template<typename UnionToSLC, typename TermStore_T, typename SLC_T>
	auto range(TermStore_T& store, SLC_T& slc)
	{
		static constexpr bool is_const = std::is_const_v<TermStore_T> || std::is_const_v<SLC_T>;	   //ensure that eigther both or none are const
		using Maybe_Const_TermStore_T = std::conditional_t<is_const, const TermStore_T, TermStore_T>;  //ensure that eigther both or none are const
		using Maybe_Const_SLC_T = std::conditional_t<is_const, const SLC_T, SLC_T>;					   //ensure that eigther both or none are const
		return SLCRef<UnionToSLC, Maybe_Const_TermStore_T, Maybe_Const_SLC_T>{ store, slc };
	}

	template<typename UnionToSLC, typename TermStore_T>
	auto range(TermStore_T& store, std::size_t idx)
	{
		auto& slc = UnionToSLC::apply(store.at(idx));
		using SLC_T = std::remove_reference_t<decltype(slc)>;
		return SLCRef<UnionToSLC, TermStore_T, SLC_T>{ store, slc };
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

	template<typename UnionToSLC, typename SLC_T, typename TermStore_T>
	void insert_new(TermStore_T& store, std::size_t slc_idx, decltype(SLC_T::null_value) elem)
	{
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
				const std::size_t new_idx = store.emplace_new(SLC_T(elem));
				UnionToSLC::apply(store.at(slc_idx)).next_idx = new_idx;
				return;
			}
		}
	} //insert_new

	template<typename UnionToSLC, typename TermStore_T, typename SLC_T, typename Compare>
	void sort(TermStore_T& store, SLC_T& slc, Compare compare)
	{
		using SLC_Value_T = std::remove_const_t<decltype(SLC_T::null_value)>;

		if (auto view = range<UnionToSLC>(store, slc); std::is_sorted(view.begin(), view.end(), compare)) {
			return;
		}
		else if (slc.next_idx == SLC_T::null_index) {	//only a single array to sort -> dont need dynamic allocation
			std::span<SLC_Value_T> whole_array(slc.values, SLC_T::array_size);
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
			for (SLC_Value_T& elem : range<UnionToSLC>(store, slc)) {
				all_values.push_back(elem);
			}
			std::sort(all_values.begin(), all_values.end(), compare);

			std::size_t vec_i = 0;
			SLC_T* current = &slc;
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