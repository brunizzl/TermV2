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
	//TermSLC is build to reside inside a BasicStore, thus it works with TypedIdx_T to access later nodes, not with pointers.
	template <typename Index_T, typename Value_T, std::size_t ArraySize>
	struct [[nodiscard]] TermSLC
	{
		static_assert(std::is_unsigned_v<Index_T>);
		static_assert(std::is_trivially_destructible_v<Value_T>);
		static_assert(std::is_trivially_copyable_v<Value_T>);

		static constexpr Index_T null_index = Index_T();	//assumed to hold a value invalid as an actual index in a BasicStore
		static constexpr Value_T null_value = Value_T();
		static constexpr std::size_t array_size = ArraySize; //make visible to outside

		Index_T next_idx;	//index of next SLC block in BasicStore
		Value_T values[ArraySize];

		constexpr TermSLC(const Value_T fst_elem = null_value) noexcept :next_idx(null_index)
		{
			this->values[0] = fst_elem;
			for (std::size_t i = 1; i < ArraySize; i++) {
				this->values[i] = null_value;
			}
		}

		//can only fill up to single block
		constexpr TermSLC(const std::span<const Value_T> new_values, const Index_T next_block) :next_idx(next_block)
		{
			assert(new_values.size() <= ArraySize);
			std::copy(new_values.begin(), new_values.end(), this->values);
			for (std::size_t i = new_values.size(); i < ArraySize; i++) {
				values[i] = null_value;
			}
		}

		//can only fill up to single block
		constexpr TermSLC(std::initializer_list<Value_T> new_values) :next_idx(null_index)
        {
			assert(new_values.size() <= ArraySize);
			std::copy(new_values.begin(), new_values.end(), this->values);
			for (std::size_t i = new_values.size(); i < ArraySize; i++) {
				values[i] = null_value;
			}
        }

		//also allows SLC::null_index to be passed in
		template<typename Union_T>
		static void free_slc(BasicStore<Union_T>& store, const Index_T slc_idx)
		{
			SLCRef<Union_T> ref = SLCRef<Union_T>(store, slc_idx);
			Index_T last_idx;
			while (ref.index != null_index) {
				last_idx = ref.index;
				ref.index = ref->next_idx;
				store.free(last_idx);
			};
		} //free_slc

		//returns position of node where insert happened.
		//this allows easy insertion of n elements in O(n), instead of O(n^2)
		template<typename Union_T>
		static std::uint32_t insert_new(BasicStore<Union_T>& store, const std::uint32_t slc_idx, Value_T elem)
		{
			auto ref = SLCRef<Union_T>(store, slc_idx);
			while (true) {
				for (std::size_t i = 0; i < array_size; i++) {
					if (ref->values[i] == null_value) {
						ref->values[i] = elem;
						return ref.index;
					}
				}
				if (ref->next_idx != null_index) {
					ref.index = ref->next_idx;
				}
				else {
					const std::size_t new_idx = store.insert(TermSLC(elem));
					ref->next_idx = new_idx;
					return new_idx;
				}
			}
		} //insert_new

		//returns position of previously last node
		//this allows easy appending of n slcs of length O(1) in O(n), instead of O(n^2)
		template<typename Union_T>
		[[nodiscard]] static std::size_t append(BasicStore<Union_T>& store, const Index_T this_idx, const Index_T append_idx)
		{
			static_assert(!std::is_same_v<TermSLC, TermString128>, "append is not meant for strings");
			auto ref = SLCRef<Union_T>(store, this_idx);
			while (ref->next_idx != null_index) {
				ref.index = ref->next_idx;
			}
			ref->next_idx = append_idx;
			return ref.index;
		} //append

		//a reference to just a TermSLC has the problem, that the adress of the next block depends not only on next_idx,
		//but also on the Store holding the TermSLC. Thus this store-aware type is used to iterate over any TermSLC.
		template<typename Union_T, bool Const = false>
		struct SLCRef
		{
			using Store_T = std::conditional_t<Const, const BasicStore<Union_T>, BasicStore<Union_T>>;
			using SLC_T = std::conditional_t<Const, const TermSLC, TermSLC>;

			Store_T& store;
			Index_T index;

			SLCRef(Store_T& new_store, Index_T new_slc_idx) :store(new_store), index(new_slc_idx) {}

			struct Iterator
			{
				using value_type      = std::remove_const_t<Value_T>;
				using difference_type = void;	//no random access
				using pointer         = std::conditional_t<Const, const value_type*, value_type*>;
				using reference       = std::conditional_t<Const, const value_type&, value_type&>;
				using iterator_category = std::forward_iterator_tag;

				SLCRef ref;						//.index == 0, if whole object represents end()
				std::uint32_t array_idx;		// == SLC::array_size, if whole object represents end()

				Iterator(SLCRef& new_ref, std::uint32_t new_array_idx) :ref(new_ref), array_idx(new_array_idx) {}

				Iterator(const Iterator& other) :ref(other.ref), array_idx(other.array_idx) {}

				Iterator& operator=(const Iterator& other)
				{
					assert(&this->ref.store == &other.ref.store);
					this->ref.index = other.ref.index;
					this->array_idx = other.array_idx;
					return *this;
				}

				enum class IncrementEffect { same_node, new_node, end };

				IncrementEffect unchecked_increment()
				{
					if (++this->array_idx == SLC_T::array_size) [[unlikely]] {
						if (this->ref->next_idx == SLC_T::null_index) [[unlikely]] {
							this->ref.index = 0; //mark this as end
							return IncrementEffect::end;
						}
						else {
							this->ref.index = ref->next_idx;
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
						if (this->ref->values[this->array_idx] != SLC_T::null_value) [[likely]] {
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
					return this->ref->values[this->array_idx]; 
				}

				bool operator==(const Iterator& other) const noexcept
				{
					assert(&this->ref.store == &other.ref.store && "only comparison between iterators in same term makes sense");
					return this->array_idx == other.array_idx && this->ref.index == other.ref.index;
				}
			};	//struct Iterator

			//not meant to normally iterate through values, as also null_value might occur at begin
			Iterator unchecked_begin() noexcept
			{
				return Iterator(*this, 0);
			}

			Iterator begin() 
			{ 
				auto iter = this->unchecked_begin();
				if (iter.ref->values[0] == null_value) [[unlikely]] {
					++iter; //operator++ guarantees to eighter return end or a valid position
				}
				return iter; 
			}

			Iterator end() noexcept { return Iterator(SLCRef(this->store, 0u), array_size); }

			SLC_T* operator->() { return &static_cast<SLC_T&>(store.at(index)); }
			const TermSLC* operator->() const { return &static_cast<const TermSLC&>(store.at(index)); }
		}; //struct SLCRef

		template<typename Union_T, typename Type_T, typename Compare>
		static void sort(const BasicMutRef<Union_T, Type_T> ref, Compare compare) noexcept
		{
			StupidBufferVector<Value_T, 16> all_values;
			SLCRef<Union_T> range = SLCRef<Union_T>(*ref.store, ref.index);
			for (const Value_T elem : range) {
				all_values.push_back(elem);
			}
			std::sort(all_values.begin(), all_values.end(), compare);

			auto iter = range.unchecked_begin(); //also expose empty slots
			for (std::size_t i = 0; i < all_values.size(); i++) { //copy sorted elements back (leave no gaps)
				*iter = all_values[i];
				iter.unchecked_increment(); //also expose empty slots
			}

			//clean up possible rest space (if SLC had gaps bevore sorting)
			if (iter != range.end()) {
				while (iter.array_idx < array_size) {
					iter.ref->values[iter.array_idx++] = null_value;
				}
				free_slc(*ref.store, iter.ref->next_idx);
				iter.ref->next_idx = null_index;
			}
		} //sort

		//O(n) operation, use with care.
		template<typename Union_T, typename Type_T>
		static std::size_t slow_size(const BasicRef<Union_T, Type_T> ref)
		{
			std::size_t result = 0;
			for (auto&& : SLCRef<Union_T, true>(ref.store, ref.index)) {
				result++;
			}
			return result;
		} //slow_size

	};	//struct TermSLC 


	using TermString128 = TermSLC<std::uint32_t, char, 12>;
	static_assert(sizeof(TermString128) * 8 == 128);

	template<typename Union_T>
	[[nodiscard]] std::size_t insert_string(BasicStore<Union_T>& store, std::string_view str)
	{
		std::uint32_t prev_inserted_at = TermString128::null_index;
		{
			const std::size_t last_substr_length = str.length() % TermString128::array_size;
			if (last_substr_length > 0) {
				const std::size_t last_substr_begin = str.length() - last_substr_length;
				const std::span<const char> last_view(str.data() + last_substr_begin, last_substr_length);
				prev_inserted_at = store.insert(TermString128(last_view, prev_inserted_at));
				str.remove_suffix(last_substr_length);
			}
		}
		assert((str.length() % TermString128::array_size == 0) && "last shorter bit should have been cut off already");
		while (str.length()) {
			const char *const last_view_begin = str.data() + str.length() - TermString128::array_size;
			const std::span<const char> last_view(last_view_begin, TermString128::array_size);
			prev_inserted_at = store.insert(TermString128(last_view, prev_inserted_at));
			str.remove_suffix(TermString128::array_size);
		}
		return prev_inserted_at;
	} //insert_string

	template<typename Union_T, typename Type_T>
	void read(const BasicRef<Union_T, Type_T> in_ref, std::string& dest)
	{
		auto ref = TermString128::SLCRef<Union_T, true>(*in_ref.store, in_ref.index);
		while (ref->next_idx != TermString128::null_index) {
			dest.append(ref->values, TermString128::array_size);
			ref.index = ref->next_idx;
		}
		for (std::size_t i = 0u; i < TermString128::array_size; i++) {
			if (ref->values[i] == '\0') [[unlikely]] {
				return;
			}
			dest.push_back(ref->values[i]);
		}			
	} //read

	template<typename Union_T1, typename Type_T1, typename Union_T2, typename Type_T2>
	[[nodiscard]] std::strong_ordering string_compare(
		const BasicRef<Union_T1, Type_T1> in_ref_1, const BasicRef<Union_T2, Type_T2> in_ref_2)
	{
		auto ref_1 = TermString128::SLCRef<Union_T1, true>(*in_ref_1.store, in_ref_1.index);
		auto ref_2 = TermString128::SLCRef<Union_T2, true>(*in_ref_2.store, in_ref_2.index);
		while (ref_1->next_idx != TermString128::null_index &&
			ref_2->next_idx != TermString128::null_index) 
		{
			static_assert(('a' <=> 'a') == std::strong_ordering::equal); //dont wanna mix with std::strong_ordering::equivalent
			const auto cmp = compare_arrays(ref_1->values, ref_2->values, TermString128::array_size);
			if (cmp != std::strong_ordering::equal) {
				return cmp;
			}
			else {
				ref_1.index = ref_1->next_idx;
				ref_2.index = ref_2->next_idx;
			}
		}
		if (ref_1->next_idx == TermString128::null_index &&
			ref_2->next_idx == TermString128::null_index) 
		{
			return compare_arrays(ref_1->values, ref_2->values, TermString128::array_size);
		}
		else {
			return ref_1->next_idx == TermString128::null_index ?
				std::strong_ordering::less : 
				std::strong_ordering::greater;
		}
	}

	template<typename Union_T, typename Type_T>
	[[nodiscard]] std::strong_ordering string_compare(const BasicRef<Union_T, Type_T> in_ref, std::string_view view)
	{
		auto ref = TermString128::SLCRef<Union_T, true>(*in_ref.store, in_ref.index);
		while (view.size() > TermString128::array_size && ref->next_idx != TermString128::null_index) {
			const auto cmp = compare_arrays(ref->values, view.data(), TermString128::array_size);
			if (cmp != std::strong_ordering::equal) {
				return cmp;
			}
			else {
				ref.index = ref->next_idx;
				view.remove_prefix(TermString128::array_size);
			}
		}
		if (view.size() <= TermString128::array_size && ref->next_idx == TermString128::null_index) {
			const std::size_t shorter_length = std::min(TermString128::array_size, view.size());
			return compare_arrays(ref->values, view.data(), shorter_length);
		}
		else {
			return view.size() <= TermString128::array_size ?
				std::strong_ordering::less : 
				std::strong_ordering::greater;
		}
	}

} //namespace bmath::intern