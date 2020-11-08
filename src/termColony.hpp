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
	//TermSLC is build to reside inside a BasicStore, thus it works with std::uint32_t to access later nodes, not with pointers.
	//as (as of writing this anyway) a unit in TermStore has a size of 128 bits, the default ArraSize fills that space.
	template <typename Value_T, std::size_t ArraySize = (128u / 8u - sizeof(uint32_t)) / sizeof(Value_T)>
	struct [[nodiscard]] TermSLC
	{
		static_assert(std::is_trivially_destructible_v<Value_T>);
		static_assert(std::is_trivially_copyable_v<Value_T>);

		static constexpr std::uint32_t null_index = 0u;	//assumed to hold a value invalid as an actual index in a BasicStore
		static constexpr Value_T null_value = Value_T();
		static constexpr std::size_t array_size = ArraySize; //make visible to outside

		std::uint32_t next_idx;	//index of next SLC block in Store
		Value_T values[ArraySize];

		//can only fill up to single block
		constexpr TermSLC(const std::initializer_list<Value_T> new_values = {}) :next_idx(null_index)
        {
			assert(new_values.size() <= ArraySize);
			std::copy(new_values.begin(), new_values.end(), this->values);
			for (std::size_t i = new_values.size(); i < ArraySize; i++) {
				values[i] = null_value;
			}
        }

		template<typename Union_T>
		using SLCRef = BasicNodeRef<Union_T, TermSLC, Const::yes>;

		template<typename Union_T>
		using SLCMutRef = BasicNodeRef<Union_T, TermSLC, Const::no>;

		//returns position of node where insert happened.
		//this allows easy insertion of n elements in O(n), instead of O(n^2)
		template<typename Union_T>
		static std::uint32_t insert_new(BasicStore<Union_T>& store, const std::uint32_t slc_idx, const Value_T elem)
		{
			auto ref = SLCMutRef<Union_T>(store, slc_idx);
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
					const std::size_t new_idx = store.insert(TermSLC({ elem }));
					ref->next_idx = new_idx;
					return new_idx;
				}
			}
		} //insert_new

		//returns position of previously last node
		//this allows easy appending of n slcs of length O(1) in O(n), instead of O(n^2)
		template<typename Union_T>
		[[nodiscard]] static std::size_t append(BasicStore<Union_T>& store, const std::uint32_t this_idx, const std::uint32_t append_idx)
		{
			auto ref = SLCMutRef<Union_T>(store, this_idx);
			while (ref->next_idx != null_index) {
				ref.index = ref->next_idx;
			}
			ref->next_idx = append_idx;
			return ref.index;
		} //append

	};	//struct TermSLC 



	template<typename Union_T, typename Type_T, Const is_const>
	struct Iterator
	{
		using Store_T = std::conditional_t<(bool)is_const, const BasicStore<Union_T>, BasicStore<Union_T>>;
		using SLC_T = std::conditional_t<(bool)is_const, const TermSLC<BasicTypedIdx<Type_T>>, TermSLC<BasicTypedIdx<Type_T>>>;
		using Ref_T = BasicNodeRef<Union_T, TermSLC<BasicTypedIdx<Type_T>>, is_const>;

		using value_type      = BasicTypedIdx<Type_T>;
		using difference_type = void;	//no random access
		using pointer         = std::conditional_t<(bool)is_const, const value_type*, value_type*>;
		using reference       = std::conditional_t<(bool)is_const, const value_type&, value_type&>;
		using iterator_category = std::forward_iterator_tag;

		Ref_T ref;				//.index == 0, if whole object represents end()
		std::uint32_t array_idx;		// == SLC::array_size, if whole object represents end()

		constexpr Iterator(const Ref_T& new_ref, std::uint32_t new_array_idx) :ref(new_ref), array_idx(new_array_idx) {}

		constexpr Iterator& operator=(const Iterator& other)
		{
			assert(this->ref.store == other.ref.store && "only interaction between iterators in same term makes sense");
			this->ref.index = other.ref.index;
			this->array_idx = other.array_idx;
			return *this;
		}

		constexpr void unchecked_increment()
		{
			if (++this->array_idx == SLC_T::array_size) [[unlikely]] {
				if (this->ref->next_idx != SLC_T::null_index) {
					this->ref.index = this->ref->next_idx;
					this->array_idx = 0;
				}
				else { //mark this as end (array_idx already is equal to SLC_T::array_size)
					this->ref.index = 0; 
				}
			}
		}

		constexpr Iterator& operator++()
		{
			do {
				if (++this->array_idx == SLC_T::array_size) [[unlikely]] {
					if (this->ref->next_idx != SLC_T::null_index) {
						this->ref.index = this->ref->next_idx;
						this->array_idx = 0;
					}
					else { //mark this as end (array_idx already is equal to SLC_T::array_size)
						this->ref.index = 0; 
						return *this;
					}
				}
			} while (this->ref->values[this->array_idx] == SLC_T::null_value);
			return *this;
		}

		constexpr Iterator operator++(int)
		{
			Iterator result = *this;
			this->operator++();
			return result;
		}

		constexpr [[nodiscard]] auto& operator*()
		{ 
			return this->ref->values[this->array_idx]; 
		}

		constexpr [[nodiscard]] auto* operator->()
		{ 
			return &this->ref->values[this->array_idx]; 
		}

		constexpr bool operator==(const Iterator& other) const noexcept
		{
			assert(this->ref.store == other.ref.store && "only comparison between iterators in same term makes sense");
			return this->array_idx == other.array_idx && this->ref.index == other.ref.index;
		}
	};	//struct Iterator


	template<typename Union_T, typename Type_T, Const is_const>
	constexpr auto unchecked_begin(const BasicNodeRef<Union_T, TermSLC<BasicTypedIdx<Type_T>>, is_const>& ref)
	{
		using Iter = Iterator<Union_T, Type_T, is_const>;
		return Iter(ref, 0u);
	}

	template<typename Union_T, typename Type_T, Const is_const>
	constexpr auto begin(const BasicNodeRef<Union_T, TermSLC<BasicTypedIdx<Type_T>>, is_const>& ref)
	{
		using SLC_T = TermSLC<BasicTypedIdx<Type_T>>;
		using Iter = Iterator<Union_T, Type_T, is_const>;
		Iter iter = unchecked_begin(ref);
		if (iter.ref->values[0] == SLC_T::null_value) [[unlikely]] {
			++iter; //operator++ guarantees to eighter return end or a valid position
		}
		return iter; 
	}

	template<typename Union_T, typename Type_T, Const is_const>
	constexpr auto end(const BasicNodeRef<Union_T, TermSLC<BasicTypedIdx<Type_T>>, is_const>& ref)
	{
		using SLC_T = TermSLC<BasicTypedIdx<Type_T>>;
		using Iter = Iterator<Union_T, Type_T, is_const>;
		return Iter({ *ref.store, 0u }, SLC_T::array_size);
	}

	template<typename Union_T, typename Type_T, typename Compare>
	static void sort_slc(BasicNodeRef<Union_T, TermSLC<BasicTypedIdx<Type_T>>, Const::no> range, Compare compare) noexcept
	{
		using SLC_T = TermSLC<BasicTypedIdx<Type_T>>;

		StupidBufferVector<BasicTypedIdx<Type_T>, 16> all_values;
		//std::vector<BasicTypedIdx<Type_T>> all_values;
		for (const auto elem : range) {
			all_values.push_back(elem);
		}
		std::sort(all_values.begin(), all_values.end(), compare);

		auto iter = unchecked_begin(range); //also expose empty slots
		for (std::size_t i = 0; i < all_values.size(); i++) { //copy sorted elements back (leave no gaps)
			*iter = all_values[i];
			iter.unchecked_increment(); //also expose empty slots
		}

		//clean up possible rest space (if SLC had gaps bevore sorting)
		if (iter != end(range)) {
			while (iter.array_idx < SLC_T::array_size) {
				iter.ref->values[iter.array_idx++] = SLC_T::null_value;
			}
			free_slc(range.new_at(iter.ref->next_idx));
			iter.ref->next_idx = SLC_T::null_index;
		}
	} //sort_slc






	struct StringSLC
	{
		static constexpr std::size_t null_index = 0;
		static constexpr std::size_t array_size = 12u; //chosen to bring Node size to 128 bit
		std::uint32_t next_idx = null_index;
		char data[array_size] = {};

		//can only fill up to single block
		constexpr StringSLC(const std::string_view new_values, const std::uint32_t next_block) noexcept :next_idx(next_block)
		{
			assert(new_values.size() <= array_size);
			std::copy(new_values.begin(), new_values.end(), this->data);
		}
	};
	static_assert(sizeof(StringSLC) * 8 == 128);


	namespace str_slc {

		template<typename Union_T>
		using StrRef = BasicNodeRef<Union_T, StringSLC, Const::yes>;

		template<typename Union_T>
		using StrMutRef = BasicNodeRef<Union_T, StringSLC, Const::no>;


		template<typename Union_T>
		[[nodiscard]] std::size_t insert(BasicStore<Union_T>& store, std::string_view str)
		{
			assert(str.length());
			std::uint32_t prev_inserted_at = StringSLC::null_index;
			{
				const std::size_t last_substr_length = str.length() % StringSLC::array_size;
				if (last_substr_length > 0) {
					const std::string_view last_view = str.substr(str.length() - last_substr_length);
					prev_inserted_at = store.insert(StringSLC(last_view, prev_inserted_at));
					str.remove_suffix(last_substr_length);
				}
			}
			assert((str.length() % StringSLC::array_size == 0) && "last shorter bit should have been cut off already");
			while (str.length()) {
				const std::string_view last_view = str.substr(str.length() - StringSLC::array_size);
				prev_inserted_at = store.insert(StringSLC(last_view, prev_inserted_at));
				str.remove_suffix(StringSLC::array_size);
			}
			return prev_inserted_at;
		} //insert_string

		template<typename Union_T>
		void read(StrRef<Union_T> ref, std::string& dest)
		{
			while (ref->next_idx != StringSLC::null_index) {
				dest.append(ref->data, StringSLC::array_size);
				ref.index = ref->next_idx;
			}
			for (std::size_t i = 0u; i < StringSLC::array_size; i++) {
				if (ref->data[i] == '\0') [[unlikely]] {
					return;
				}
				dest.push_back(ref->data[i]);
			}			
		} //read

		template<typename Union_T1, typename Union_T2>
		[[nodiscard]] std::strong_ordering compare(
			StrRef<Union_T1> ref_1, StrRef<Union_T2> ref_2)
		{
			while (ref_1->next_idx != StringSLC::null_index && ref_2->next_idx != StringSLC::null_index) 
			{
				static_assert(('a' <=> 'a') == std::strong_ordering::equal); //dont wanna mix with std::strong_ordering::equivalent
				const auto cmp = compare_arrays(ref_1->data, ref_2->data, StringSLC::array_size);
				if (cmp != std::strong_ordering::equal) {
					return cmp;
				}
				else {
					ref_1.index = ref_1->next_idx;
					ref_2.index = ref_2->next_idx;
				}
			}
			if (ref_1->next_idx == StringSLC::null_index &&
				ref_2->next_idx == StringSLC::null_index) 
			{
				return compare_arrays(ref_1->data, ref_2->data, StringSLC::array_size);
			}
			else {
				return ref_1->next_idx == StringSLC::null_index ?
					std::strong_ordering::less : 
					std::strong_ordering::greater;
			}
		} //string_compare

		template<typename Union_T>
		[[nodiscard]] std::strong_ordering compare(StrRef<Union_T> ref, std::string_view view)
		{
			while (view.size() > StringSLC::array_size && ref->next_idx != StringSLC::null_index) {
				const auto cmp = compare_arrays(ref->data, view.data(), StringSLC::array_size);
				if (cmp != std::strong_ordering::equal) {
					return cmp;
				}
				else {
					ref.index = ref->next_idx;
					view.remove_prefix(StringSLC::array_size);
				}
			}
			if (view.size() <= StringSLC::array_size && ref->next_idx == StringSLC::null_index) {
				const std::size_t shorter_length = std::min(StringSLC::array_size, view.size());
				return compare_arrays(ref->data, view.data(), shorter_length);
			}
			else {
				return view.size() <= StringSLC::array_size ?
					std::strong_ordering::less : 
					std::strong_ordering::greater;
			}
		} //string_compare

	} //namespace str_slc


	//also allows SLC_T::null_index to be passed in
	template<typename Union_T, typename SLC_T>
	void free_slc(BasicNodeRef<Union_T, SLC_T, Const::no> ref)
	{
		std::uint32_t last_idx;
		while (ref.index != StringSLC::null_index) {
			last_idx = ref.index;
			ref.index = ref->next_idx;
			ref.store->free(last_idx);
		};
	} //free_slc

} //namespace bmath::intern