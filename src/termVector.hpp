
#pragma once

#include <cstdint>
#include <complex>
#include <cassert>
#include <algorithm>
#include <type_traits>
#include <limits>
#include <string_view>
#include <bit>

#include "termStore.hpp"
#include "reference.hpp"

namespace simp {

	//unlike containers like std::vector and the like, there is no proxy managing data on the heap here.
	//all management lies directly with the data.
	//as C++ does not allow structs of size only known at runtime, it is not possible to directly represent the idea
	//  in the data structure.
	//what is done instead, is to allocate an array of StoredVector, where only the Info part of the first one is 
	//  used, and all succeeding ones just act as an elongation of StoredVector::data of the first.
	template<typename Value_T, //data stored in array
		std::size_t AllocNodeSize = sizeof(std::complex<double>)> //if some fixed size metadata should be stored with the array, this is the type to wrap it in.		
	struct StoredVector
	{
		static_assert(std::is_trivially_destructible_v<Value_T>);
		static_assert(std::is_trivially_copyable_v<Value_T>);
		static_assert(AllocNodeSize % sizeof(Value_T) == 0);

		struct Info
		{
			std::uint16_t size = 0u;
			std::uint16_t capacity = min_capacity;
		};

		static constexpr std::size_t min_capacity = (AllocNodeSize - sizeof(Info)) / sizeof(Value_T);
		static constexpr std::size_t values_per_node = AllocNodeSize / sizeof(Value_T);
		static constexpr std::size_t values_per_info = values_per_node - min_capacity;

	private:
		//note: subsequent_data_part is never read directly, only as an offset of StoredVector::data pointing in there.
		//this union mainly serves illustration purposes
		union
		{
			Info info; //active in first StoredVector object of array
			Value_T subsequent_data_part[values_per_info]; //active in all but first object in StoredVector array
		};

		Value_T data_[min_capacity];

		constexpr StoredVector(const std::uint16_t new_size, const std::uint16_t new_capacity) noexcept
			:info{ .size = new_size, .capacity = new_capacity }
		{}

	public:
		constexpr std::uint16_t capacity() const noexcept { return this->info.capacity; }
		constexpr std::uint16_t size() const noexcept { return this->info.size; }

		constexpr void shrink_size_to(const std::uint16_t new_) noexcept { assert(this->info.size >= new_); this->info.size = new_; }

		constexpr       Value_T* data()       noexcept { return this->data_; }
		constexpr const Value_T* data() const noexcept { return this->data_; }

		//only intended to be used by SaveIterator
		constexpr       Value_T* unsave_data_no_start_offset()       noexcept { return this->subsequent_data_part; }
		constexpr const Value_T* unsave_data_no_start_offset() const noexcept { return this->subsequent_data_part; }

		constexpr StoredVector(std::initializer_list<Value_T> init) noexcept 
			:info{ .size = static_cast<decltype(Info::size)>(init.size()), .capacity = min_capacity }
		{
			assert(init.size() <= min_capacity);
			std::copy(init.begin(), init.end(), this->data_);
		}

		static constexpr std::size_t _node_count(const std::size_t capacity_) noexcept 
		{	
			assert((capacity_ + values_per_node - min_capacity) % values_per_node == 0u);
			return (capacity_ + values_per_node - min_capacity) / values_per_node;
		}
		constexpr std::size_t node_count() const noexcept { return _node_count(this->info.capacity); }

		static constexpr std::size_t smallest_fit_capacity(const std::size_t size_) noexcept
		{
			const std::size_t supremum = size_ + values_per_node - 1u; //guaranteed to be larger than or equal to result
			const std::size_t overhang = (supremum - min_capacity) % values_per_node; 
			return supremum - overhang;
		}

		template<typename Union_T, typename Init_T>
		static constexpr void emplace(Union_T& at, const Init_T& init, 
			const std::size_t new_capacity) noexcept
		{
			assert(new_capacity <= std::numeric_limits<decltype(Info::capacity)>::max());
			StoredVector& dest_vec = static_cast<StoredVector&>(at);
			new (&dest_vec) StoredVector(init.size(), new_capacity);
			std::copy_n(init.data(), init.size(), +dest_vec.data_);
		}

		template<typename Store_T, typename Init_T>
		static constexpr [[nodiscard]] std::size_t build(Store_T& store, const Init_T& init) noexcept
		{
			const std::size_t alloc_capacity = smallest_fit_capacity(init.size());
			const std::size_t alloc_idx = store.allocate_n(_node_count(alloc_capacity));
			emplace(store.at(alloc_idx), init, alloc_capacity);
			return alloc_idx;
		}

		template<typename Store_T>
		static constexpr void free(Store_T& store, std::size_t index)
		{
			const std::size_t capacity_ = static_cast<const StoredVector&>(store.at(index)).info.capacity;
			store.free_n(index, _node_count(capacity_));
		}

		//these are to be used with caution, as vector might reallocate while in use.
		constexpr Value_T* begin() noexcept { return this->data_; }
		constexpr Value_T* end() noexcept { return this->data_ + this->info.size; }

		//this is constant -> these are (in most circumstances) save to use
		constexpr const Value_T* begin() const noexcept { return this->data_; }
		constexpr const Value_T* end() const noexcept { return this->data_ + this->info.size; }

		constexpr Value_T& front() noexcept { return *this->data_; }
		constexpr Value_T& back() noexcept { return *(this->data_ + this->info.size - 1u); }

		constexpr const Value_T& front() const noexcept { return *this->data_; }
		constexpr const Value_T& back() const noexcept { return *(this->data_ + this->info.size - 1u); }

		//kinda unsave, as always :o
		constexpr Value_T& operator[](const std::uint32_t at) noexcept { return *(this->data_ + at); }
		constexpr const Value_T& operator[](const std::uint32_t at) const noexcept { return *(this->data_ + at); }

		template<std::nullptr_t = nullptr> requires(std::is_same_v<Value_T, char>)
		constexpr operator std::string_view() const noexcept { return { this->data(), this->size() }; }

	}; //struct StoredVector

	namespace detail_vector {

		struct SaveEndIndicator {};

		//this iterator does not hold the position pointed at directly, but always takes the way via the store,
		//  where the StoredVector we walk along is held.
		//The Store is thus allowed to move its data elsewhere during StoredVector traversal.
		template<typename Value_T, std::size_t AllocNodeSize, StoreLike Store_T>
		class SaveIterator
		{
			static_assert(std::is_const_v<Value_T> == std::is_const_v<Store_T>);
			using StoredVector_T = std::conditional_t<std::is_const_v<Store_T>,
				const StoredVector<std::remove_const_t<Value_T>, AllocNodeSize>,
				      StoredVector<std::remove_const_t<Value_T>, AllocNodeSize>
			>;

			using Diff_T = const std::ptrdiff_t;	

			constexpr Value_T* raw_pointer() noexcept
			{
				assert(this->index < this->stop);
				return static_cast<StoredVector_T&>(*this->store.data()).unsave_data_no_start_offset() + this->index;
			}

			static constexpr bool valid_interaction(const SaveIterator& fst, const SaveIterator& snd) noexcept 
			{
				return (&fst.store == &snd.store) && fst.stop == snd.stop;
			}

		public:
			Store_T& store;
			std::int32_t index; //imagines all entries in store to be StoredVector_T, stores offest of current Value_T to store.data()
			std::int32_t stop; //first index counted as above no longer belonging to instance of StoredVector_T

			//http://www.cplusplus.com/reference/iterator/RandomAccessIterator/
			using value_type      = Value_T;
			using difference_type = std::ptrdiff_t;	
			using pointer         = Value_T*;
			using reference       = Value_T&;
			using iterator_category = std::contiguous_iterator_tag;

			static constexpr SaveIterator build(Store_T& store, const std::int32_t store_index, const std::int32_t start_index,
				const std::int32_t stop_index) noexcept
			{
				const std::int32_t vec_start = StoredVector_T::values_per_node * store_index + StoredVector_T::values_per_info;
				return SaveIterator{ store, vec_start + start_index, vec_start + stop_index };
			}

			constexpr SaveIterator& operator++() noexcept { ++this->index; return *this; }
			constexpr SaveIterator operator++(int) noexcept { auto result = *this; ++(*this); return result; }
			constexpr SaveIterator& operator+=(Diff_T n) noexcept { this->index += n; return *this; }
			constexpr SaveIterator operator+(Diff_T n) const noexcept { auto result = *this; result += n; return result; }

			constexpr SaveIterator& operator--() noexcept { --this->index; return *this; }
			constexpr SaveIterator operator--(int) noexcept { auto result = *this; --(*this); return result; }
			constexpr SaveIterator& operator-=(Diff_T n) noexcept { this->index -= n; return *this; }
			constexpr SaveIterator operator-(Diff_T n) const noexcept { auto result = *this; result -= n; return result; }

			constexpr const Value_T& operator*()  const noexcept { return *this->raw_pointer(); }
			constexpr const Value_T* operator->() const noexcept { return this->raw_pointer(); }
			constexpr const Value_T& operator[](Diff_T n) const noexcept { return *(this->raw_pointer() + n); }
			constexpr Value_T& operator*()  noexcept { return *this->raw_pointer(); }
			constexpr Value_T* operator->() noexcept { return this->raw_pointer(); }
			constexpr Value_T& operator[](Diff_T n) noexcept { return *(this->raw_pointer() + n); }

			constexpr bool operator==(const SaveIterator& snd) const noexcept
			{
				assert(valid_interaction(*this, snd));
				return this->index == snd.index;
			}

			constexpr std::strong_ordering operator<=>(const SaveIterator& snd) const noexcept
			{
				assert(valid_interaction(*this, snd));
				return this->index <=> snd.index;
			}

			constexpr bool at_end() const noexcept { return this->index == this->stop; }
			constexpr bool operator==(const SaveEndIndicator&) const noexcept { return this->at_end(); }

		}; //struct SaveIterator

	} //namespace stored_vector

	template<typename Value_T, std::size_t AllocNodeSize, StoreLike Store_T> 
	constexpr auto begin(const BasicNodeRef<StoredVector<Value_T, AllocNodeSize>, Store_T>& ref)
	{
		using CV_Value_T = std::conditional_t<std::is_const_v<Store_T>, const Value_T, Value_T>;
		using Iter = detail_vector::SaveIterator<CV_Value_T, AllocNodeSize, Store_T>;
		return Iter::build(*ref.store, ref.index, 0, ref->size());
	}

	template<typename Value_T, std::size_t AllocNodeSize, StoreLike Store_T>
	constexpr auto end(const BasicNodeRef<StoredVector<Value_T, AllocNodeSize>, Store_T>& ref)
	{
		return detail_vector::SaveEndIndicator{};
	}

} //namespace simp
