
#pragma once

#include <cstdint>
#include <complex>
#include <cassert>
#include <algorithm>
#include <type_traits>
#include <limits>
#include <string_view>

#include "termStore.hpp"

namespace bmath::intern {

	//unlike containers like std::vector and the like, there is no proxy managing data on the heap here.
	//all management lies directly with the data.
	//as C++ does not allow structs of size only known at runtime, it is not possible to directly represent the idea
	//  in the data structure.
	//what is done instead, is to allocate an array of StoredVector, where only the Info part of the first one is 
	//  used, and all succeeding ones just act as an elongation of StoredVector::data of the first.
	template<typename Value_T, std::size_t AllocNodeSize = sizeof(std::complex<double>)>
	struct StoredVector
	{
	public:
		static_assert(std::is_trivially_destructible_v<Value_T>);
		static_assert(std::is_trivially_copyable_v<Value_T>);

		struct Info
		{
			std::uint16_t size = 0u;
			std::uint16_t capacity = min_capacity;
		}; 

		static constexpr std::size_t min_capacity = (AllocNodeSize - sizeof(Info)) / sizeof(Value_T);
		static constexpr std::size_t values_per_node = AllocNodeSize / sizeof(Value_T);
		static constexpr std::size_t values_per_info = values_per_node - min_capacity;
		static_assert(values_per_node > 0u, "AllocNodeSize may at least be sizeof(Value_T)");

	private:
		//note: subsequent_data_part is never read directly, only as an offset of StoredVector::data pointing in there.
		//this union mainly serves illustration purposes
		union
		{
			Info info; //active in first StoredVector object of array
			Value_T subsequent_data_part[values_per_info]; //active in all but first object in StoredVector array
		};

		Value_T data_[min_capacity];

	public:
		constexpr std::size_t capacity() const noexcept { return this->info.capacity; }
		constexpr std::size_t size() const noexcept { return this->info.size; }
		constexpr auto& size() noexcept { return this->info.size; }

		constexpr Value_T* data() noexcept { return this->data_; }
		constexpr const Value_T* data() const noexcept { return this->data_; }

		constexpr StoredVector(std::initializer_list<Value_T> init) noexcept 
			:info{ .size = static_cast<decltype(Info::size)>(init.size()), .capacity = min_capacity }
		{
			assert(init.size() <= min_capacity);
			std::copy(init.begin(), init.end(), this->data_);
		}

		constexpr StoredVector(const std::uint16_t new_size, const std::uint16_t new_capacity) noexcept 
			:info{ .size = new_size, .capacity = new_capacity }
		{}

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

	namespace stored_vector {

		struct SaveEndIndicator { std::uint32_t idx; };

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
				return static_cast<StoredVector_T&>(this->store.at(this->store_idx)).data() + this->array_idx; 
			}

			static constexpr bool valid_interaction(const SaveIterator& fst, const SaveIterator& snd) noexcept 
			{
				return (&fst.store == &snd.store) && (fst.store_idx == snd.store_idx);
			}

		public:
			Store_T& store;
			std::uint32_t store_idx;
			std::uint32_t array_idx;

			//http://www.cplusplus.com/reference/iterator/RandomAccessIterator/
			using value_type      = Value_T;
			using difference_type = std::ptrdiff_t;	
			using pointer         = Value_T*;
			using reference       = Value_T&;
			using iterator_category = std::contiguous_iterator_tag;

			constexpr SaveIterator& operator++() noexcept { ++this->array_idx; return *this; }  
			constexpr SaveIterator operator++(int) noexcept { auto result = *this; ++(*this); return result; }
			constexpr SaveIterator& operator+=(Diff_T n) noexcept { this->array_idx += n; return *this; }  
			constexpr SaveIterator operator+(Diff_T n) const noexcept { auto result = *this; result += n; return result; }

			constexpr SaveIterator operator+(const SaveIterator& snd) const noexcept 
			{ 
				assert(valid_interaction(*this, snd));
				return SaveIterator{ this->store, this->store_idx, this->array_idx + snd.array_idx };
			}

			constexpr SaveIterator& operator--() noexcept { --this->array_idx; return *this; }  
			constexpr SaveIterator operator--(int) noexcept { auto result = *this; --(*this); return result; }
			constexpr SaveIterator& operator-=(Diff_T n) noexcept { this->array_idx -= n; return *this; }  
			constexpr SaveIterator operator-(Diff_T n) const noexcept { auto result = *this; result -= n; return result; }

			constexpr SaveIterator operator-(const SaveIterator& snd) noexcept 
			{ 
				assert(valid_interaction(*this, snd) && this->array_idx >= snd.array_idx);
				return SaveIterator{ this->store, this->store_idx, this->array_idx - snd.array_idx };
			}

			constexpr const Value_T& operator*()  const noexcept { return *this->raw_pointer(); }
			constexpr const Value_T* operator->() const noexcept { return this->raw_pointer(); }
			constexpr const Value_T& operator[](Diff_T n) const noexcept { return *(this->raw_pointer() + n); }
			constexpr Value_T& operator*()  noexcept { return *this->raw_pointer(); }
			constexpr Value_T* operator->() noexcept { return this->raw_pointer(); }
			constexpr Value_T& operator[](Diff_T n) noexcept { return *(this->raw_pointer() + n); }

			constexpr bool operator==(const SaveIterator& snd) const noexcept
			{
				assert(valid_interaction(*this, snd));
				return this->offset == snd.offset;
			}

			constexpr std::strong_ordering operator<=>(const SaveIterator& snd) const noexcept
			{
				assert(valid_interaction(*this, snd));
				return this->offset <=> snd.offset;
			}

			constexpr bool operator==(const SaveEndIndicator& end_) const noexcept { return this->array_idx == end_.idx; }
		}; //struct SaveIterator

	} //namespace stored_vector

	template<typename Value_T, std::size_t AllocNodeSize, StoreLike Store_T> 
	constexpr auto begin(const BasicNodeRef<StoredVector<Value_T, AllocNodeSize>, Store_T>& ref)
	{
		using CV_Value_T = std::conditional_t<std::is_const_v<Store_T>, const Value_T, Value_T>;
		using Iter = stored_vector::SaveIterator<CV_Value_T, AllocNodeSize, Store_T>;
		return Iter{ *ref.store, ref.index, 0u };
	}

	template<typename Value_T, std::size_t AllocNodeSize, StoreLike Store_T>
	constexpr auto end(const BasicNodeRef<StoredVector<Value_T, AllocNodeSize>, Store_T>& ref)
	{
		return stored_vector::SaveEndIndicator{ (std::uint32_t) ref->size() };
	}

} //namespace bmath::intern
