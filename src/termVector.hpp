
#pragma once

#include <cstdint>
#include <complex>
#include <cassert>
#include <algorithm>
#include <type_traits>

#include "termStore.hpp"

namespace bmath::intern {

	template<typename Value_T, std::size_t AllocNodeSize = sizeof(std::complex<double>)>
	struct StoredVector
	{
		static_assert(std::is_trivially_destructible_v<Value_T>);
		static_assert(std::is_trivially_copyable_v<Value_T>);

		static constexpr std::size_t min_capacity = (AllocNodeSize - 2u * sizeof(std::uint16_t)) / sizeof(Value_T);
		static constexpr std::size_t values_per_node = AllocNodeSize / sizeof(Value_T);
		static_assert(min_capacity > 0u && values_per_node > 0u);

		std::uint16_t size = 0u;
		std::uint16_t capacity = min_capacity;

		Value_T data[min_capacity];

		constexpr StoredVector(const std::uint16_t new_size, const std::uint16_t new_capacity) noexcept 
			:size(new_size), capacity(new_capacity)
		{}

		constexpr StoredVector(std::initializer_list<Value_T> init) noexcept 
			:size(init.size()), capacity(min_capacity)
		{
			assert(init.size() <= min_capacity);
			std::copy(init.begin(), init.end(), this->data);
		}

		static constexpr std::size_t node_count(const std::size_t capacity_) noexcept 
		{	
			ASSERT((capacity_ + values_per_node - min_capacity) % values_per_node == 0u);
			return (capacity_ + values_per_node - min_capacity) / values_per_node;
		}

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
			StoredVector& dest_vec = static_cast<StoredVector&>(at);
			new (&dest_vec) StoredVector(init.size(), new_capacity);
			std::copy_n(init.data(), init.size(), +dest_vec.data);
		}

		template<typename Store_T, typename Init_T>
		static constexpr [[nodiscard]] std::size_t build(Store_T& store, const Init_T& init) noexcept
		{
			const std::size_t alloc_capacity = smallest_fit_capacity(init.size());
			const std::size_t alloc_idx = store.allocate_n(node_count(alloc_capacity));
			emplace(store.at(alloc_idx), init, alloc_capacity);
			return alloc_idx;
		}

		template<typename Store_T>
		static constexpr void free(Store_T& store, std::size_t index)
		{
			const std::size_t capacity_ = static_cast<const StoredVector&>(store.at(index)).capacity;
			store.free_n(index, node_count(capacity_));
		}

		//these are to be used with caution, as vector might reallocate while in use.
		constexpr Value_T* begin() noexcept { return this->data; }
		constexpr Value_T* end() noexcept { return this->data + this->size; }

		//this is constant -> these are save to use
		constexpr const Value_T* begin() const noexcept { return this->data; }
		constexpr const Value_T* end() const noexcept { return this->data + this->size; }
	}; //struct StoredVector

	namespace stored_vector {

		struct MutEndIndicator { std::uint32_t idx; };

		template<typename Value_T, std::size_t AllocNodeSize, typename Store_T>
		struct MutIterator
		{
			using value_type      = Value_T;
			using difference_type = std::ptrdiff_t;	
			using pointer         = Value_T*;
			using reference       = Value_T&;
			using iterator_category = std::contiguous_iterator_tag;

			Store_T& store;
			std::uint32_t store_idx;
			std::uint32_t array_idx;

			constexpr MutIterator& operator++() noexcept { ++this->array_idx; return *this; }  
			constexpr MutIterator operator++(int) noexcept { auto result = *this; ++(*this); return result; }

			constexpr Value_T& operator*()  noexcept 
			{ 
				using Mut_T = std::remove_const_t<Value_T>;
				using StoredVector_T = std::conditional_t <std::is_const_v<Value_T>,
					const StoredVector<Mut_T, AllocNodeSize>,
					StoredVector<Mut_T, AllocNodeSize>>;
				return *(static_cast<StoredVector_T&>(this->store.at(this->store_idx)).data + this->array_idx); 
			}

			constexpr Value_T* operator->() noexcept 
			{ 
				using Mut_T = std::remove_const_t<Value_T>;
				using StoredVector_T = std::conditional_t <std::is_const_v<Value_T>,
					const StoredVector<Mut_T, AllocNodeSize>,
					StoredVector<Mut_T, AllocNodeSize>>;
				return static_cast<StoredVector_T&>(this->store.at(this->store_idx)).data + this->array_idx; 
			}

			constexpr bool operator==(const MutIterator& snd) const noexcept
			{
				assert(&this->store == &snd.store);
				assert(this->store_idx == snd.store_idx);
				return this->offset == snd.offset;
			}

			constexpr bool operator==(const MutEndIndicator& end_) const noexcept { return this->array_idx == end_.idx; }
		}; //struct MutIterator

	} //namespace stored_vector

	template<typename Union_T, typename Value_T, std::size_t AllocNodeSize>
	constexpr auto begin(const BasicNodeRef<Union_T, StoredVector<Value_T, AllocNodeSize>, Const::no>& ref)
	{
		using Iter = stored_vector::MutIterator<Value_T, AllocNodeSize, BasicStore<Union_T>>;
		using Vec_T = StoredVector<Value_T, AllocNodeSize>;
		return Iter(*ref.store, ref.index, 0u);
	}

	template<typename Union_T, typename Value_T, std::size_t AllocNodeSize>
	constexpr auto end(const BasicNodeRef<Union_T, StoredVector<Value_T, AllocNodeSize>, Const::no>& ref)
	{
		return stored_vector::MutEndIndicator(ref->size);
	}

	template<typename Union_T, typename Value_T, std::size_t AllocNodeSize>
	constexpr auto save_begin(const BasicNodeRef<Union_T, StoredVector<Value_T, AllocNodeSize>, Const::yes>& ref)
	{
		using Iter = stored_vector::MutIterator<const Value_T, AllocNodeSize, const BasicStore<Union_T>>;
		using Vec_T = StoredVector<Value_T, AllocNodeSize>;
		return Iter(*ref.store, ref.index, 0u);
	}

	template<typename Union_T, typename Value_T, std::size_t AllocNodeSize>
	constexpr auto save_end(const BasicNodeRef<Union_T, StoredVector<Value_T, AllocNodeSize>, Const::yes>& ref)
	{
		return stored_vector::MutEndIndicator(ref->size);
	}

} //namespace bmath::intern
