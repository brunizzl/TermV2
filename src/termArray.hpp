
#pragma once

#include <cstdint>
#include <complex>
#include <cassert>
#include <algorithm>

namespace bmath::intern {

	template<typename Value_T, std::size_t AllocNodeSize = sizeof(std::complex<double>)>
	struct Array
	{
		static_assert(std::is_trivially_destructible_v<Value_T>);
		static_assert(std::is_trivially_copyable_v<Value_T>);

		static constexpr std::size_t min_capacity = (AllocNodeSize - 2u * sizeof(std::uint16_t)) / sizeof(Value_T);
		static constexpr std::size_t values_per_node = AllocNodeSize / sizeof(Value_T);

		union
		{
			struct FirstNode
			{
				std::uint16_t size = 0u;
				std::uint16_t capacity = min_capacity;

				Value_T data_[min_capacity];
			} first;

			struct FollowingNode
			{
				Value_T data_[values_per_node];
			} following;
		};

		constexpr Array(const std::size_t new_size = 0u, const std::size_t new_capacity = min_capacity) noexcept 
			:first.size(new_size), first.capacity(new_capacity)
		{}

		static constexpr std::size_t node_count(const std::size_t new_capacity) noexcept 
		{			
			return (new_capacity - min_capacity) / values_per_node + 1u;
		}

		template<typename Store_T, typename Vec_T>
		static constexpr std::size_t build(Store_T& store, const Vec_T& new_data)
		{
			const std::size_t alloc_idx = store.allocate_n(node_count(new_data.size()));
			const std::size_t alloc_capacity = (new_data.size() + values_per_node - 1u) / values_per_node * values_per_node;
			const Array* new_array = &static_cast<Array&>(store.at(alloc_idx));
			new (new_array) Array(new_data.size(), alloc_capacity);
			std::copy_n(new_data.data(), new_data.size(), +new_array.first.data_);
		}

		template<typename Store_T>
		struct ConstIterator
		{
			const Value_T* where;

		}; //struct Iterator

	}; //struct Array


} //namespace bmath::intern
