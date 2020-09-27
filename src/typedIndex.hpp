#pragma once

#include <cstdint>
#include <type_traits>

#include "termUtility.hpp"

namespace bmath::intern {
	//stores both an index and an enum value in the same variable 
	//with the lower bits representing the enum, the upper bits representing the (shifted) index
	template<typename TypesEnum, TypesEnum MaxEnumValue, typename UnderlyingType = std::uint32_t>
	class [[nodiscard]] BasicTypedIdx
	{
		static_assert(std::is_enum_v<TypesEnum>);
		static_assert(std::is_unsigned_v<UnderlyingType>);

		UnderlyingType data;

		static constexpr UnderlyingType nr_enum_bits()
		{
			UnderlyingType power = 0;
			while ((1 << power) <= static_cast<UnderlyingType>(MaxEnumValue)) {	//cant use pow in constexpr :(
				power++;
			}
			return power;
		}

		static constexpr UnderlyingType index_offset = nr_enum_bits();
		static constexpr UnderlyingType enum_mask = (1 << index_offset) - 1;
		static constexpr UnderlyingType index_mask = ~enum_mask;

	public:
		static constexpr std::size_t max_index = index_mask >> index_offset;

		explicit constexpr BasicTypedIdx() :data(static_cast<UnderlyingType>(0)) {}

		explicit constexpr BasicTypedIdx(UnderlyingType data_) :data(data_) {}

		constexpr BasicTypedIdx(std::size_t index, TypesEnum type)
			: data(static_cast<UnderlyingType>(index << index_offset) | static_cast<UnderlyingType>(type))
		{
			throw_if(index > max_index, "TypedIdx has recieved index bigger than max_index");
			throw_if(type > MaxEnumValue, "TypedIdx has recieved enum value bigger than MaxEnumValue");
		}

		[[nodiscard]] constexpr std::size_t get_index() const noexcept { return data >> index_offset; }
		[[nodiscard]] constexpr TypesEnum get_type() const noexcept { return static_cast<TypesEnum>(data & enum_mask); }

		auto operator<=>(const BasicTypedIdx&) const = default;
		bool operator==(const BasicTypedIdx&) const = default;
	};	//class BasicTypedIdx

} //namespace bmath::intern