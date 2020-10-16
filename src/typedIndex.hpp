#pragma once

#include <cstdint>
#include <type_traits>

#include "termUtility.hpp"

namespace bmath::intern {

	template<typename UnderlyingType, typename TypesEnum>
	struct SplitResult
	{
		UnderlyingType index;
		TypesEnum type;
	};

	//stores both an index and an enum value in the same variable 
	//with the lower bits representing the enum, the upper bits representing the (shifted) index
	template<typename TypesEnum, typename UnderlyingType = std::uint32_t>
	class [[nodiscard]] BasicTypedIdx
	{
		static_assert(std::is_unsigned_v<UnderlyingType>);
		static_assert(sizeof(UnderlyingType) >= sizeof(TypesEnum));

		static constexpr UnderlyingType nr_enum_bits()
		{
			UnderlyingType power = 0;
			while ((1 << power) <= static_cast<UnderlyingType>(TypesEnum::COUNT)) {	//cant use pow in constexpr :(
				power++;
			}
			return power;
		}

		UnderlyingType data;

		static constexpr UnderlyingType index_offset = nr_enum_bits();
		static constexpr UnderlyingType enum_mask = (1 << index_offset) - 1;
		static constexpr UnderlyingType index_mask = ~enum_mask;

	public:
		using Enum_T = TypesEnum;
		static constexpr std::size_t max_index = index_mask >> index_offset;

		explicit constexpr BasicTypedIdx() :data(static_cast<UnderlyingType>(TypesEnum::COUNT)) {}

		constexpr BasicTypedIdx(std::size_t index, TypesEnum type)
			: data(static_cast<UnderlyingType>(index << index_offset) | static_cast<UnderlyingType>(type))
		{
			throw_if(index > max_index, "TypedIdx has recieved index bigger than max_index");
			throw_if(type > TypesEnum::COUNT, "TypedIdx has recieved enum value bigger than MaxEnumValue");
		}

		[[nodiscard]] constexpr auto get_index() const noexcept { return data >> index_offset; }
		[[nodiscard]] constexpr auto get_type() const noexcept { return static_cast<TypesEnum>(data & enum_mask); }

		[[nodiscard]] constexpr auto split() const noexcept
		{ return SplitResult<UnderlyingType, TypesEnum>{ this->get_index(), this->get_type() }; }

		constexpr friend std::strong_ordering operator<=>(const BasicTypedIdx&, const BasicTypedIdx&) = default;
		constexpr friend bool operator==(const BasicTypedIdx&, const BasicTypedIdx&) = default;
	};	//class BasicTypedIdx

} //namespace bmath::intern