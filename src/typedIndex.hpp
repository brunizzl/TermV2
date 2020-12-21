#pragma once

#include <cstdint>
#include <type_traits>
#include <bit>

#include "termUtility.hpp"

namespace bmath::intern {

	template<typename UnderlyingType, typename TypesEnum>
	struct [[nodiscard]] SplitResult
	{
		UnderlyingType index;
		TypesEnum type;
	};

	//stores both an index and an enum value in the same variable 
	//with the lower bits representing the enum, the upper bits representing the (shifted) index
	template<typename TypesEnum, typename UnderlyingType = std::uint32_t>
	class [[nodiscard]] BasicTypedIdx_Bitmask
	{
		static_assert(std::is_unsigned_v<UnderlyingType>);

		UnderlyingType data;

		static constexpr std::size_t index_offset = std::bit_width((unsigned)TypesEnum::COUNT);
		static constexpr UnderlyingType enum_mask = (1ull << index_offset) - 1ull;
		static constexpr UnderlyingType index_mask = ~enum_mask;

	public:
		using Enum_T = TypesEnum;
		static constexpr std::size_t max_index = index_mask >> index_offset;

		explicit constexpr BasicTypedIdx_Bitmask() noexcept :data(-1u) {}

		constexpr BasicTypedIdx_Bitmask(const std::size_t index, const TypesEnum type)
			: data(static_cast<UnderlyingType>(index << index_offset) | static_cast<UnderlyingType>(type))
		{
			throw_if(index > max_index, "TypedIdx has recieved index bigger than max_index");
			throw_if(type > TypesEnum::COUNT, "TypedIdx has recieved enum value bigger than MaxEnumValue");
		}

		[[nodiscard]] constexpr auto get_index() const noexcept { return data >> index_offset; }
		[[nodiscard]] constexpr auto get_type() const noexcept { return static_cast<TypesEnum>(data & enum_mask); }

		[[nodiscard]] constexpr auto split() const noexcept
		{ return SplitResult<UnderlyingType, TypesEnum>{ this->get_index(), this->get_type() }; }

		constexpr friend std::strong_ordering operator<=>(const BasicTypedIdx_Bitmask&, const BasicTypedIdx_Bitmask&) = default;
		constexpr friend bool operator==(const BasicTypedIdx_Bitmask&, const BasicTypedIdx_Bitmask&) = default;
	};	//class BasicTypedIdx_Bitmask




	//prefered for debugging, as debugger allows inspection of single fields
	template<typename TypesEnum, typename UnderlyingType = std::uint32_t>
	class [[nodiscard]] BasicTypedIdx_BitField
	{
		static_assert(std::is_unsigned_v<UnderlyingType>);

		static constexpr std::size_t index_offset = std::bit_width((unsigned)TypesEnum::COUNT);
		static constexpr UnderlyingType enum_mask = (1ull << index_offset) - 1ull;
		static constexpr UnderlyingType index_mask = ~enum_mask;

		UnderlyingType index : sizeof(UnderlyingType) * 8u - index_offset;
		UnderlyingType type : index_offset;

	public:
		using Enum_T = TypesEnum;
		static constexpr std::size_t max_index = index_mask >> index_offset;

		explicit constexpr BasicTypedIdx_BitField() noexcept :index(-1u), type(static_cast<UnderlyingType>(TypesEnum::COUNT)) {}

		constexpr BasicTypedIdx_BitField(const std::size_t new_index, const TypesEnum new_type)
			:index(new_index), type(static_cast<UnderlyingType>(new_type))
		{
			throw_if(index > max_index, "TypedIdx has recieved index bigger than max_index");
			throw_if(type > static_cast<UnderlyingType>(TypesEnum::COUNT), "TypedIdx has recieved enum value bigger than MaxEnumValue");
		}

		[[nodiscard]] constexpr auto get_index() const noexcept { return this->index; }
		[[nodiscard]] constexpr auto get_type() const noexcept { return static_cast<TypesEnum>(this->type); }

		[[nodiscard]] constexpr auto split() const noexcept
		{ return SplitResult<UnderlyingType, TypesEnum>{ this->get_index(), this->get_type() }; }

		constexpr friend std::strong_ordering operator<=>(const BasicTypedIdx_BitField&, const BasicTypedIdx_BitField&) = default;
		constexpr friend bool operator==(const BasicTypedIdx_BitField&, const BasicTypedIdx_BitField&) = default;
	};	//class BasicTypedIdx_BitField


	template<typename TypesEnum, typename UnderlyingType = std::uint32_t>
	//using BasicTypedIdx = BasicTypedIdx_Bitmask<TypesEnum, UnderlyingType>;
	using BasicTypedIdx = BasicTypedIdx_BitField<TypesEnum, UnderlyingType>;

} //namespace bmath::intern