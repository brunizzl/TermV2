
#include <cstdint>
#include <concepts>
#include <vector>
#include <cmath>
#include <cassert>

#include "baseTerm.hpp"

namespace bmath::intern {

	

	template<typename TypesEnum, TypesEnum MaxEnumValue, typename UnderlyingType = std::uint32_t>
	class [[nodiscard]] IndexTypePair
	{
		UnderlyingType data;
		
		static constexpr UnderlyingType enums_used_digits() 
		{
			UnderlyingType power = 0;
			while ((1 << power) <= static_cast<UnderlyingType>(MaxEnumValue)) {
				power++;
			}
			return power;
		}

		static constexpr UnderlyingType index_offset = enums_used_digits();
		static constexpr UnderlyingType enum_mask = (1 << index_offset) - 1;
		static constexpr UnderlyingType index_mask = ~enum_mask;

	public:
		static constexpr std::size_t max_index =  index_mask >> index_offset;

		constexpr IndexTypePair(std::size_t index, TypesEnum type)
			:data(static_cast<UnderlyingType>(index << index_offset) | static_cast<UnderlyingType>(type))
		{
			if (index > max_index) [[unlikely]] {
				throw std::exception("IndexTypePair has recieved index bigger than max_index");
			}
			if (type > MaxEnumValue) [[unlikely]] {
				throw std::exception("IndexTypePair has recieved enum value bigger than MaxEnumValue");
			}
		}

		[[nodiscard]] constexpr std::size_t get_index() const { return data >> index_offset; }

		constexpr void set_index(std::size_t new_index) { 
			if (new_index > max_index) [[unlikely]] {
				throw std::exception("IndexTypePair has recieved index bigger than max_index");
			}
			data = static_cast<UnderlyingType>(new_index << index_offset) | (data & enum_mask); 
		}

		[[nodiscard]] constexpr TypesEnum get_type() const { return static_cast<TypesEnum>(data & enum_mask); }

		constexpr void set_type(TypesEnum new_type) { 
			if (new_type > MaxEnumValue) [[unlikely]] {
				throw std::exception("IndexTypePair has recieved enum value bigger than MaxEnumValue");
			}
			data = (data & index_mask) | static_cast<UnderlyingType>(new_type); 
		}
	};

	using TermIndexTypePair = IndexTypePair<Type, Type::COUNT>;

	template <typename Index_T, typename Value_T>
	class [[nodiscard]] TermStore
	{


		Index_T head;
		union
		{
			std::vector<Value_T> vector;
			Value_T single_val;
		};


	};

}