
#include <cstdint>
#include <vector>
#include <cassert>
#include <bitset>
#include <type_traits>

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

		constexpr IndexTypePair() :data(static_cast<UnderlyingType>(MaxEnumValue)) {}

		constexpr IndexTypePair(std::size_t index, TypesEnum type) noexcept
			:data(static_cast<UnderlyingType>(index << index_offset) | static_cast<UnderlyingType>(type))
		{
			if (index > max_index) [[unlikely]] {
				throw std::exception("IndexTypePair has recieved index bigger than max_index");
			}
			if (type > MaxEnumValue) [[unlikely]] {
				throw std::exception("IndexTypePair has recieved enum value bigger than MaxEnumValue");
			}
		}

		[[nodiscard]] constexpr std::size_t get_index() const noexcept { return data >> index_offset; }

		constexpr void set_index(std::size_t new_index) { 
			if (new_index > max_index) [[unlikely]] {
				throw std::exception("IndexTypePair has recieved index bigger than max_index");
			}
			data = static_cast<UnderlyingType>(new_index << index_offset) | (data & enum_mask); 
		}

		[[nodiscard]] constexpr TypesEnum get_type() const noexcept { return static_cast<TypesEnum>(data & enum_mask); }

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
		static_assert(std::is_default_constructible<Value_T>::value, "required for default constructor of TermStore");
		static_assert(std::is_trivially_destructible<Value_T>::value, "required to allow Value_T to be used in VecElem union and Data union");

		static constexpr std::size_t vec_elem_size = sizeof(Value_T) * 8;

		//every vec_elem_size'th element in vec will not store actual term content, but a table of which of 
		//the next (vec_elem_size -1) slots are still free. 
		union VecElem
		{
			Value_T value;
			std::bitset<vec_elem_size> occupied_slots;
			static_assert(sizeof(Value_T) == sizeof(std::bitset<vec_elem_size>), "union VecElem assumes equal member size");

			struct BuildValue {};	//used to differentiate the constructors
			template<typename... Args>
			VecElem(BuildValue, Args&&... args) :value(std::forward<Args>(args)...) {}

			struct BuildBitset {};	//used to differentiate the constructors
			template<typename... Args>
			VecElem(BuildBitset, Args&&... args) :bitset(std::forward<Args>(args)...) {}

			VecElem(const VecElem& snd) :occupied_slots(snd.occupied_slots) {}	//just does a shallow copy of snd

			~VecElem() {} //both std::bitset and Value_T are trivially destructible
		};

		//head stores the index and term-type of the term trees root.
		//if head.get_index() is set to 0, head lies not in vec, but is val.
		//this is always possible, as at vec[0] only occupancy data is stored (named occupied_slots)
		Index_T head;

		//if the term tree consists only of its root, this will be stored in val directly, not in the vector. (with head.get_index() == 0)
		union Data
		{
			std::vector<VecElem> vec;
			Value_T val;

			template<typename... Args>
			Data(Args&&... args) :val(std::forward<Args>(args)...) {}

			Data() :val() {}

			Data(Data& other) { std::memcpy(this, &other, sizeof(Data)); }	//will always just copy the other bit by bit
			~Data() {}	//does not clean up vec, because it doesnt know if vec is in use
		} data;

	public:

		TermStore() noexcept
			:head(), data()
		{
		}

		TermStore(TermStore&& other)
			:head(other.head), data(other.data)
		{
			if (other.head.get_index() != 0) {
				new(&this->data.vec, std::move(other.data.vec));
			}
		}

		~TermStore()
		{
			if (head.get_index() != 0) {
				data.vec.~vector<VecElem>();
			}
		}

	};
}