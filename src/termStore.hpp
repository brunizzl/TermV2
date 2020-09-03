
#include <cstdint>
#include <vector>
#include <cassert>
#include <bitset>
#include <type_traits>

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
	};	//class IndexTypePair








	template <typename Index_T, typename Value_T>
	class [[nodiscard]] TermStore
	{
		static_assert(std::is_default_constructible<Value_T>::value, "required for default constructor of TermStore");
		static_assert(std::is_trivially_destructible<Value_T>::value, "required to allow Value_T to be used in VecElem union");

		static constexpr std::size_t table_dist = sizeof(Value_T) * 8;	//also number of elements each table keeps track of
		using OccupancyTable = std::bitset<table_dist>;

		struct BuildValue {};	//used to differentiate the constructors of VecElem
		struct BuildTable {};	//used to differentiate the constructors of VecElem

		//every table_dist'th element in vec will not store actual term content, but a table of which of 
		//the next (table_dist -1) slots are still free. 
		//thus this union can act as both.
		union [[nodiscard]] VecElem
		{
			Value_T value;
			OccupancyTable table;
			static_assert(sizeof(Value_T) == sizeof(OccupancyTable), "union VecElem assumes equal member size");

			template<typename... Args>
			VecElem(BuildValue, Args&&... args) :value(std::forward<Args>(args)...) {}

			template<typename... Args>
			VecElem(BuildTable, Args&&... args) :table(std::forward<Args>(args)...) {}

			VecElem(const VecElem& snd) :table(snd.table) {}	//bitwise copy of snd

			~VecElem() {} //both std::bitset and Value_T are trivially destructible
		};

		std::vector<VecElem> store;

	public:
		//head stores the index in store and term-type of the term trees root.
		Index_T head;

		TermStore()
			:store(), head()
		{}

		template<typename... Args>
		[[nodiscard]] std::size_t emplace_back(Args&&... args)
		{
			constexpr std::size_t none = -1;
			const auto find_index_and_set_table = [](TermStore& self) {
				for (std::size_t table_pos = 0; table_pos < self.store.size(); table_pos += table_dist) {
					if (self.store[table_pos].table.all()) [[unlikely]] {
						continue;
					}
					else {
						OccupancyTable& table = self.store[table_pos].table;
						for (std::size_t relative_pos = 1; relative_pos < table_dist; relative_pos++) {	//first bit encodes position of table -> start one later
							if (!table[relative_pos]) {
								table.set(relative_pos);
								return table_pos + relative_pos;
							}
						}
					}
				}
				return none;
			};

			const std::size_t new_pos = find_index_and_set_table(*this);
			if (new_pos == none) [[unlikely]] {
				if (this->store.size() % table_dist != 0) [[unlikely]]
				{
					throw std::exception("TermStore's emplace_back() found no free vector index, yet the next element to append to vector is not an occupancy_table");
				}
				this->store.emplace_back(BuildTable(), 0x3);	//first bit is set, as table itself occupies that slot, second as new element is emplaced afterwards.
				this->store.emplace_back(BuildValue(), std::forward<Args>(args)...);
				return this->store.size() - 1;	//index of just inserted element
			}
			else {
				if (new_pos >= this->store.size()) {
					this->store.emplace_back(BuildValue(), std::forward<Args>(args)...);	//put new element in store
				}
				else {
					new (&this->store[new_pos]) VecElem(BuildValue(), std::forward<Args>(args)...);	//reuse old element in store
				}
				return new_pos;
			}
		}

		void free(std::size_t idx)
		{
			if (this->store.size() - 1 < idx) [[unlikely]] {
				throw std::exception("TermStore supposed to free unowned slots");
			}
			else {
				OccupancyTable& table = this->store[idx / table_dist].table;
				table.reset(idx % table_dist);
			}
		}

		[[nodiscard]] Value_T& at(std::size_t idx)
		{
			if (idx % table_dist == 0) [[unlikely]] {
				throw std::exception("TermStore::at() supposed to access Value_T, but requests Table index");
			}			
			return this->store.at(idx).value;
		}

		[[nodiscard]] const Value_T& at(std::size_t idx) const
		{
			if (idx % table_dist == 0) [[unlikely]] {
				throw std::exception("TermStore::at() supposed to access Value_T, but requests Table index");
			}			
			return this->store.at(idx).value;
		}
	};	//class TermStore

}