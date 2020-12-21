#pragma once

#include <exception>
#include <algorithm>
#include <cassert>
#include <array>
#include <vector>
#include <span>
#include <type_traits>
#include <bit>
#include <complex>
#include <concepts>
#include <compare>

#define ASSERT(expr) if (!(expr)) __debugbreak()

namespace bmath::intern {

	constexpr void throw_if(bool cond, const char* const msg)
	{
		if (cond) [[unlikely]] {
			throw std::exception(msg);
		}
	}


	template<const auto x, const auto... xs, typename T>
	constexpr bool is_one_of(const T y) noexcept
	{ 
		static_assert(sizeof(T) <= 8, "this function copies the values, ya dummie!"); //although i assume this to be inlined.
		if constexpr (!sizeof...(xs)) { return x == y; }
		else                          { return x == y || is_one_of<xs...>(y); }
	}	

	template<typename... Bool>
	constexpr bool equivalent(const bool x, const bool y, const Bool... xs)
	{
		if  constexpr (!sizeof...(xs)) { return x == y; }
		else                           { return x == y && equivalent(x, xs...); }
	}


	template<std::size_t N>
	constexpr auto replicate(auto val)
	{
		std::array<decltype(val), N> result;
		result.fill(val);
		return result;
	}
	static_assert(replicate<3>('a') == std::array{ 'a', 'a', 'a' });


	template <typename Struct_T, std::size_t Size, typename SearchMemberPtr_T, typename Search_T>
	[[nodiscard]] constexpr const Struct_T& find(
		const std::array<Struct_T, Size>& data, const SearchMemberPtr_T ptr, const Search_T key) noexcept
	{
		const auto itr = std::find_if(begin(data), end(data), [key, ptr](const auto &v) { return v.*ptr == key; });
		const bool valid = itr != end(data);
		assert(valid);
		return *itr;
	}

	template <typename Struct_T, std::size_t Size, typename SearchMemberPtr_T, typename Search_T>
	[[nodiscard]] constexpr const Struct_T& search(
		const std::array<Struct_T, Size>& data, const SearchMemberPtr_T ptr, const Search_T key, const Struct_T& null_val = {}) noexcept
	{
		const auto itr = std::find_if(begin(data), end(data), [key, ptr](const auto &v) { return v.*ptr == key; });
		return itr != end(data) ? *itr : null_val;
	}


	//taken (and adapted) from here: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0732r2.pdf
	template<std::size_t N> 
	struct StringLiteral :std::array<char, N>
	{
		//perhaps change to array reference of length N + 1? (+ 1 because '\0')
		constexpr StringLiteral(const char* init) { std::copy_n(init, N, this->data()); } 

		template<std::size_t Start, std::size_t Length>
		constexpr StringLiteral<Length> substr() const noexcept 
		{ 
			static_assert(Start + Length <= N);
			return StringLiteral<Length>(this->data() + Start);
		}

		constexpr bool operator==(const StringLiteral&) const noexcept = default;
	};

	template<std::size_t N>
	StringLiteral(const char (&str)[N]) -> StringLiteral<N - 1>;



	template<typename Value_T, std::size_t BufferSize>
	class [[nodiscard]] StupidBufferVector
	{
		static_assert(BufferSize > 0u);
		static_assert(std::is_trivially_copyable_v<Value_T>);     //thus the "Stupid" in the name
		static_assert(std::is_trivially_destructible_v<Value_T>); //thus the "Stupid" in the name

		std::size_t size_ = 0u;
		Value_T* data_ = &local_data[0];
		union
		{
			std::size_t capacity = 0u;
			Value_T local_data[BufferSize];
		};

		template<typename SndValue_T, std::size_t SndBuffer>
		friend class StupidBufferVector;

		constexpr void unsave_reallocate(const std::size_t new_capacity) noexcept
		{
			assert(new_capacity > BufferSize && (this->data_ == this->local_data || new_capacity > this->capacity) && 
				"no reallocation neccesairy");

			Value_T* const new_data = new Value_T[new_capacity];
			std::move(this->data_, this->data_ + this->size_, new_data);
			if (this->data_ != this->local_data) {
				delete[] this->data_; //only works for trivially destructible Value_T
			}
			this->data_ = new_data;
			this->capacity = new_capacity;
		}

	public:
		constexpr std::size_t size() const noexcept { return this->size_; }
		constexpr const Value_T* data() const noexcept { return this->data_; }
		constexpr Value_T* data() noexcept { return this->data_; }

		constexpr StupidBufferVector() noexcept :size_(0u) {}

		constexpr StupidBufferVector(const std::initializer_list<Value_T> init) noexcept :size_(init.size())
		{
			if (init.size() > BufferSize) [[unlikely]] {
				this->data_ = new Value_T[init.size()];
				this->capacity = init.size();
			}
			std::copy(init.begin(), init.end(), this->data_);
		}

		~StupidBufferVector() noexcept
		{
			if (this->data_ != this->local_data) {
				delete[] this->data_; //only works for trivially destructible Value_T
			}
		}

		constexpr void reserve(const std::size_t new_capacity) noexcept
		{
			if (new_capacity > BufferSize && 
				(this->data_ == this->local_data || new_capacity > this->capacity)) 
			{
				this->unsave_reallocate(new_capacity);
			}
		}

		StupidBufferVector(const StupidBufferVector&) = delete;

		template<std::size_t SndBuffer>
		StupidBufferVector& operator=(const StupidBufferVector<Value_T, SndBuffer>& snd) noexcept
		{
			assert(false && "please be aware this function is not yet tested");
			this->clear();
			this->reserve(snd.size_);
			std::copy(snd.data_, snd.data_ + snd.size_, this->data_);
		}

		template<std::size_t SndBuffer>
		StupidBufferVector& operator=(StupidBufferVector<Value_T, SndBuffer>&& snd) noexcept
		{
			this->~StupidBufferVector();
			new (this) StupidBufferVector();

			if (snd.data_ != snd.local_data) { //only works for trivially destructible Value_T
				this->data_ = std::exchange(snd.data_, nullptr);
				this->capacity = std::exchange(snd.capacity, 0u);
				this->size_ = std::exchange(snd.size_, 0u);
			}
			else {
				this->reserve(snd.size_);
				this->size_ = snd.size_;
				std::move(snd.data_, snd.data_ + snd.size_, this->data_);
				snd.clear();
			} 
			return *this;
		}

		constexpr StupidBufferVector(StupidBufferVector&& snd) noexcept :size_(std::exchange(snd.size_, 0u))
		{
			if (snd.data_ != snd.local_data) {
				this->data_ = std::exchange(snd.data_, nullptr);
				this->capacity = std::exchange(snd.capacity, 0u);
			}
			else {
				std::move(snd.data_, snd.data_ + this->size_, this->data_);
			}
		}

		template<typename... Args>
		constexpr Value_T& emplace_back(Args&&... args) noexcept
		{
			if (this->size_ > BufferSize) {
				if (this->size_ == this->capacity) [[unlikely]] {
					this->unsave_reallocate(this->capacity * 2u);
				}
			}
			else if (this->size_ == BufferSize) [[unlikely]] {
				this->unsave_reallocate(BufferSize * 2u);
			}

			Value_T* const addr = &this->data_[this->size_++];
			new (addr) Value_T{ args... };
			return  *addr;
		}

		constexpr Value_T& push_back(const Value_T& elem) noexcept { return this->emplace_back(elem); }

		constexpr Value_T pop_back() noexcept
		{ 
			assert(this->size_ > 0u && "tried popping on empty data_vector");
			return this->data_[--this->size_]; //only works for trivially destructible Value_T
		}

		constexpr void shorten_to(Value_T* const new_end) noexcept
		{
			const std::size_t new_size = new_end - this->data_;
			assert(new_size <= this->size_);
			this->size_ = new_size; //only works for trivially destructible Value_T
		}

		//may be used with caution, as this does no initialisation
		constexpr void expand_to_size(const std::size_t new_size) noexcept
		{
			assert(this->size_ <= new_size);
			this->reserve(new_size);
			this->size_ = new_size;
		}

		constexpr void clear() noexcept { this->size_ = 0u; } //only works for trivially destructible Value_T

		constexpr const Value_T& operator[](std::size_t where) const noexcept { return this->data_[where]; }
		constexpr Value_T& operator[](std::size_t where) noexcept { return this->data_[where]; }

		constexpr const Value_T& front() const noexcept { return this->data_[0]; }
		constexpr const Value_T& back() const noexcept { return this->data_[this->size_ - 1u]; }
		constexpr Value_T& front() noexcept { return this->data_[0]; }
		constexpr Value_T& back() noexcept { return this->data_[this->size_ - 1u]; }

		constexpr const Value_T* begin() const noexcept { return this->data_; }
		constexpr const Value_T* end() const noexcept { return this->data_ + this->size_; }
		constexpr Value_T* begin() noexcept { return this->data_; }
		constexpr Value_T* end() noexcept { return this->data_ + this->size_; }

		constexpr std::span<const Value_T> range() const noexcept { return { this->data_, this->size_ }; }
		constexpr std::span<Value_T> range() noexcept { return { this->data_, this->size_ }; }

	}; //class StupidBufferVector



	template<typename Value_T, std::size_t MaxSize>
	class [[nodiscard]] ShortVector
	{
		std::size_t size_;
		Value_T data_[MaxSize];

	public:
		constexpr std::size_t size() const noexcept { return this->size_; }
		constexpr const Value_T* data() const noexcept { return this->data_; }
		constexpr Value_T* data() noexcept { return this->data_; }

		constexpr ShortVector() noexcept :size_(0) {}

		constexpr ShortVector(const std::initializer_list<Value_T> init) noexcept :size_(init.size())
		{
			assert(init.size() <= MaxSize && "initializer length exeeds MaxSize");
			std::copy(init.begin(), init.end(), this->data_);
		}

		ShortVector(const ShortVector&) = delete;
		ShortVector& operator=(const ShortVector&) = delete;
		ShortVector& operator=(ShortVector&&) = delete;
		ShortVector(ShortVector&& snd) = delete;

		template<typename... Args>
		constexpr Value_T& emplace_back(Args&&... args) noexcept
		{
			assert(this->size_ < MaxSize && "tried pushing on full data_vector");
			Value_T* const addr = &this->data_[this->size_++];
			new (addr) Value_T{ args... };
			return *addr;
		}

		constexpr Value_T& push_back(const Value_T& elem) noexcept { return this->emplace_back(elem); }

		constexpr void pop_back() noexcept
		{ 
			assert(this->size_ > 0u && "tried popping on empty data_vector");
			this->data_[--this->size_].~Value_T();
		}

		constexpr void clear() noexcept { 
			if constexpr (std::is_trivially_destructible_v<Value_T>) { 
				this->size_ = 0u; 
			}
			else { 
				while (this->size_) { this->pop_back(); } 
			}
		}

		constexpr const Value_T& operator[](const std::size_t where) const noexcept { return this->data_[where]; }
		constexpr Value_T& operator[](const std::size_t where) noexcept { return this->data_[where]; }

		constexpr const Value_T& front() const noexcept {  return this->data_[0]; }
		constexpr const Value_T& back() const noexcept {  return this->data_[this->size_ - 1u]; }
		constexpr Value_T& front() noexcept { return this->data_[0]; }
		constexpr Value_T& back() noexcept { return this->data_[this->size_ - 1u]; }

		constexpr const Value_T* begin() const noexcept { return this->data_; }
		constexpr const Value_T* end() const noexcept { return this->data_ + this->size_; }
		constexpr Value_T* begin() noexcept { return this->data_; }
		constexpr Value_T* end() noexcept { return this->data_ + this->size_; }

		constexpr std::span<const Value_T> range() const noexcept { return { this->data_, this->size_ }; }
		constexpr std::span<Value_T> range() noexcept { return { this->data_, this->size_ }; }

		constexpr friend auto operator<=>(const ShortVector&, const ShortVector&) = default;
		constexpr friend bool operator==(const ShortVector&, const ShortVector&) = default;

	}; //class ShortVector




	template<typename... Enums>
	class [[nodiscard]] SumEnum;

	namespace enum_detail {

		template<typename Needle, typename Haystack>
		struct Contains :std::false_type {};

		template<typename Needle, typename Haystack>
		constexpr bool contains_v = Contains<Needle, Haystack>::value;

		template<typename Needle, typename... HayTail>
		struct Contains<Needle, SumEnum<Needle, HayTail...>> :std::true_type {};

		template<typename Needle, typename HayHead, typename... HayTail>
		struct Contains<Needle, SumEnum<HayHead, HayTail...>> :std::bool_constant<
			contains_v<Needle, HayHead> || contains_v<Needle, SumEnum<HayTail...>>
		> {};


		template<typename, typename = void> 
		struct HasCOUNT :std::false_type {};

		template<typename E> 
		struct HasCOUNT<E, std::void_t<decltype(E::COUNT)>> :std::true_type {};




		template<typename...> struct List;

		template<typename...> struct IsEmpty;
		template<typename... Elems> struct IsEmpty<List<Elems...>> :std::bool_constant<sizeof...(Elems) == 0> {};
		template<typename... Elems> constexpr bool is_empty_v = IsEmpty<Elems...>::value;

		static_assert(is_empty_v<List<>> && !is_empty_v<List<int, double>>);


		template<typename, typename> struct Concat;

		template<typename... Elems_1, typename... Elems_2>
		struct Concat<List<Elems_1...>, List<Elems_2...>> { using type = List<Elems_1..., Elems_2...>; };

		static_assert(std::is_same_v<Concat<List<int, int>, List<double, nullptr_t>>::type, List<int, int, double, nullptr_t>>);



		template<typename...> struct ListMembers;

		template<typename Head, typename... Tail>
		struct ListMembers<Head, Tail...> 
		{ using type = typename Concat<typename ListMembers<Head>::type, typename ListMembers<Tail...>::type>::type; };

		template<typename... Enums>
		struct ListMembers<SumEnum<Enums...>> { using type = typename ListMembers<Enums...>::type; };

		template<typename Enum>
		struct ListMembers<Enum> { using type = List<Enum>; };

		template<> struct ListMembers<> { using type = List<>; };

		static_assert(std::is_same_v<ListMembers<SumEnum<int, SumEnum<float, bool>>>::type, List<int, float, bool>>);


		template<typename, typename> struct InList;
		template<typename Needle, typename List_> constexpr bool in_list_v = InList<Needle, List_>::value;

		template<typename Needle, typename Elem_0, typename... Elems>
		struct InList<Needle, List<Elem_0, Elems...>> :std::bool_constant<
			std::is_same_v<Needle, Elem_0> || in_list_v<Needle, List<Elems...>>
		> {};

		template<typename Needle> struct InList<Needle, List<>> :std::false_type {};

		static_assert(!in_list_v<char, List<double, int, int, void>>);
		static_assert(in_list_v<char, List<double, char, int, void>>);


		template<typename, typename> struct Intersection;

		template<typename Lhs_1, typename... Lhs_Tail, typename... Rhs>
		struct Intersection<List<Lhs_1, Lhs_Tail...>, List<Rhs...>> 
		{ 
			using type = typename std::conditional_t<in_list_v<Lhs_1, List<Rhs...>>,
				typename Concat<List<Lhs_1>, typename Intersection<List<Lhs_Tail...>, List<Rhs...>>::type>::type,
				typename Intersection<List<Lhs_Tail...>, List<Rhs...>>::type
			>;
		};

		template<typename... Rhs> struct Intersection<List<>, List<Rhs...>> { using type = List<>; };

		static_assert(std::is_same_v<Intersection<List<bool, int, char>, List<float, double, bool>>::type, List<bool>>);


		template<typename, typename> struct Disjoint;

		template<typename... Lhs, typename... Rhs> 
		struct Disjoint<List<Lhs...>, List<Rhs...>> :std::bool_constant<
			is_empty_v<typename Intersection<List<Lhs...>, List<Rhs...>>::type>
		> {};

		template<typename List_1, typename List_2> 
		constexpr bool disjoint_v = Disjoint<List_1, List_2>::value;
				
		static_assert(disjoint_v<List<bool, int, char>, List<float, double>>);
		static_assert(!disjoint_v<List<bool, int, char>, List<float, double, int>>);

	} //namespace enum_detail

	template<>
	class [[nodiscard]] SumEnum<>
	{
	protected:

		enum class Value :unsigned {} value; //only data held by all of SumEnum
		static constexpr unsigned next_offset = 0u;

	public:
		constexpr SumEnum(const Value e) noexcept :value(e) {}
		constexpr operator Value() const noexcept { return this->value; } //implicit conversion allows use in switch

		explicit constexpr SumEnum(const unsigned u) noexcept :value(static_cast<Value>(u)) {}
		explicit constexpr operator unsigned() const noexcept { return static_cast<unsigned>(this->value); }
	}; //class SumEnum<>


	template<typename Enum, typename... TailEnums>
	class [[nodiscard]] SumEnum<Enum, TailEnums...> :public SumEnum<TailEnums...>
	{
		static_assert(enum_detail::HasCOUNT<Enum>::value, 
			"enum part of SumEnum must name last member COUNT");
		static_assert(enum_detail::disjoint_v<enum_detail::ListMembers<Enum>::type, enum_detail::ListMembers<TailEnums...>::type>, 
			"No two parameters of SumEnum's parameter pack may contain the same type within (or be equal).");

		using Base = SumEnum<TailEnums...>;
		static constexpr unsigned this_offset = Base::next_offset;

	protected:
		using Value = typename Base::Value;
		static constexpr unsigned next_offset = static_cast<unsigned>(Enum::COUNT) + this_offset;

	public:
		using Base::Base;
		constexpr SumEnum(const Enum e) noexcept :Base(static_cast<unsigned>(e) + this_offset) {}

		//this constructor applies if Enum itself is WrapEnum<E> or SumEnum<...> that contains E (directly or deeper within)
		template<typename E> requires enum_detail::contains_v<E, Enum>
		constexpr SumEnum(const E e) noexcept : Base(static_cast<unsigned>(static_cast<Enum>(e)) + this_offset) {}


		//E is contained in Base -> hand over to Base
		template<typename E> requires enum_detail::contains_v<E, Base>
		constexpr E to() const noexcept { return static_cast<const Base>(*this).to<E>(); }

		//Enum itself is SumEnum<...> and contains E -> hand over to Enum
		template<typename E> requires enum_detail::contains_v<E, Enum>
		constexpr E to() const noexcept { return this->to<Enum>().to<E>(); }

		//E is same as Enum -> just undo the offset
		template<typename E> requires std::is_same_v<E, Enum>
		constexpr E to() const noexcept { return Enum(static_cast<unsigned>(this->value) - this_offset); }


		//default case: search in parent types
		template<typename E> requires enum_detail::contains_v<E, Base>
		constexpr bool is() const noexcept { return static_cast<const Base>(*this).is<E>(); }

		//Enum itself is SumEnum<...> and contains E -> hand over to Enum
		template<typename E> requires enum_detail::contains_v<E, Enum>
		constexpr bool is() const noexcept { return this->to<Enum>().is<E>(); }

		//E is same as Enum -> check if current value is between offsets
		template<typename E> requires std::is_same_v<E, Enum>
		constexpr bool is() const noexcept 
		{ 
			return static_cast<unsigned>(this->value) >= this_offset && 
				static_cast<unsigned>(this->value) < next_offset; 
		}


		constexpr friend std::strong_ordering operator<=>(const SumEnum&, const SumEnum&) noexcept = default;
		constexpr friend bool operator==(const SumEnum&, const SumEnum&) noexcept = default;
		static constexpr Value COUNT = static_cast<Value>(next_offset); //only relevant for outhermost instanciation
	}; //class SumEnum<Enum, TailEnums...>


	template<typename Enum, Enum LastMember>
	struct [[nodiscard]] WrapEnum //allows to use SumEnum with enums not having their last member named COUNT
	{
		static_assert(std::is_enum_v<Enum>);

		Enum value;
		constexpr WrapEnum(const Enum e) noexcept :value(e) {}

		explicit constexpr WrapEnum(const unsigned u) noexcept :value(static_cast<Enum>(u)) {}
		explicit constexpr operator unsigned() const noexcept { return static_cast<unsigned>(this->value); }

		//this is the only reason for WrapEnum to exist.
		static constexpr Enum COUNT = static_cast<Enum>(static_cast<unsigned>(LastMember) + 1u);
	}; //struct WrapEnum 

	   //if a member of SumEnum only has a single state itself, this may be used
	template<StringLiteral name>
	struct UnitEnum
	{
		explicit constexpr operator unsigned() const noexcept { return 0u; }
		static constexpr unsigned COUNT = 1u;
	};

	//crutch while intellisense doesnt know class non type template parameters
#define UNIT_ENUM(NAME) struct NAME {\
		explicit constexpr operator unsigned() const noexcept { return 0u; }\
		static constexpr unsigned COUNT = 1u;\
	}





	//remove if c++20 libraries have catched up
	template<typename T>
	constexpr std::strong_ordering compare_arrays(const T* lhs, const T* rhs, std::size_t size)
	{
		while (size --> 1u && *lhs == *rhs) {
			lhs++;
			rhs++;
		}
		return *lhs <=> *rhs;
	}

	constexpr std::strong_ordering compare_double(const double lhs, const double rhs)
	{
		if (lhs == 0.0 && rhs == 0.0) [[unlikely]] { //different zero signs are ignored
			return std::strong_ordering::equal; 
		}
		static_assert(sizeof(double) == sizeof(std::uint64_t)); //bit_cast may cast to something of doubles size.
		return std::bit_cast<std::uint64_t>(lhs) <=> std::bit_cast<std::uint64_t>(rhs);
	}

	constexpr std::strong_ordering compare_complex(const std::complex<double>& lhs, const std::complex<double>& rhs)
	{
		if (const auto cmp = compare_double(lhs.real(), rhs.real()); cmp != std::strong_ordering::equal) {
			return cmp;
		}
		return compare_double(lhs.imag(), rhs.imag());
	}





	template<typename UInt_T>
	struct [[nodiscard]] IntBitSet
	{
		static_assert(std::is_unsigned_v<UInt_T>);

		static constexpr std::size_t npos = std::countr_zero((UInt_T(0)));

		UInt_T data;

		constexpr IntBitSet() noexcept :data(0u) {}
		constexpr IntBitSet(const UInt_T new_data) noexcept :data(new_data) {}
		constexpr operator UInt_T() const noexcept { return this->data; }

		constexpr void  flip(const std::uint32_t pos) noexcept { this->data ^=  (UInt_T(1) << pos); }
		constexpr void reset(const std::uint32_t pos) noexcept { this->data &= ~(UInt_T(1) << pos); }
		constexpr void   set(const std::uint32_t pos) noexcept { this->data |=  (UInt_T(1) << pos); }
		constexpr void   set(const std::uint32_t pos, const bool val) noexcept { val ? this->set(pos) : this->reset(pos); }

		constexpr bool  test(const std::uint32_t pos) const noexcept { return this->data & (UInt_T(1) << pos); }

		constexpr bool none() const noexcept { return   !this->data;  }
		constexpr bool  any() const noexcept { return    this->data;  }
		constexpr bool  all() const noexcept { return !(~this->data); }

		constexpr std::uint32_t count() const noexcept { return std::popcount(this->data); }
		constexpr std::uint32_t find_first_true() const noexcept { return std::countr_zero(this->data); }
		constexpr std::uint32_t find_first_false() const noexcept { return std::countr_one(this->data); }

		constexpr void operator&=(const IntBitSet snd) noexcept { this->data &= snd.data; }
		constexpr void operator|=(const IntBitSet snd) noexcept { this->data |= snd.data; }
		constexpr void operator^=(const IntBitSet snd) noexcept { this->data ^= snd.data; }
	}; //class IntBitSet

	using BitSet8  = IntBitSet<std::uint8_t >;
	using BitSet16 = IntBitSet<std::uint16_t>;
	using BitSet32 = IntBitSet<std::uint32_t>;
	using BitSet64 = IntBitSet<std::uint64_t>;

	static_assert(BitSet16::npos == 16u);
	static_assert(BitSet64::npos == 64u);

	template<std::size_t Bits, std::enable_if_t<(Bits % 64u == 0u), void*> = nullptr>
	class [[nodiscard]] BitSet
	{
		static constexpr std::size_t array_size = Bits / 64u;

		template<typename Pred>
		constexpr bool test_all(const Pred pred) const noexcept { return std::all_of(this->data, this->data + array_size, pred); }

	public:
		BitSet64 data[array_size];

		constexpr std::size_t size() const noexcept { return Bits; }

		constexpr BitSet() noexcept :data{ 0u } {}
		constexpr BitSet(const std::uint64_t new_data) noexcept : data{ new_data } {}

		constexpr void  flip(const std::size_t pos) noexcept { this->data[pos / 64u].flip(pos % 64u); }
		constexpr void reset(const std::size_t pos) noexcept { this->data[pos / 64u].reset(pos % 64u); }
		constexpr void   set(const std::size_t pos) noexcept { this->data[pos / 64u].set(pos % 64u); }
		constexpr void   set(const std::size_t pos, const bool val) noexcept { this->data[pos / 64u].set(pos % 64u, val); }

		constexpr bool  test(const std::size_t pos) const noexcept { return this->data[pos / 64u].test(pos % 64u); }

		constexpr bool  all() const noexcept { return this->test_all([](const BitSet64& x) { return x.all(); }); }
		constexpr bool none() const noexcept { return this->test_all([](const BitSet64& x) { return x.none(); }); }
		constexpr bool  any() const noexcept { return !this->none(); }

		constexpr std::size_t count() const noexcept
		{
			std::size_t result = 0u;
			for (std::size_t i = 0u; i < array_size; i++) {
				result += this->data[i].count();
			}
			return result;
		}

		constexpr std::size_t find_first_true() const noexcept
		{
			for (std::size_t i = 0u; i < array_size; i++) {
				const std::size_t bit_in_i = this->data[i].find_first_true();
				if (bit_in_i != BitSet64::npos) { 
					return i * 64u + bit_in_i; 
				}
			}
			return Bits;
		}

		constexpr std::size_t find_first_false() const noexcept
		{
			for (std::size_t i = 0u; i < array_size; i++) {
				const std::size_t bit_in_i = this->data[i].find_first_false();
				if (bit_in_i != BitSet64::npos) { 
					return i * 64u + bit_in_i; 
				}
			}
			return Bits;
		}
	}; //class BitSet


	//similar to std::vector<bool>, but with 128 bit local buffer
	class BitVector
	{
		std::size_t size_; //unit is bit, not BitSet64!
		static constexpr std::size_t local_max_size = 128u; //number of bits held by local_data
		constexpr std::size_t last_bitset_idx() const noexcept { return (this->size_ - 1u) / 64u; }

		BitSet64* data_ = &local_data[0];

		union
		{
			std::size_t capacity; //unit is bit, not BitSet64!
			BitSet64 local_data[2];
		};

		void unsave_reallocate(const std::size_t new_capacity) noexcept //new_capacity is counted in bit
		{
			assert(new_capacity > local_max_size && (this->data_ == this->local_data || new_capacity > this->capacity) && 
				"no reallocation neccesairy");
			assert(new_capacity % 64u == 0u && "realocation may be to small");

			BitSet64* const new_data = new BitSet64[new_capacity / 64u](); //zero initialized
			const std::size_t old_end = this->last_bitset_idx() + 1u;
			std::copy_n(this->data_, old_end, new_data);
			if (this->data_ != this->local_data) {
				delete[] this->data_;
			}
			this->data_ = new_data;
			this->capacity = new_capacity;
		}

	public:
		constexpr std::size_t size() const noexcept { return this->size_; }

		constexpr BitVector() noexcept :size_(0u), local_data{ 0u, 0u } {}

		constexpr BitVector(const BitSet64 data, std::size_t size) noexcept :size_(size), local_data{ data, 0u } 
		{ 
			assert(size <= local_max_size); 
		}

		//not yet needed -> not yet thought about
		BitVector(const BitVector&) = delete;
		BitVector(BitVector&&) = delete;
		BitVector& operator=(const BitVector&) = delete;
		BitVector& operator=(BitVector&&) = delete;

		~BitVector() noexcept
		{
			if (this->data_ != this->local_data) {
				delete[] this->data_;
			}
		}
		
		void reserve(const std::size_t new_capacity) noexcept
		{
			if (new_capacity > local_max_size && 
				(this->data_ == this->local_data || new_capacity > this->capacity)) 
			{
				this->unsave_reallocate(std::bit_ceil(new_capacity));
			}
		}

		constexpr void clear() noexcept 
		{ 
			for (std::size_t i = 0u; i <= this->last_bitset_idx(); i++) {
				this->data_[i] = 0ull;
			}
			this->size_ = 0u; 
		}

		constexpr void  flip(const std::size_t pos) noexcept { this->data_[pos / 64u].flip(pos % 64u); }
		constexpr void reset(const std::size_t pos) noexcept { this->data_[pos / 64u].reset(pos % 64u); }
		constexpr void   set(const std::size_t pos) noexcept { this->data_[pos / 64u].set(pos % 64u); }
		constexpr void   set(const std::size_t pos, const bool val) noexcept { this->data_[pos / 64u].set(pos % 64u, val); }

		constexpr bool  test(const std::size_t pos) const noexcept { return this->data_[pos / 64u].test(pos % 64u); }

		constexpr bool  all() const noexcept 
		{ 
			const bool all_but_last = std::all_of(this->data_, this->data_ + this->last_bitset_idx(), 
				[](const BitSet64& x) { return x.all(); }); 
			const bool last = this->data_[this->last_bitset_idx()].count() == (this->size_ % 64u);
			return all_but_last && last;
		}

		constexpr bool none() const noexcept 
		{ 
			return std::all_of(this->data_, this->data_ + this->last_bitset_idx() + 1u, 
				[](const BitSet64& x) { return x.none(); }); 
		}

		constexpr bool  any() const noexcept { return !this->none(); }

		void push_false() noexcept 
		{  
			if (this->size_ > local_max_size) {
				if (this->size_ == this->capacity) [[unlikely]] {
					this->unsave_reallocate(this->capacity * 2u);
				}
			}
			else if (this->size_ == local_max_size) [[unlikely]] {
				this->unsave_reallocate(local_max_size * 2u);
			}
			this->size_++;
		}

		void push_true() noexcept
		{
			const std::size_t old_size = this->size_;
			this->push_false();
			this->set(old_size);
		}

		void push_back(const bool bit)
		{
			const std::size_t old_size = this->size_;
			this->push_false();
			if (bit) {
				this->set(old_size);
			}
		}

		constexpr void pop_back() noexcept
		{ 
			assert(this->size_ > 0u && "tried popping on empty data_vector");
			this->reset(--this->size_);
		}

		void push_n_false(const std::size_t n) noexcept
		{
			const std::size_t new_size = this->size_ + n;
			if (this->size_ > local_max_size) {
				if (new_size > this->capacity) [[unlikely]] {
					this->unsave_reallocate(std::max(this->capacity * 2u, std::bit_ceil(new_size)));
				}
			}
			else if (new_size > local_max_size) [[unlikely]] {
				this->unsave_reallocate(std::max(local_max_size * 2u, std::bit_ceil(new_size)));
			}
			this->size_ = new_size;
		}

		[[deprecated]] void push_n_true(const std::size_t n) noexcept
		{
			assert(n <= 64u && n > 0u); //makes it easier for now (enables mask to be std::uint64_t)
			const std::uint64_t mask = -1ull >> (64u - n);
			const std::uint64_t lower_half = mask << (this->size_ % 64u);
			const std::uint64_t upper_half = (mask >> 1u) >> (63u - this->size_ % 64u);
			this->push_n_false(n);
			if (upper_half) {
				this->data_[this->last_bitset_idx() - 1u] |= lower_half;
				this->data_[this->last_bitset_idx()     ] = upper_half;
			}
			else {
				this->data_[this->last_bitset_idx()] |= lower_half;
			}
		}

		constexpr std::size_t count() const noexcept
		{
			std::size_t result = 0u;
			for (std::size_t i = 0u; i <= this->last_bitset_idx(); i++) {
				result += this->data_[i].count();
			}
			return result;
		}

		constexpr std::size_t find_first_true() const noexcept
		{
			std::size_t array_idx = 0u;
			for (; array_idx < this->last_bitset_idx(); array_idx++) {
				const std::size_t bit_idx = this->data_[array_idx].find_first_true();
				if (bit_idx != BitSet64::npos) { 
					return array_idx * 64u + bit_idx; 
				}
			}
			const std::size_t bit_idx = this->data_[array_idx].find_first_true();
			const std::size_t res_idx = array_idx * 64u + bit_idx;
			return (res_idx < this->size_) ? res_idx : -1ull;
		}

		constexpr std::size_t find_first_false() const noexcept
		{
			std::size_t array_idx = 0u;
			for (; array_idx < this->last_bitset_idx(); array_idx++) {
				const std::size_t bit_idx = this->data_[array_idx].find_first_false();
				if (bit_idx != BitSet64::npos) { 
					return array_idx * 64u + bit_idx; 
				}
			}
			const std::size_t bit_idx = this->data_[array_idx].find_first_false();
			const std::size_t res_idx = array_idx * 64u + bit_idx;
			return (res_idx < this->size_) ? res_idx : -1ull;
		}

		//if a false bit exists, this is set and its position is returned.
		//else a true bit is appended and its position returned
		std::size_t set_first_false() noexcept
		{
			const std::size_t pos = this->find_first_false();
			if (pos != -1ull) {
				this->set(pos);
				return pos;
			}
			else {
				const std::size_t res_idx = this->size_;
				this->push_true();
				return res_idx;
			}
		}

		//returns index of first subrange starting at an integer multiple of std::bit_ceil(n) 
		//  previously consisting of n consecutive false entries but now set to true
		//if there was no such sequence in this before, (false) allignment bits and n true entries are appended
		std::size_t set_first_n_alligned_false(const std::size_t n) noexcept
		{
			assert(n <= 64u && n > 0u); //makes it easier for now (enables mask to be std::uint64_t)

			const std::size_t n_ceil = std::bit_ceil(n);
			const std::uint64_t mask = -1ull >> (64u - n);
			{
				std::size_t array_idx = 0u;
				for (; array_idx < this->size_ / 64u; array_idx++) {
					const BitSet64 current = this->data_[array_idx];
					if (current.all()) {
						continue;
					}
					for (std::size_t offset = 0u; offset < 64u; offset += n_ceil) {
						if (!(current & (mask << offset))) {
							this->data_[array_idx] |= (mask << offset);
							return array_idx * 64u + offset;
						}
					}
				}

				assert(array_idx == this->last_bitset_idx() || this->size_ % 64u == 0u);
				for (std::size_t offset = 0u; offset < this->size_ % 64u; offset += n_ceil) {

					if (!(this->data_[array_idx] & (mask << offset))) {
						this->data_[array_idx] |= (mask << offset);
						return array_idx * 64u + offset;
					}
				}
			}		

			//found no sufficient subrange in existing bits -> append one at the end (alligned)
			const std::size_t allign_ammount = n_ceil - ((this->size_ - 1u) % n_ceil) - 1u;
			const std::size_t result_index = this->size_ + allign_ammount;
			this->push_n_false(allign_ammount + n);
			this->data_[result_index / 64u] |= mask << (result_index % 64u);
			return result_index;
		} //set_first_n_alligned_false()

		constexpr void reset_alligned_n(const std::size_t start, const std::size_t n) noexcept
		{
			assert(start % std::bit_ceil(n) == 0u && "not alligned");
			assert(n <= 64u && n > 0u); //makes it easier for now (enables mask to be std::uint64_t)

			const std::uint64_t mask = -1ull >> (64u - n); //last n bits set to true
			this->data_[start / 64u] &= ~(mask << (start % 64u)); 
		}
	}; //class BitVector




	//can be used like std::optional<double>, but with extra double operations
	struct OptDouble
	{
		static_assert(std::numeric_limits<double>::has_quiet_NaN);
		double val = std::numeric_limits<double>::quiet_NaN(); //default initialize to invalid state

		constexpr OptDouble(const double new_val) noexcept :val(new_val) {}
		constexpr OptDouble() noexcept = default;

		bool has_value() const noexcept { return !std::isnan(this->val); }
		explicit operator bool() const noexcept { return this->has_value(); }

		constexpr double& operator*() noexcept { return this->val; }
		constexpr const double& operator*() const noexcept { return this->val; }

		constexpr OptDouble operator+(const OptDouble snd) const noexcept { return this->val + snd.val; }
		constexpr OptDouble operator-(const OptDouble snd) const noexcept { return this->val - snd.val; }
		constexpr OptDouble operator*(const OptDouble snd) const noexcept { return this->val * snd.val; }
		constexpr OptDouble operator/(const OptDouble snd) const noexcept { return this->val / snd.val; }

		constexpr OptDouble operator+=(const OptDouble snd) noexcept { this->val += snd.val; return *this; }
		constexpr OptDouble operator-=(const OptDouble snd) noexcept { this->val -= snd.val; return *this; }
		constexpr OptDouble operator*=(const OptDouble snd) noexcept { this->val *= snd.val; return *this; }
		constexpr OptDouble operator/=(const OptDouble snd) noexcept { this->val /= snd.val; return *this; }
	}; //struct OptDouble

	//can be used like std::optional<std::complex<double>>, but with extra complex operations
	struct OptComplex
	{
		static_assert(std::numeric_limits<double>::has_quiet_NaN);
		std::complex<double> val = std::numeric_limits<double>::quiet_NaN(); //default initialize to invalid state

		constexpr OptComplex(const std::complex<double>& new_val) noexcept :val(new_val) {}
		constexpr OptComplex(const double new_val) noexcept :val(new_val) {}
		constexpr OptComplex() noexcept = default;

		bool has_value() const noexcept { return !std::isnan(this->val.real()); }
		explicit operator bool() const noexcept { return this->has_value(); }

		constexpr std::complex<double>& operator*() noexcept { return this->val; }
		constexpr std::complex<double>* operator->() noexcept { return &this->val; }
		constexpr const std::complex<double>& operator*() const noexcept { return this->val; }
		constexpr const std::complex<double>* operator->() const noexcept { return &this->val; }

		constexpr OptComplex operator+(const OptComplex& snd) const noexcept { return this->val + snd.val; }
		constexpr OptComplex operator-(const OptComplex& snd) const noexcept { return this->val - snd.val; }
		constexpr OptComplex operator*(const OptComplex& snd) const noexcept { return this->val * snd.val; }
		constexpr OptComplex operator/(const OptComplex& snd) const noexcept { return this->val / snd.val; }		

		constexpr OptComplex& operator+=(const OptComplex& snd) noexcept { this->val += snd.val; return *this; }
		constexpr OptComplex& operator-=(const OptComplex& snd) noexcept { this->val -= snd.val; return *this; }
		constexpr OptComplex& operator*=(const OptComplex& snd) noexcept { this->val *= snd.val; return *this; }
		constexpr OptComplex& operator/=(const OptComplex& snd) noexcept { this->val /= snd.val; return *this; }
	}; //struct OptComplex

} //namespace bmath::intern
