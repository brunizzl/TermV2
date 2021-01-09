#pragma once

#include <type_traits>
#include <typeinfo>
#include <bit>
#include <compare>
#include <concepts>

namespace bmath::intern {
    
    

	template<typename... Enums>
	class [[nodiscard]] SumEnum;

	template<typename Enum, Enum LastValidMember>
	struct [[nodiscard]] WrapEnum //allows to use SumEnum with enums not having their last member named COUNT
	{
		static_assert(std::is_enum_v<Enum>);

		Enum value;
		constexpr WrapEnum(const Enum e) noexcept :value(e) {}

		explicit constexpr WrapEnum(const unsigned u) noexcept :value(static_cast<Enum>(u)) {}
		explicit constexpr operator unsigned() const noexcept { return static_cast<unsigned>(this->value); }

		//this is the only reason for WrapEnum to exist.
		static constexpr Enum COUNT = static_cast<Enum>(static_cast<unsigned>(LastValidMember) + 1u);
	}; //struct WrapEnum 

	namespace enum_detail {

		template<typename Needle, typename Haystack>
		struct DecideContainedIn :std::false_type {};

		template<typename Needle, typename... HayTail>
		struct DecideContainedIn<Needle, SumEnum<Needle, HayTail...>> :std::true_type {};

		template<typename Needle, typename HayHead, typename... HayTail>
		struct DecideContainedIn<Needle, SumEnum<HayHead, HayTail...>> :std::bool_constant<
			DecideContainedIn<Needle, HayHead>::value || 
			DecideContainedIn<Needle, SumEnum<HayTail...>>::value
		> {};

		template<typename Needle, typename Haystack>
		concept ContainedIn = DecideContainedIn<Needle, Haystack>::value;



		template<typename Specialized, template<typename...> class Template>
		struct DecideInstanceOf :std::false_type {};

		template<template<typename...> class Template, typename... Args>
		struct DecideInstanceOf<Template<Args...>, Template> :std::true_type {};

		template<typename Specialized, template<typename...> class Template>
		concept InstanceOf = DecideInstanceOf<Specialized, Template>::value;


		template<typename Enum>
		concept InteroperableEnum = std::is_enum_v<Enum> &&
			requires { Enum::COUNT; };

		template<typename T>
		concept SumEnumLike = InstanceOf<T, SumEnum> || InstanceOf<T, WrapEnum> || InteroperableEnum<T>;

		template<typename T>
		concept SumEnumAtom = !SumEnumLike<T> && !std::is_enum_v<T>;





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
		static_assert(enum_detail::SumEnumLike<Enum>, "\n"\
			"SumEnum may only be instanciated with the following types: \n"\
			"   1. an instance of SumEnum\n"\
			"   2. an enum class with its last value named \"COUNT\", thus acting like SumEnum\n"\
			"   3. a struct / class clearly not modeling an enum (represented in SumEnum as single value)\n"\
			"note 1: likely you passed in an enum class but forgot to add \"COUNT\" as last member\n"\
			"note 2: Enum::COUNT is only expected to be used as \"number of elements\" in SumEnum context, not as valid value of enum\n"\
			"note 3: option 3 is called \"Atom\" in SumEnum templates\n");

		static_assert(enum_detail::disjoint_v<enum_detail::ListMembers<Enum>::type, enum_detail::ListMembers<TailEnums...>::type>, 
			"No two parameters of SumEnum's parameter pack may contain the same type within (or be equal).");

		using Base = SumEnum<TailEnums...>;
		static constexpr unsigned this_offset = Base::next_offset;

	protected:
		using Value = typename Base::Value;
		static constexpr unsigned next_offset = static_cast<unsigned>(Enum::COUNT) + this_offset;

	private:
		template<enum_detail::ContainedIn<Base> E>
		static constexpr Value value_of() { return Base::template as<E>; }

		template<enum_detail::ContainedIn<Enum> E>
		static constexpr Value value_of() { return static_cast<Value>(static_cast<unsigned>(Enum::template as<E>) + this_offset); }

		template<std::same_as<Enum> E>
		static constexpr Value value_of() { static_assert(false, "more than one value represents requested type"); }

	public:
		using Base::Base;

		constexpr SumEnum(const Enum e) noexcept :Base(static_cast<unsigned>(e) + this_offset) {}

		//this constructor applies if Enum itself is WrapEnum<E> or SumEnum<...> that contains E (directly or deeper within)
		template<enum_detail::ContainedIn<Enum> E>
		constexpr SumEnum(const E e) noexcept :Base(static_cast<unsigned>(static_cast<Enum>(e)) + this_offset) {}

		//this constructor applies if E is contained in Base
		template<enum_detail::ContainedIn<Base> E>
		constexpr SumEnum(const E e) noexcept :Base(e) {}


		template<typename E>
		static constexpr Value as = SumEnum::template value_of<E>();


		//E is contained in Base -> hand over to Base
		template<enum_detail::ContainedIn<Base> E>
		constexpr E to() const noexcept { return static_cast<const Base>(*this).to<E>(); }

		//Enum itself is SumEnum<...> and contains E -> hand over to Enum
		template<enum_detail::ContainedIn<Enum> E>
		constexpr E to() const noexcept { return this->to<Enum>().to<E>(); }

		//E is same as Enum -> just undo the offset
		template<std::same_as<Enum> E>
		constexpr E to() const noexcept { return Enum(static_cast<unsigned>(this->value) - this_offset); }


		//default case: search in parent types
		template<enum_detail::ContainedIn<Base> E>
		constexpr bool is() const noexcept { return static_cast<const Base>(*this).is<E>(); }

		//Enum itself is SumEnum<...> and contains E -> hand over to Enum
		template<enum_detail::ContainedIn<Enum> E>
		constexpr bool is() const noexcept { return this->to<Enum>().is<E>(); }

		//E is same as Enum -> check if current value is between offsets
		template<std::same_as<Enum> E>
		constexpr bool is() const noexcept 
		{ 
			return static_cast<unsigned>(this->value) >= this_offset && 
				static_cast<unsigned>(this->value) < next_offset; 
		}


		constexpr friend std::strong_ordering operator<=>(const SumEnum&, const SumEnum&) noexcept = default;
		constexpr friend bool operator==(const SumEnum&, const SumEnum&) noexcept = default;
		static constexpr Value COUNT = static_cast<Value>(next_offset); //only relevant for outhermost instanciation
	}; //class SumEnum<Enum, TailEnums...>


	template<enum_detail::SumEnumAtom Atom, typename... TailEnums>
	class [[nodiscard]] SumEnum<Atom, TailEnums...> :public SumEnum<TailEnums...>
	{
		using Base = SumEnum<TailEnums...>;
		static constexpr unsigned this_offset = Base::next_offset;

	protected:
		using Value = typename Base::Value;
		static constexpr unsigned next_offset = 1u + this_offset;

	private:
		template<enum_detail::ContainedIn<Base> E>
		static constexpr Value value_of() { return Base::template as<E>; }

		template<std::same_as<Atom> E>
		static constexpr Value value_of() { return static_cast<Value>(this_offset); }

	public:
		using Base::Base;

		constexpr SumEnum(const Atom) noexcept :Base(this_offset) 
		{ 
			static_assert(std::is_empty_v<Atom>, 
				"please use \"SumEnum<...>::as<Atom>\" to create SumEnum with value representing non-empty Atom type"); 
		}

		//this constructor applies if E is contained in Base
		template<enum_detail::ContainedIn<Base> E>
		constexpr SumEnum(const E e) noexcept :Base(e) {}


		template<typename E>
		static constexpr Value as = SumEnum::template value_of<E>();


		//E is contained in Base -> hand over to Base
		template<enum_detail::ContainedIn<Base> E>
		constexpr E to() const noexcept { return static_cast<const Base>(*this).to<E>(); }

		template<std::same_as<Atom> E>
		constexpr E to() const noexcept { static_assert(false, "SumEnum can only convert to enum-like types"); }


		//default case: search in parent types
		template<enum_detail::ContainedIn<Base> E>
		constexpr bool is() const noexcept { return static_cast<const Base>(*this).is<E>(); }

		//E is same as Enum -> check if current value is between offsets
		template<std::same_as<Atom> E>
		constexpr bool is() const noexcept { return static_cast<unsigned>(this->value) == this_offset; }


		constexpr friend std::strong_ordering operator<=>(const SumEnum&, const SumEnum&) noexcept = default;
		constexpr friend bool operator==(const SumEnum&, const SumEnum&) noexcept = default;
		static constexpr Value COUNT = static_cast<Value>(next_offset); //only relevant for outhermost instanciation
	}; //class SumEnum<Atom, TailEnums...>

} //namespace bmath::intern