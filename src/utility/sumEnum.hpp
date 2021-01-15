#pragma once

#include <type_traits>
#include <typeinfo>
#include <bit>
#include <compare>
#include <concepts>

#include "meta.hpp"

namespace bmath::intern {
    
    

	template<typename... Enums>
	class [[nodiscard]] SumEnum;

	struct SingleSumEnumEntry {}; //marker if you want to use Type as enum value (Type has to inherit from here)

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


		template<typename Enum>
		concept EnumLike = requires (Enum e) {
			{e}           -> meta::ExplicitlyConvertibleTo<unsigned>;
			{Enum::COUNT} -> meta::ExplicitlyConvertibleTo<unsigned>;
		};

		template<typename T>
		concept Atom = std::is_base_of_v<SingleSumEnumEntry, T>;

		template<typename T>
		concept Enumeratable = Atom<T> || EnumLike<T>;


		/////////////////   ListMembers

		using meta::List;

		template<typename...> 
		struct ListMembers;

		template<typename... Args>
		using ListMembers_t = typename ListMembers<Args...>::type;

		template<typename Head, typename... Tail>
		struct ListMembers<Head, Tail...> 
		{ 
			using type = decltype(meta::concat(ListMembers_t<Head>{}, ListMembers_t<Tail...>{}));
		};

		template<typename... Enums>
		struct ListMembers<SumEnum<Enums...>> 
		{ 
			using type = decltype(meta::concat(List<SumEnum<Enums...>>{}, ListMembers_t<Enums...>{}));
		};

		template<typename Enum>
		struct ListMembers<Enum> { using type = List<Enum>; };

		template<> 
		struct ListMembers<> { using type = List<>; };

		static_assert(ListMembers_t<SumEnum<int, SumEnum<float, bool>>>{} == 
			List<SumEnum<int, SumEnum<float, bool>>, int, SumEnum<float, bool>, float, bool>{});

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


	template<enum_detail::EnumLike Enum, typename... TailEnums>
	class [[nodiscard]] SumEnum<Enum, TailEnums...> :public SumEnum<TailEnums...>
	{
		static_assert(meta::disjoint(enum_detail::ListMembers_t<Enum>{}, enum_detail::ListMembers_t<TailEnums...>{}),
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


	template<enum_detail::Atom T, typename... TailEnums>
	class [[nodiscard]] SumEnum<T, TailEnums...> :public SumEnum<TailEnums...>
	{
		using Base = SumEnum<TailEnums...>;
		static constexpr unsigned this_offset = Base::next_offset;

	protected:
		using Value = typename Base::Value;
		static constexpr unsigned next_offset = 1u + this_offset; //Atom occupies one value

	private:
		template<enum_detail::ContainedIn<Base> E>
		static constexpr Value value_of() { return Base::template as<E>; }

		template<std::same_as<T> E>
		static constexpr Value value_of() { return static_cast<Value>(this_offset); }

	public:
		using Base::Base;

		constexpr SumEnum(const T) noexcept :Base(this_offset) 
		{ 
			static_assert(std::is_empty_v<T>, 
				"please use \"SumEnum<...>::as<T>\" to create SumEnum with value representing non-empty T type"); 
		}

		//this constructor applies if E is contained in Base
		template<enum_detail::ContainedIn<Base> E>
		constexpr SumEnum(const E e) noexcept :Base(e) {}


		template<typename E>
		static constexpr Value as = SumEnum::template value_of<E>();


		//E is contained in Base -> hand over to Base
		template<enum_detail::ContainedIn<Base> E>
		constexpr E to() const noexcept { return static_cast<const Base>(*this).to<E>(); }

		template<std::same_as<T> E>
		constexpr E to() const noexcept { static_assert(false, "SumEnum can only convert to enum-like types"); }


		//default case: search in parent types
		template<enum_detail::ContainedIn<Base> E>
		constexpr bool is() const noexcept { return static_cast<const Base>(*this).is<E>(); }

		//E is same as Enum -> check if current value is between offsets
		template<std::same_as<T> E>
		constexpr bool is() const noexcept { return static_cast<unsigned>(this->value) == this_offset; }


		constexpr friend std::strong_ordering operator<=>(const SumEnum&, const SumEnum&) noexcept = default;
		constexpr friend bool operator==(const SumEnum&, const SumEnum&) noexcept = default;
		static constexpr Value COUNT = static_cast<Value>(next_offset); //only relevant for outhermost instanciation
	}; //class SumEnum<Atom<T>, TailEnums...>





	template<typename Enum, Enum LastValidMember>
	struct [[nodiscard]] WrapEnum //allows to use SumEnum with enums not having their last member named COUNT
	{
		static_assert(std::is_enum_v<Enum>);

		Enum value;
		constexpr WrapEnum(const Enum e) noexcept :value(e) {}
		constexpr operator Enum() const noexcept { return this->value; }
		explicit constexpr operator unsigned() const noexcept { return static_cast<unsigned>(this->value); }

		//this is the only reason for WrapEnum to exist.
		static constexpr Enum COUNT = static_cast<Enum>(static_cast<unsigned>(LastValidMember) + 1u);
	}; //struct WrapEnum 

	template <enum_detail::Enumeratable Enum>
	class [[nodiscard]] OpaqueEnum
	{
		enum class Value :unsigned {} value;

	public:
		constexpr OpaqueEnum(const Enum e) noexcept :value(static_cast<Value>(static_cast<unsigned>(e))) {}
		constexpr operator Enum() const noexcept { return static_cast<unsigned>(this->value); }
		explicit constexpr operator unsigned() const noexcept { return static_cast<unsigned>(this->value); }

		static constexpr Value COUNT = static_cast<Value>(static_cast<unsigned>(Enum::COUNT));
	}; //struct OpaqueEnum




	template<meta::InstanceOf<SumEnum> SumEnum_T, meta::InstanceOf<meta::List> Cases>
	class EnumSwitch
	{
		enum class Value :unsigned {};

	public:
		template<enum_detail::Enumeratable E> requires ((bool) meta::contains<E>(Cases{}))
		static constexpr Value as = static_cast<Value>(meta::value(meta::index<E>(Cases{})));

		static constexpr Value decide(const SumEnum_T e) noexcept
		{
			return EnumSwitch::decide_impl(e);
		}

	private:
		template<typename Head, typename... Tail>
		static constexpr Value decide_impl(const SumEnum<Head, Tail...> e) noexcept
		{
			if constexpr (sizeof...(Tail) > 0u) {
				if (e.is<Head>()) {
					return EnumSwitch::template as<Head>;
				}
				else {
					return EnumSwitch::decide_impl(SumEnum<Tail...>(static_cast<unsigned>(e)));
				}
			}
			else {
				assert(e.is<Head>());
				return EnumSwitch::template as<Head>;
			}
		}
	};



} //namespace bmath::intern