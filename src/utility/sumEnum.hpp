#pragma once

#include <type_traits>
#include <typeinfo>
#include <bit>
#include <compare>
#include <concepts>
#include <numeric>

#include "meta.hpp"
#include "typeDebug.hpp"

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
		struct ListMembers<SumEnum<Enums...>> { using type = ListMembers_t<Enums...>; };

		template<typename E> requires (!meta::InstanceOf<E, SumEnum>)
		struct ListMembers<E> { using type = List<E>; };

		template<> 
		struct ListMembers<> { using type = List<>; };

		static_assert(ListMembers_t<SumEnum<int, SumEnum<float, bool>>>{} == List<int, float, bool>{});


		/////////////////   MemberInfo

		namespace info_detail {
			template<Enumeratable E, std::size_t Begin, std::size_t End>
			struct MemberInfo
			{
				using type = E;
				constexpr std::size_t begin() const { return Begin; }
				constexpr std::size_t end() const { return End; }
			};

			template<Enumeratable, unsigned Begin, unsigned End, bool IncludeSelf>
			struct MakeMemberInfo;

			template<Enumeratable S, unsigned B, unsigned E, bool I>
			using MakeMemberInfo_t = typename MakeMemberInfo<S, B, E, I>::type;

			template<Enumeratable Head, typename... Tail, unsigned Begin, unsigned End, bool IncludeSelf>
			class MakeMemberInfo<SumEnum<Head, Tail...>, Begin, End, IncludeSelf>
			{
				using Base = SumEnum<Tail...>;
				static constexpr unsigned Split = Begin + (unsigned)Base::COUNT;
				using OwnInfo   = MemberInfo<SumEnum<Head, Tail...>, Begin, End>;
				using HeadInfos = MakeMemberInfo_t<Head, Split, End, true>;
				using TailInfos = MakeMemberInfo_t<Base, Begin, Split, false>;
				using SubInfos  = decltype(meta::concat(TailInfos{}, HeadInfos{}));
			public:
				using type = std::conditional_t<IncludeSelf, decltype(meta::cons<OwnInfo>(SubInfos{})), SubInfos>;
			};

			template<unsigned Begin, unsigned End>
			struct MakeMemberInfo<SumEnum<>, Begin, End, false> { using type = List<>; };

			template<Enumeratable E, unsigned Begin, unsigned End>
				requires (!meta::InstanceOf<E, SumEnum>)
			struct MakeMemberInfo<E, Begin, End, true> { using type = List<MemberInfo<E, Begin, End>>; };
		} //namespace info_detail

		template<meta::InstanceOf<SumEnum> E>
		constexpr auto generate_member_infos() { return info_detail::MakeMemberInfo_t<E, 0, (unsigned)E::COUNT, true>{}; }

		template<Enumeratable E, meta::InstanceOf<List> Infos>
		constexpr auto find_info(Infos is) 
		{ 
			return meta::find(is, [](auto i) { return std::is_same_v<typename decltype(i)::type, E>; } );
		}

	} //namespace enum_detail

	template<>
	class [[nodiscard]] SumEnum<>
	{
	protected:

		enum class Value :unsigned {} value; //only data held by all of SumEnum

	public:
		constexpr SumEnum(const Value e) noexcept :value(e) {}
		constexpr operator Value() const noexcept { return this->value; } //implicit conversion allows use in switch

		explicit constexpr SumEnum(const unsigned u) noexcept :value(static_cast<Value>(u)) {}
		explicit constexpr operator unsigned() const noexcept { return static_cast<unsigned>(this->value); }

		static constexpr Value COUNT = static_cast<Value>(0u);
	}; //class SumEnum<>


	template<enum_detail::EnumLike Enum, typename... TailEnums>
	class [[nodiscard]] SumEnum<Enum, TailEnums...> :public SumEnum<TailEnums...>
	{
		static_assert(meta::disjoint(enum_detail::ListMembers_t<Enum>{}, enum_detail::ListMembers_t<TailEnums...>{}),
			"No two parameters of SumEnum's parameter pack may contain the same type within (or be equal).");

		using Base = SumEnum<TailEnums...>;
		static constexpr unsigned this_offset = (unsigned)Base::COUNT;
		static constexpr unsigned next_offset = static_cast<unsigned>(Enum::COUNT) + this_offset;

	protected:
		using Value = typename Base::Value;

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
		static constexpr unsigned this_offset = (unsigned)Base::COUNT;
		static constexpr unsigned next_offset = 1u + this_offset; //Atom occupies one value

	protected:
		using Value = typename Base::Value;

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

	template<meta::InstanceOf<SumEnum> SumEnum_T, meta::InstanceOf<meta::List> RawCases>
	class EnumSwitch
	{
		enum class Value :unsigned {};
		
		template<meta::InstanceOf<meta::List> CasesPart> 
		static constexpr Value decide_impl(const SumEnum_T e, const CasesPart cs) noexcept
		{
			//std::cout << compact_type_name(typeid(cs).name()) << "\n";
			constexpr auto cases_count = meta::size(cs);
			if constexpr (cases_count.val() > 1) {
				constexpr auto mid_index = cases_count / meta::Int_<2>{};
				constexpr auto mid_type = meta::at(mid_index, cs);
				constexpr std::size_t split_value = mid_type.begin();

				if (static_cast<unsigned>(e) < split_value) {
					constexpr auto first_half = meta::take(mid_index, cs);
					return EnumSwitch::decide_impl(e, first_half);
				}
				else {
					constexpr auto second_half = meta::drop(mid_index, cs);
					return EnumSwitch::decide_impl(e, second_half);
				}
			}
			else {
				using ResultType = typename decltype(meta::head(cs))::type;
				assert(e.is<ResultType>());
				return EnumSwitch::template as<ResultType>;
			}
		}

	public:
		template<enum_detail::Enumeratable E> requires ((bool) meta::contains<E>(RawCases{}))
		static constexpr Value as = static_cast<Value>(meta::index_of<E>(RawCases{}).val());

		static constexpr Value decide(const SumEnum_T e) noexcept
		{
			constexpr auto all_infos = enum_detail::generate_member_infos<SumEnum_T>();
			constexpr auto cases = meta::filter(
				[](auto i) { return meta::contains<typename decltype(i)::type>(RawCases{}); },
				all_infos);
			return EnumSwitch::decide_impl(e, cases);
		}
	};



} //namespace bmath::intern