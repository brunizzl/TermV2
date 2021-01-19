#pragma once

#include <type_traits>
#include <bit>
#include <compare>
#include <concepts>
#include <numeric>
#include <algorithm>
#include <array>

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

		template<Enumeratable E, std::size_t Begin, std::size_t End>
		struct MemberInfo
		{
			using type = E;
			constexpr std::size_t begin() const { return Begin; }
			constexpr std::size_t end() const { return End; }
		};

		namespace info_detail {
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
	struct [[nodiscard]] SumEnum<>
	{
		enum class Value :unsigned {} value; //only data held by all of SumEnum

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

	public:
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

	public:
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






	template<meta::InstanceOf<SumEnum> SumEnum_T, meta::InstanceOf<meta::List> TypeCases, SumEnum_T... ValueCases>
	class EnumSwitch
	{
		enum class CaseIdentifier :unsigned {};

		struct Option
		{
			CaseIdentifier identifier;
			std::size_t begin_, end_;
		};

		static constexpr std::array value_cases = arr::make<unsigned, (unsigned)ValueCases...>();

		template<typename E>
		static constexpr CaseIdentifier type_identifier()
		{
			static_assert(meta::index_of<E>(TypeCases{}).val() != -1, "only types passed in the template arguments are valid");
			return static_cast<CaseIdentifier>(meta::index_of<E>(TypeCases{}).val());
		}

		static constexpr CaseIdentifier value_identifier(unsigned e)
		{
			if (arr::index_of(e, value_cases) == -1) throw "only enum values passed in the template arguments are valid";
			return static_cast<CaseIdentifier>(TypeCases{}.size().val() + arr::index_of(e, value_cases));
		}

		static constexpr auto compute_all_options()
		{
			constexpr auto all_infos = enum_detail::generate_member_infos<SumEnum_T>();
			constexpr auto used_infos = meta::filter(
				[](auto i) { return meta::contains<typename decltype(i)::type>(TypeCases{}); },
				all_infos);
			constexpr std::array type_options = arr::from_list(
				[](auto x) { return Option{ EnumSwitch::type_identifier<typename decltype(x)::type>(), x.begin(), x.end() }; },
				used_infos);
			constexpr std::array value_options = arr::map(
				[](unsigned e) { return Option{ EnumSwitch::value_identifier(e), e, e + 1 }; },
				value_cases);
			constexpr auto make_options = [&value_options, &type_options]() {
				std::array res = arr::concat(type_options, value_options);
				std::sort(res.begin(), res.end(), [](Option a, Option b) { return a.begin_ < b.begin_; });
				return res;
			};
			constexpr std::array options = make_options();

			constexpr auto compute_reached = [&options]() {
				std::array<int, (unsigned)SumEnum_T::COUNT> res = {};
				for (const Option& option : options) {
					for (std::size_t i = option.begin_; i < option.end_; i++) {
						res[i]++;
					}
				}
				return res;
			};
			constexpr auto reached = compute_reached();
			static_assert(std::all_of(reached.begin(), reached.end(), [](auto x) { return x >= 1; }), "every case must be covered");
			static_assert(std::all_of(reached.begin(), reached.end(), [](auto x) { return x <= 1; }), "no case may be covered twice");

			return options;
		}
		static constexpr std::array all_options = compute_all_options();

		template<std::array Options> requires (arr::holds<Option>(Options))
		static constexpr CaseIdentifier decide_impl(const SumEnum_T e) noexcept
		{
			if constexpr (Options.size() > 1u) {
				constexpr std::size_t mid = Options.size() / 2u;

				if (static_cast<unsigned>(e) < Options[mid].begin_) {
					return EnumSwitch::decide_impl<arr::take<mid>(Options)>(e);
				}
				else {
					return EnumSwitch::decide_impl<arr::drop<mid>(Options)>(e);
				}
			}
			else {
				return Options.front().identifier;
			}
		}

	public:
		static constexpr CaseIdentifier decide(const SumEnum_T e) noexcept 
		{ 
			return EnumSwitch::decide_impl<all_options>(e); 
		}

		template<typename E>
		static constexpr CaseIdentifier is_type = EnumSwitch::type_identifier<E>();

		static constexpr CaseIdentifier is_value(SumEnum_T e) { return value_identifier((unsigned)e); }
	}; //class EnumSwitch



} //namespace bmath::intern