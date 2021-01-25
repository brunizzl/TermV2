#pragma once

#include <type_traits>
#include <bit>
#include <compare>
#include <concepts>
#include <numeric>
#include <algorithm>

#include "meta.hpp"
#include "array.hpp"

namespace bmath::intern {
    
    

	template<typename... Enums>
	class [[nodiscard]] SumEnum;

	struct SingleSumEnumEntry {}; //marker if you want to use Type as enum value (Type has to inherit from here)

	namespace detail_enum {

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
			{e}           -> ExplicitlyConvertibleTo<unsigned>;
			{Enum::COUNT} -> ExplicitlyConvertibleTo<unsigned>;
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
			using type = meta::Concat_t<ListMembers_t<Head>, ListMembers_t<Tail...>>;
		};

		template<typename... Enums>
		struct ListMembers<SumEnum<Enums...>> { using type = ListMembers_t<Enums...>; };

		template<typename E> requires (!InstanceOf<E, SumEnum>)
		struct ListMembers<E> { using type = List<E>; };

		template<> 
		struct ListMembers<> { using type = List<>; };

		static_assert(std::is_same_v<ListMembers_t<SumEnum<int, SumEnum<float, bool>>>, List<int, float, bool>>);


		/////////////////   MemberInfo

		template<Enumeratable E, std::size_t Begin, std::size_t End>
		struct MemberInfo
		{
			using type = E;
			static constexpr std::size_t begin_ = Begin;
			static constexpr std::size_t end_ = End;
		};

		namespace detail_info {
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
				using SubInfos  = meta::Concat_t<TailInfos, HeadInfos>;
			public:
				using type = std::conditional_t<IncludeSelf, meta::Cons_t<OwnInfo, SubInfos>, SubInfos>;
			};

			template<unsigned Begin, unsigned End>
			struct MakeMemberInfo<SumEnum<>, Begin, End, false> { using type = List<>; };

			template<Enumeratable E, unsigned Begin, unsigned End>
				requires (!InstanceOf<E, SumEnum>)
			struct MakeMemberInfo<E, Begin, End, true> { using type = List<MemberInfo<E, Begin, End>>; };
		} //namespace detail_info

		template<InstanceOf<SumEnum> E>
		using MemberInfos_t = detail_info::MakeMemberInfo_t<E, 0, (unsigned)E::COUNT, true>;

	} //namespace detail_enum

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


	template<detail_enum::EnumLike Enum, typename... TailEnums>
	class [[nodiscard]] SumEnum<Enum, TailEnums...> :public SumEnum<TailEnums...>
	{
		static_assert(meta::disjoint_v<detail_enum::ListMembers_t<Enum>, detail_enum::ListMembers_t<TailEnums...>>,
			"No two parameters of SumEnum's parameter pack may contain the same type within (or be equal).");

		using Base = SumEnum<TailEnums...>;
		static constexpr unsigned this_offset = (unsigned)Base::COUNT;
		static constexpr unsigned next_offset = static_cast<unsigned>(Enum::COUNT) + this_offset;

	public:
		using Value = typename Base::Value;

	private:
		template<detail_enum::ContainedIn<Base> E>
		static constexpr Value value_of() { return Base::template as<E>; }

		template<detail_enum::ContainedIn<Enum> E>
		static constexpr Value value_of() { return static_cast<Value>(static_cast<unsigned>(Enum::template as<E>) + this_offset); }

		template<std::same_as<Enum> E>
		static constexpr Value value_of() { static_assert(false, "more than one value represents requested type"); }

	public:
		using Base::Base;

		constexpr SumEnum(const Enum e) noexcept :Base(static_cast<unsigned>(e) + this_offset) {}

		//this constructor applies if Enum itself is WrapEnum<E> or SumEnum<...> that contains E (directly or deeper within)
		template<detail_enum::ContainedIn<Enum> E>
		constexpr SumEnum(const E e) noexcept :Base(static_cast<unsigned>(static_cast<Enum>(e)) + this_offset) {}

		//this constructor applies if E is contained in Base
		template<detail_enum::ContainedIn<Base> E>
		constexpr SumEnum(const E e) noexcept :Base(e) {}


		template<typename E>
		static constexpr Value as = SumEnum::template value_of<E>();


		//E is contained in Base -> hand over to Base
		template<detail_enum::ContainedIn<Base> E>
		constexpr E to() const noexcept { return static_cast<const Base>(*this).to<E>(); }

		//Enum itself is SumEnum<...> and contains E -> hand over to Enum
		template<detail_enum::ContainedIn<Enum> E>
		constexpr E to() const noexcept { return this->to<Enum>().to<E>(); }

		//E is same as Enum -> just undo the offset
		template<std::same_as<Enum> E>
		constexpr E to() const noexcept { return Enum(static_cast<unsigned>(this->value) - this_offset); }


		//default case: search in parent types
		template<detail_enum::ContainedIn<Base> E>
		constexpr bool is() const noexcept { return static_cast<const Base>(*this).is<E>(); }

		//Enum itself is SumEnum<...> and contains E -> hand over to Enum
		template<detail_enum::ContainedIn<Enum> E>
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




	template<detail_enum::Atom T, typename... TailEnums>
	class [[nodiscard]] SumEnum<T, TailEnums...> :public SumEnum<TailEnums...>
	{
		using Base = SumEnum<TailEnums...>;
		static constexpr unsigned this_offset = (unsigned)Base::COUNT;
		static constexpr unsigned next_offset = 1u + this_offset; //Atom occupies one value

	public:
		using Value = typename Base::Value;

	private:
		template<detail_enum::ContainedIn<Base> E>
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
		template<detail_enum::ContainedIn<Base> E>
		constexpr SumEnum(const E e) noexcept :Base(e) {}


		template<typename E>
		static constexpr Value as = SumEnum::template value_of<E>();


		//E is contained in Base -> hand over to Base
		template<detail_enum::ContainedIn<Base> E>
		constexpr E to() const noexcept { return static_cast<const Base>(*this).to<E>(); }

		template<std::same_as<T> E>
		constexpr E to() const noexcept { static_assert(false, "SumEnum can only convert to enum-like types"); }


		//default case: search in parent types
		template<detail_enum::ContainedIn<Base> E>
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



	template <detail_enum::Enumeratable Enum>
	class [[nodiscard]] OpaqueEnum
	{
		enum class Value :unsigned {} value;

	public:
		constexpr OpaqueEnum(const Enum e) noexcept :value(static_cast<Value>(static_cast<unsigned>(e))) {}
		constexpr operator Enum() const noexcept { return static_cast<unsigned>(this->value); }
		explicit constexpr operator unsigned() const noexcept { return static_cast<unsigned>(this->value); }

		static constexpr Value COUNT = static_cast<Value>(static_cast<unsigned>(Enum::COUNT));
	}; //struct OpaqueEnum






	template<
		InstanceOf<SumEnum> SumEnum_T,
		meta::ListInstance TypeCases,
		std::array ValueCases = std::array<unsigned, 0>{}>
	class EnumSwitch
	{
		enum class CaseIdentifier :unsigned {};

		struct Option
		{
			CaseIdentifier identifier;
			std::size_t begin_, end_;
		};

		template<typename E>
		static constexpr CaseIdentifier type_identifier()
		{
			static_assert(meta::index_of_v<E, TypeCases> != -1, "only types passed in the template arguments are valid");
			return static_cast<CaseIdentifier>(meta::index_of_v<E, TypeCases>);
		}

		template<SumEnum_T e>
		static constexpr CaseIdentifier value_identifier()
		{
			static_assert(arr::index_of(e, ValueCases) != -1, "only enum values passed in the template arguments are valid");
			return static_cast<CaseIdentifier>(meta::size_v<TypeCases> + arr::index_of(e, ValueCases));
		}


		template<typename T>
		struct IsInTypeCases :std::bool_constant<meta::contains_v<typename T::type, TypeCases>> {};

		template<typename T>
		struct InfoToOption
		{
			static constexpr Option value = Option{ 
				EnumSwitch::type_identifier<typename T::type>(), T::begin_, T::end_ };
		}; 
		
		template<SumEnum_T val>
		struct ValueToOption
		{
			static constexpr Option value = Option{
				EnumSwitch::value_identifier<val>(), (unsigned)val, (unsigned)val + 1 };
		};

		static constexpr auto compute_all_options()
		{
			using AllInfos = detail_enum::MemberInfos_t<SumEnum_T>;
			using UsedInfos = meta::Filter_t<IsInTypeCases, AllInfos>;

			constexpr std::array type_options = arr::from_list_v<InfoToOption, UsedInfos>;
			constexpr std::array value_options = arr::map_v<Option, ValueToOption, ValueCases>;

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
			static_assert(std::all_of(reached.begin(), reached.end(), [](auto x) { return x >= 1; }), 
				"every case must be covered");
			static_assert(std::all_of(reached.begin(), reached.end(), [](auto x) { return x <= 1; }), 
				"no case may be covered twice");

			return options;
		}
		static constexpr auto all_options = compute_all_options();

		template<std::array Options>
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
		
		template<auto e>
		static constexpr CaseIdentifier is_value = EnumSwitch::value_identifier<SumEnum_T(e)>();
	}; //class EnumSwitch



} //namespace bmath::intern