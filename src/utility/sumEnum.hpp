#pragma once

#include <type_traits>
#include <bit>
#include <compare>
#include <concepts>
#include <numeric>
#include <algorithm>
#include <utility>

#include "meta.hpp"
#include "array.hpp"

namespace simp {
    
    

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

		constexpr std::strong_ordering operator<=>(const SumEnum&) const noexcept = default;
		constexpr bool operator==(const SumEnum&) const noexcept = default;
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


		constexpr std::strong_ordering operator<=>(const SumEnum&) const noexcept = default;
		constexpr bool operator==(const SumEnum&) const noexcept = default;
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


		constexpr std::strong_ordering operator<=>(const SumEnum&) const noexcept = default;
		constexpr bool operator==(const SumEnum&) const noexcept = default;
		static constexpr Value COUNT = static_cast<Value>(next_offset); //only relevant for outhermost instanciation
	}; //class SumEnum<Atom<T>, TailEnums...>





	template<typename Enum, Enum LastValidMember>
	struct [[nodiscard]] WrapEnum //allows to use SumEnum with enums not having their last member named COUNT
	{
		static_assert(std::is_enum_v<Enum>);

		Enum value;
		constexpr WrapEnum(const Enum e) noexcept :value(e) {}
		constexpr operator Enum() const noexcept { return this->value; }
		explicit constexpr WrapEnum(unsigned u) noexcept :value(static_cast<Enum>(u)) {}
		explicit constexpr operator unsigned() const noexcept { return static_cast<unsigned>(this->value); }

		//this is the only reason for WrapEnum to exist.
		static constexpr Enum COUNT = static_cast<Enum>(static_cast<unsigned>(LastValidMember) + 1u);
	}; //struct WrapEnum 


	//the only use of Identifier is to allow multiple OpaqueEnum types covering the same underlying Enum type
	template <int Identifier, detail_enum::Enumeratable Enum> 
	struct [[nodiscard]] OpaqueEnum
	{
		Enum value;
		constexpr OpaqueEnum(const Enum e) noexcept :value(e) {}
		constexpr operator Enum() const noexcept { return this->value; }
		explicit constexpr OpaqueEnum(unsigned u) noexcept :value(static_cast<Enum>(u)) {}
		explicit constexpr operator unsigned() const noexcept { return static_cast<unsigned>(this->value); }

		static constexpr auto COUNT = Enum::COUNT;
	}; //struct OpaqueEnum

	//in semantics quite close to native unsigned integer types but may only hold values smaller than COUNT
	template<unsigned COUNT_>
	struct FiniteUnsignedInt
	{
		unsigned value;
		constexpr FiniteUnsignedInt(const unsigned u) noexcept :value(u) {}
		constexpr operator unsigned() const noexcept { return this->value; }
		static constexpr unsigned COUNT = COUNT_;
	};


	//first Enum::COUNT values belong to Enum, the rest to not Enum -> thus no COUNT is needed
	template<detail_enum::EnumLike Enum>
	struct FinalSumEnum
	{
		enum class Value :unsigned {} value;

		//implicit conversion allows usage in switch case
		constexpr operator Value() const noexcept { return this->value; }

		constexpr FinalSumEnum(const Enum e) :value(Value(unsigned(e))) {}

		template<typename E>
		constexpr FinalSumEnum(const E e) noexcept :value(Value(unsigned(Enum(e)))) {}

		explicit constexpr FinalSumEnum(const unsigned u) noexcept :value(static_cast<Value>(u)) {}
		explicit constexpr operator unsigned() const noexcept { return static_cast<unsigned>(this->value); }

	private:
		template<typename E>
		static constexpr Value value_of() { return (Value)unsigned(Enum::template as<E>); }

	public:
		template<typename E>
		static constexpr Value as = FinalSumEnum::template value_of<E>();

		template<typename E> requires (!std::is_same_v<Enum, E>)
		constexpr E to() const noexcept { return Enum((unsigned)this->value).to<E>(); }

		template<typename E> requires (std::is_same_v<Enum, E>)
		constexpr E to() const noexcept { return Enum((unsigned)this->value); }

		template<typename E> requires (!std::is_same_v<Enum, E>)
		constexpr bool is() const noexcept { return Enum((unsigned)this->value).is<E>(); }

		template<typename E> requires (std::is_same_v<Enum, E>)
		constexpr bool is() const noexcept { return (unsigned)this->value < (unsigned)Enum::COUNT; }

		constexpr std::strong_ordering operator<=>(const FinalSumEnum&) const noexcept = default;
		constexpr bool operator==(const FinalSumEnum&) const noexcept = default;
	}; //struct FinalSumEnum




	template<
		InstanceOf<SumEnum> SumEnum_T,
		meta::ListInstance TypeCases,
		meta::SeqInstance ValueCases = meta::Seq<>>
	class EnumSwitch
	{
		enum class CaseIdentifier :unsigned {};

		template<CaseIdentifier ID, std::size_t Begin, std::size_t End>
		struct Option_ 
		{
			static constexpr CaseIdentifier id = ID;
			static constexpr std::size_t begin_ = Begin;
			static constexpr std::size_t end_ = End;
		};

		template<typename E>
		static constexpr CaseIdentifier type_identifier()
		{
			static_assert(meta::index_of_v<E, TypeCases> != meta::size_v<TypeCases>, 
				"only types passed in the template arguments are valid");
			return static_cast<CaseIdentifier>(meta::index_of_v<E, TypeCases>);
		}

		template<auto e>
		static constexpr CaseIdentifier value_identifier()
		{
			static_assert(meta::sindex_of_v<e, ValueCases> != meta::ssize_v<ValueCases>, 
				"only enum values passed in the template arguments are valid");
			return static_cast<CaseIdentifier>(meta::size_v<TypeCases> + meta::sindex_of_v<e, ValueCases>);
		}

		class MakeAllOptions
		{
			using AllInfos = detail_enum::MemberInfos_t<SumEnum_T>;

			template<typename T>
			struct IsInTypeCases :std::bool_constant<meta::contains_v<typename T::type, TypeCases>> {};

			using UsedInfos = meta::Filter_t<IsInTypeCases, AllInfos>;
			static_assert(meta::size_v<UsedInfos> == meta::size_v<TypeCases>,
				"every type case must stand for a unique range in SumEnum_T");

			template<typename T>
			struct InfoToOption { using type = Option_<
				EnumSwitch::type_identifier<typename T::type>(), 
				T::begin_, 
				T::end_
			>; };

			template<auto val>
			struct ValueToOption { using type = Option_<
				EnumSwitch::value_identifier<val>(), 
				(unsigned)SumEnum_T(val), 
				(unsigned)SumEnum_T(val) + 1
			>; };

			using TypeOptions = meta::Map_t<InfoToOption, UsedInfos>;
			using ValueOptions = meta::SeqToList_t<ValueToOption, ValueCases>;
			using UnorderedOptions = meta::Concat_t<TypeOptions, ValueOptions>;

			template<typename O1, typename O2>
			struct CompareOptions :std::bool_constant<(O1::begin_ < O2::begin_)> {};

			using Options = meta::Sort_t<UnorderedOptions, CompareOptions>;

			template<typename O>
			struct OptionToRange { static constexpr auto value = std::pair{ O::begin_, O::end_ }; };

			static constexpr auto reached_values = []() {
				const auto ranges = arr::from_list_v<std::pair<std::size_t, std::size_t>, OptionToRange, Options>;
				std::array<int, (unsigned)SumEnum_T::COUNT> result = {};
				for (const auto range : ranges) {
					for (std::size_t i = range.first; i < range.second; i++) {
						result[i]++;
					}
				}
				return result;
			}();
			static_assert(std::all_of(reached_values.begin(), reached_values.end(), [](auto x) { return x >= 1; }),
				"every case must be covered");
			static_assert(std::all_of(reached_values.begin(), reached_values.end(), [](auto x) { return x <= 1; }),
				"no case may be covered twice");

		public:
			using type = Options;
		};

		using AllOptions = typename MakeAllOptions::type;

		template<meta::ListInstance Options>
		static constexpr CaseIdentifier decide_subrange(const SumEnum_T e) noexcept
		{
			if constexpr (meta::size_v<Options> > 1u) {
				constexpr std::size_t mid = meta::size_v<Options> / 2u;
				using FstHalf = meta::Take_t<mid, Options>;
				using SndHalf = meta::Drop_t<mid, Options>;

				return (unsigned)e < (meta::Head_t<SndHalf>::begin_) ?
					EnumSwitch::decide_subrange<FstHalf>(e) :
					EnumSwitch::decide_subrange<SndHalf>(e);
			}
			else {
				return meta::Head_t<Options>::id;
			}
		}

	public:
		static constexpr CaseIdentifier decide(const SumEnum_T e) noexcept 
		{ 
			return EnumSwitch::decide_subrange<AllOptions>(e);
		}

		template<typename E>
		static constexpr CaseIdentifier is_type = EnumSwitch::type_identifier<E>();
		
		template<auto e>
		static constexpr CaseIdentifier is_value = EnumSwitch::value_identifier<e>();
	}; //class EnumSwitch


} //namespace simp