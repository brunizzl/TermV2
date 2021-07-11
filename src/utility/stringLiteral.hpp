#pragma once

#include <array>
#include <string_view>
#include <utility>

namespace simp {
    
	template<typename T, std::size_t... Is>
	constexpr auto array_from_ptr(std::index_sequence<Is...>, const T* init)
	{
		return std::array<T, sizeof...(Is)>{ init[Is]... };
	}
    
	//taken (and adapted) from here: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0732r2.pdf
	template<std::size_t N> 
	struct StringLiteral :std::array<char, N>
	{
		//perhaps change to array reference of length N + 1? (+ 1 because '\0')
		constexpr StringLiteral(const char* init) noexcept :std::array<char, N>(array_from_ptr(std::make_index_sequence<N>{}, init)) {}

		constexpr StringLiteral(const std::array<char, N>& init) noexcept :std::array<char, N>(init) {}

		constexpr StringLiteral() noexcept :std::array<char, N>{} {}

		template<std::size_t Start, std::size_t Length>
		constexpr StringLiteral<Length> substr() const noexcept 
		{ 
			static_assert(Start + Length <= N);
			return StringLiteral<Length>(this->data() + Start);
		}

		template<std::size_t N2>
		constexpr StringLiteral<N + N2> operator+(const StringLiteral<N2>& snd) const noexcept
		{
			StringLiteral<N + N2> res;
			const auto end = std::copy_n(this->data(), N, res.data());
			std::copy_n(snd.data(), N2, end);
			return res;
		}

		template<std::size_t N2>
		constexpr StringLiteral<N + N2 - 1> operator+(const char(&snd)[N2]) const noexcept
		{
			StringLiteral<N + N2 - 1> res;
			const auto end = std::copy_n(this->data(), N, res.data());
			std::copy_n(snd, N2 - 1, end);
			return res;
		}

		constexpr bool operator==(const StringLiteral&) const noexcept = default;

		template<std::size_t N2> requires (N != N2)
		constexpr bool operator==(const StringLiteral<N2>&) const noexcept { return false; }

		constexpr operator std::string_view() const noexcept { return std::string_view(this->data(), N); }
	};

	template<std::size_t N1, std::size_t N2>
	constexpr StringLiteral<N1 - 1 + N2> operator+(const char(&fst)[N1], const StringLiteral<N2>& snd) noexcept
	{
		StringLiteral<N1 - 1 + N2> res;
		const auto end = std::copy_n(fst, N1 - 1, res.data());
		std::copy_n(snd.data(), N2, end);
		return res;
	}

	template<std::size_t N>
	StringLiteral(const char (&str)[N]) -> StringLiteral<N - 1>;

	template<std::size_t N>
	StringLiteral(const std::array<char, N>&) -> StringLiteral<N>;

	static_assert("so freunde " + StringLiteral("herzlich willkommen") + " zur linearen algebra" == 
		StringLiteral("so freunde herzlich willkommen zur linearen algebra"));
    



	///////////////////////conversion functions

	

	template<unsigned long long Val>
	constexpr auto ull_to_string_literal()
	{
		constexpr auto digit_count = [](unsigned long long val) {
			unsigned long long count = 1;
			while (val > 9) {
				val /= 10;
				count++;
			}
			return count;
		};
		StringLiteral<digit_count(Val)> result;
		unsigned long long val = Val;
		std::size_t i = result.size() - 1;
		do {
			result[i--] = (val % 10) + '0';
			val /= 10;
		}while (val);
		return result;
	}

	static_assert(ull_to_string_literal<3>() == StringLiteral("3"));
	static_assert(ull_to_string_literal<0>() == StringLiteral("0"));
	static_assert(ull_to_string_literal<1000>() == StringLiteral("1000"));
	static_assert(ull_to_string_literal<656472>() == StringLiteral("656472"));

	namespace detail_to_string {
		template<double SmallVal>
		constexpr auto decimal_places()
		{
			static_assert(SmallVal < 1.0 && SmallVal >= 0.0);
			if constexpr (SmallVal == 0.0) {
				return StringLiteral("");
			}
			else {
				constexpr auto digits = [](double val) {
					unsigned count = 0;
					while (val != 0.0 && count < 5) {
						val *= 10;
						val -= (int)val;
						count++;
					}
					return count;
				}(SmallVal);
				StringLiteral<digits + 1> result;
				result.front() = '.';
				double val = SmallVal;
				for (std::size_t i = 1; i <= digits; i++) {
					val *= 10;
					result[i] = ((int)val % 10) + '0';
				}
				return result;
			}
		}

		static_assert(decimal_places<0.25>() == StringLiteral(".25"));
		static_assert(decimal_places<0.125>() == StringLiteral(".125"));
		static_assert(decimal_places<0.123425>() == StringLiteral(".12342"));
	} //namespace detail_to_string

	template<double Val, bool showpos = false>
	constexpr auto double_to_string_literal()
	{
		constexpr double abs_val = (Val < 0) ? -Val : Val;
		constexpr unsigned long long int_part = abs_val;
		constexpr double small_part = abs_val - int_part;
		constexpr auto positive_string = 
			ull_to_string_literal<int_part>() + 
			detail_to_string::decimal_places<small_part>();

		     if constexpr (Val < 0) { return "-" + positive_string; }
		else if constexpr (showpos) { return "+" + positive_string; }
		else                        { return positive_string; }
	}

	static_assert(double_to_string_literal<0.125>() == StringLiteral("0.125"));
	static_assert(double_to_string_literal<-201.125>() == StringLiteral("-201.125"));
	static_assert(double_to_string_literal<70.0>() == StringLiteral("70"));
	static_assert(double_to_string_literal<-70.0>() == StringLiteral("-70"));

	template<double Re, double Im>
	constexpr auto complex_to_string_literal()
	{
		if constexpr (Im == 0.0) {
			return double_to_string_literal<Re>();
		}
		else if constexpr (Re == 0.0) { 
			return double_to_string_literal<Im>() + "i"; 
		}
		else if constexpr (Re == 0.0 && Im == 0.0) {
			return StringLiteral("0");
		}
		else {
			return double_to_string_literal<Re>() + 
				double_to_string_literal<Im, true>() + "i";
		}
	}

#define BMATH_SV_TO_LITERAL(sv) StringLiteral<sv.size()>(sv.data())

	namespace detail_to_char_seq {
		template<template<auto...> class Res_T, StringLiteral S, std::size_t... Is>
		constexpr auto impl(std::index_sequence<Is...>)
		{
			return Res_T<S[Is]...>{};
		}
	} //namespace detail_to_char_seq

	template<template<auto...> class Res_T, StringLiteral S>
	struct ToCharSeq 
	{
		using type = decltype(detail_to_char_seq::impl<Res_T, S>(std::make_index_sequence<S.size()>{}));
	};

	template<template<auto...> class Res_T, StringLiteral S>
	using ToCharSeq_t = typename ToCharSeq<Res_T, S>::type;

} //namespace simp