#pragma once

#include <array>
#include <string_view>

namespace bmath::intern {
    
    
	//taken (and adapted) from here: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0732r2.pdf
	template<std::size_t N> 
	struct StringLiteral :std::array<char, N>
	{
		//perhaps change to array reference of length N + 1? (+ 1 because '\0')
		constexpr StringLiteral(const char* init) { std::copy_n(init, N, this->data()); }

		constexpr StringLiteral(const std::array<char, N>& init) :std::array<char, N>(init) {}

		constexpr StringLiteral() { this->fill('\0'); }

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

		constexpr std::string_view to_view() const noexcept { return std::string_view(this->data(), N); }
	};

	template<std::size_t N1, std::size_t N2>
	constexpr StringLiteral<N1 - 1 + N2> operator+(const char(&fst)[N1], const StringLiteral<N2>& snd)
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
    
} //namespace bmath::intern