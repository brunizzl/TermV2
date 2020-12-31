#pragma once

#include <array>

namespace bmath::intern {
    
    
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

    
    
} //namespace bmath::intern