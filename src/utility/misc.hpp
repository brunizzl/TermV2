#pragma once


#include <exception>
#include <algorithm>
#include <cassert>
#include <array>
#include <complex>
#include <compare>
#include <bit>


#undef ASSERT

#ifdef NDEBUG

#define ASSERT(expr) ((void)0)

#else

#define ASSERT(expr) if (!(expr)) __debugbreak()

#endif


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
		const std::array<Struct_T, Size>& data, const SearchMemberPtr_T ptr, const Search_T key, 
        const Struct_T& null_val = {}) noexcept
	{
		const auto itr = std::find_if(begin(data), end(data), [key, ptr](const auto &v) { return v.*ptr == key; });
		return itr != end(data) ? *itr : null_val;
	}


	template<typename Key_T, Key_T NullKey, typename Val_T, std::size_t Size>
	struct StupidLinearMap
	{
		std::array<Key_T, Size> keys;
		std::array<Val_T, Size> vals;

		constexpr StupidLinearMap() noexcept :vals{} {
			keys.fill(NullKey);
		}

		constexpr Val_T& at_or_insert(const Key_T& key) noexcept
		{
			assert(key != NullKey);
			for (std::size_t i = 0; i < Size; i++) {
				if (this->keys[i] == key) {
					return this->vals[i];
				}
				if (this->keys[i] == NullKey) {
					this->keys[i] = key;
					return this->vals[i];
				}
			}
			assert(false);
			return this->vals[0];
		}

		constexpr Val_T& at(const Key_T& key) noexcept
		{
			assert(key != NullKey);
			const auto key_iter = std::find(this->keys.begin(), this->keys.end(), key);
			assert(key_iter != this->keys.end());
			return this->vals[std::distance(this->keys.begin(), key_iter)];
		}

		constexpr const Val_T& at(const Key_T& key) const noexcept
		{
			assert(key != NullKey);
			const auto key_iter = std::find(this->keys.begin(), this->keys.end(), key);
			assert(key_iter != this->keys.end());
			return this->vals[std::distance(this->keys.begin(), key_iter)];
		}
	}; //struct StupidLinearMap


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