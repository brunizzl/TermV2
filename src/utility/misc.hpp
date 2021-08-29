#pragma once


#include <exception>
#include <algorithm>
#include <cassert>
#include <array>
#include <complex>
#include <compare>
#include <bit>

#include "array.hpp"
#include "meta.hpp"

#if defined(_MSC_VER)
#define BMATH_UNREACHABLE __assume(false)
#elif (defined(__GNUC__) || defined(__clang__))
#define BMATH_UNREACHABLE __builtin_unreachable()
#else
#define BMATH_UNREACHABLE ((void)0)
#endif


#if defined(_MSC_VER)
#define BMATH_FORCE_INLINE __forceinline
#else
#define BMATH_FORCE_INLINE __attribute__((always_inline))
#endif


#ifdef NDEBUG
#define BMATH_IF_DEBUG(x) ((void)0)
#else
#define BMATH_IF_DEBUG(x) x
#endif



namespace simp {
    
    
	constexpr BMATH_FORCE_INLINE void throw_if(bool cond, const char* const msg)
	{
		if (cond) [[unlikely]] {
			throw std::exception(msg);
		}
	}

	template<const auto x, const auto... xs, typename T>
	constexpr BMATH_FORCE_INLINE bool is_one_of(const T y) noexcept
	{ 
		if constexpr (!sizeof...(xs)) { return x == y; }
		else                          { return x == y || is_one_of<xs...>(y); }
	}	

	template<typename... Bool>
	constexpr BMATH_FORCE_INLINE bool equivalent(const bool x, const bool y, const Bool... xs)
	{
		if  constexpr (!sizeof...(xs)) { return x == y; }
		else                           { return x == y && equivalent(x, xs...); }
	}

	template<typename T, typename U>
	constexpr BMATH_FORCE_INLINE T change_field(const T& original, U T::* field, const U& replacement)
	{
		T result = original;
		result.*field = replacement;
		return result;
	}

	template<typename F>
	struct YCombinate 
	{
		F f;
		template<class...Args>
		decltype(auto) operator()(Args&&...args) const { return f(*this, std::forward<Args>(args)...); }
	};
	template<class F>
	YCombinate<std::decay_t<F>> y_combinate(F&& f) { return { std::forward<F>(f) }; };


	template<typename F>
	struct CallOnDelete
	{
		F f;
		constexpr CallOnDelete(F&& f_) noexcept :f(f_) {}
		constexpr ~CallOnDelete() { this->f(); }
	};
	template<typename F>
	CallOnDelete(F) -> CallOnDelete<F>;

    
	template <typename Struct_T, std::size_t Size, typename Search_T>
	[[nodiscard]] constexpr const Struct_T& find(
		const std::array<Struct_T, Size>& data, const Search_T Struct_T::* ptr, const Search_T key) noexcept
	{
		const auto itr = std::find_if(begin(data), end(data), [key, ptr](const auto &v) { return v.*ptr == key; });
		assert(itr != end(data));
		return *itr;
	}

	template <typename Struct_T, std::size_t Size, typename Search_T>
	[[nodiscard]] constexpr const Struct_T& search(
		const std::array<Struct_T, Size>& data, Search_T Struct_T::* const ptr, const Search_T key,
        const Struct_T& null_val = {}) noexcept
	{
		const auto itr = std::find_if(begin(data), end(data), [key, ptr](const auto &v) { return v.*ptr == key; });
		return itr != end(data) ? *itr : null_val;
	}

	template<typename T, std::size_t N, typename U>
	constexpr bool is_sorted_by(const std::array<T, N>& arr, U T::* const by)
	{
		return std::is_sorted(arr.begin(), arr.end(), 
			[by](const T& a, const T& b) { return a.*by < b.*by; });
	}

	template<MinimalNum T>
	constexpr T nat_pow(T base, const unsigned long long expo) noexcept
	{
		T res = 1.0;
		for (std::size_t power = 1; power <= expo; power *= 2) {
			if ((expo & power)) {
				res *= base;
			}
			base *= base;
		}
		return res;
	}

	static_assert(nat_pow(3.0, 9) == 19683);
	static_assert(nat_pow(10.0, 6) == 1000000);
	static_assert(nat_pow(10.0, 2) == 100);
	static_assert(nat_pow(2ull, 53) - 1 == 9007199254740991ull);




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
		const std::partial_ordering cmp = lhs <=> rhs;
		if (cmp == std::partial_ordering::less) {
			return std::strong_ordering::less;
		}
		else if (cmp == std::partial_ordering::greater) {
			return std::strong_ordering::greater;
		}
		else if (cmp == std::partial_ordering::equivalent) {
			return std::strong_ordering::equal;
		}
		else [[unlikely]] {
			static_assert(sizeof(double) == sizeof(std::uint64_t)); //bit_cast may cast to something of doubles size.
			return std::bit_cast<std::uint64_t>(lhs) <=> std::bit_cast<std::uint64_t>(rhs);
		}
	}

	constexpr std::strong_ordering compare_complex(const std::complex<double>& lhs, const std::complex<double>& rhs)
	{
		//const auto abs_squared = [](const std::complex<double>& c) { return c.real() * c.real() + c.imag() * c.imag(); };
		//if (const auto cmp = compare_double(abs_squared(lhs), abs_squared(rhs)); cmp != std::strong_ordering::equal) {
		//	return cmp;
		//}
		if (const auto cmp = compare_double(lhs.real(), rhs.real()); cmp != std::strong_ordering::equal) {
			return cmp;
		}
		return compare_double(lhs.imag(), rhs.imag());
	}


    
	//can be used like std::optional<double>, but with extra double operations
	struct OptionalDouble
	{
		static_assert(std::numeric_limits<double>::has_quiet_NaN);
		double val = std::numeric_limits<double>::quiet_NaN(); //default initialize to invalid state

		constexpr OptionalDouble(const double new_val) noexcept :val(new_val) {}
		constexpr OptionalDouble() noexcept = default;

		bool has_value() const noexcept { return !std::isnan(this->val); }
		explicit operator bool() const noexcept { return this->has_value(); }

		constexpr double& operator*() noexcept { return this->val; }
		constexpr const double& operator*() const noexcept { return this->val; }

		constexpr OptionalDouble operator+(const OptionalDouble snd) const noexcept { return this->val + snd.val; }
		constexpr OptionalDouble operator-(const OptionalDouble snd) const noexcept { return this->val - snd.val; }
		constexpr OptionalDouble operator*(const OptionalDouble snd) const noexcept { return this->val * snd.val; }
		constexpr OptionalDouble operator/(const OptionalDouble snd) const noexcept { return this->val / snd.val; }

		constexpr OptionalDouble operator+=(const OptionalDouble snd) noexcept { this->val += snd.val; return *this; }
		constexpr OptionalDouble operator-=(const OptionalDouble snd) noexcept { this->val -= snd.val; return *this; }
		constexpr OptionalDouble operator*=(const OptionalDouble snd) noexcept { this->val *= snd.val; return *this; }
		constexpr OptionalDouble operator/=(const OptionalDouble snd) noexcept { this->val /= snd.val; return *this; }
	}; //struct OptionalDouble

	//can be used like std::optional<std::complex<double>>, but with extra complex operations
	struct OptionalComplex
	{
		static_assert(std::numeric_limits<double>::has_quiet_NaN);
		std::complex<double> val = std::numeric_limits<double>::quiet_NaN(); //default initialize to invalid state

		constexpr OptionalComplex(const std::complex<double>& new_val) noexcept :val(new_val) {}
		constexpr OptionalComplex(const double re, const double im = 0.0) noexcept :val(re, im) {}
		constexpr OptionalComplex() noexcept = default;

		bool has_value() const noexcept { return !std::isnan(this->val.real()); }
		explicit operator bool() const noexcept { return this->has_value(); }

		constexpr std::complex<double>& operator*() noexcept { return this->val; }
		constexpr std::complex<double>* operator->() noexcept { return &this->val; }
		constexpr const std::complex<double>& operator*() const noexcept { return this->val; }
		constexpr const std::complex<double>* operator->() const noexcept { return &this->val; }

		constexpr OptionalComplex operator+(const OptionalComplex& snd) const noexcept { return this->val + snd.val; }
		constexpr OptionalComplex operator-(const OptionalComplex& snd) const noexcept { return this->val - snd.val; }
		constexpr OptionalComplex operator*(const OptionalComplex& snd) const noexcept { return this->val * snd.val; }
		constexpr OptionalComplex operator/(const OptionalComplex& snd) const noexcept { return this->val / snd.val; }		

		constexpr OptionalComplex& operator+=(const OptionalComplex& snd) noexcept { this->val += snd.val; return *this; }
		constexpr OptionalComplex& operator-=(const OptionalComplex& snd) noexcept { this->val -= snd.val; return *this; }
		constexpr OptionalComplex& operator*=(const OptionalComplex& snd) noexcept { this->val *= snd.val; return *this; }
		constexpr OptionalComplex& operator/=(const OptionalComplex& snd) noexcept { this->val /= snd.val; return *this; }

		constexpr std::strong_ordering operator<=>(const OptionalComplex& snd) noexcept { return compare_complex(this->val, snd.val); }
		constexpr bool operator==(const OptionalComplex& snd) noexcept { return compare_complex(this->val, snd.val) == std::strong_ordering::equal; }
		constexpr bool operator!=(const OptionalComplex& snd) noexcept { return compare_complex(this->val, snd.val) != std::strong_ordering::equal; }
	}; //struct OptionalComplex

	constexpr std::string repeat(const std::string_view in, int times)
	{
		std::string res;
		res.reserve(in.size() * times);
		while (times-- > 0) res += in;
		return res;
	} //repeat

	template<typename T>
	struct Range
	{
		T* iter;
		T* stop;

		//acts as range
		constexpr T* begin() const noexcept { return this->iter; }
		constexpr T* end() const noexcept { return this->stop; }
		constexpr std::size_t size() const noexcept { return this->stop - this->iter; }

		//acts as iterator
		constexpr bool at_end() const noexcept { return this->iter == this->stop; }
		constexpr Range& operator++() noexcept { ++this->iter;  return *this; }
		constexpr T& operator*() const noexcept { return *this->iter; }
	}; //struct Range
    
} //namespace simp