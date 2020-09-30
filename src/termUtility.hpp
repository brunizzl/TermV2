#pragma once

#include <exception>
#include <algorithm>
#include <cassert>
#include <array>
#include <vector>

namespace bmath::in {

	constexpr void throw_if(bool cond, const char* const msg)
	{
		if (cond) [[unlikely]] {
			throw std::exception(msg);
		}
	}

	template<typename Exception_T = std::exception, typename... Args>
	constexpr void throw_if(bool cond, Args&&... args)
	{
		if (cond) [[unlikely]] {
			throw Exception_T{ std::forward<Args>(args)... };
		}
	}

	//stolen from Jason Turner: https://www.youtube.com/watch?v=INn3xa4pMfg
	//(but also modified slightly)
	template <typename Key, typename Value, std::size_t Size>
	[[nodiscard]] constexpr Value find(
		const std::array<std::pair<Key, Value>, Size>& data, const Key key) noexcept
	{
		const auto itr = std::find_if(begin(data), end(data),
			[&key](const auto &v) { return v.first == key; });
		if (itr == end(data)) {
			assert(false);
		}
		return itr->second;
	}

	template <typename Key, typename Value, std::size_t Size>
	[[nodiscard]] constexpr Value search(
		const std::array<std::pair<Key, Value>, Size>& data, 
		const Key key, const Value null_value) noexcept 
	{
		const auto itr = std::find_if(begin(data), end(data),
			[&key](const auto &v) { return v.first == key; });
		if (itr != end(data)) {
			return itr->second;
		} else {
			return null_value;
		}
	}

	template<typename Val>
	std::ostream& operator<<(std::ostream& str, const std::vector<Val>& vec)
	{
		str << "{ ";
		bool first = true;
		for (const auto& elem : vec) {
			if (!std::exchange(first, false)) {
				str << ", ";
			}
			str << elem;
		}
		str << '}';
	}

} //namespace bmath::in