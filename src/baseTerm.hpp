#pragma once

#include <type_traits>

namespace bmath::intern {

	enum class Base_Types
	{
		regular,
		pattern,
	};

	template <Base_Types base_type>
	struct Base_Term
	{
		enum class Specific_Types
		{
			sum,
			product,
		};

		virtual ~Base_Term() {}

		virtual void print() const = 0;
		virtual constexpr Specific_Types get_type() const = 0;
	};

	using Regular_Term = Base_Term<Base_Types::regular>;
	using Pattern_Term = Base_Term<Base_Types::pattern>;
	static_assert(!std::is_same<Regular_Term, Pattern_Term>::value);

	using Regular_Types = Base_Term<Base_Types::regular>::Specific_Types;
	using Pattern_Types = Base_Term<Base_Types::pattern>::Specific_Types;

} //namespace bmath::intern
