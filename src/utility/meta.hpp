#pragma once

#include <type_traits>
#include <concepts>

namespace bmath::intern::meta {


	template<typename From, typename To>
	concept ExplicitlyConvertibleTo =
		requires (From from) { static_cast<To>(from); };


	/////////////////   InstanceOf

	template<typename T, template<typename...> class Template>
	struct DecideInstanceOf :std::false_type {};

	template<template<typename...> class Template, typename... Args>
	struct DecideInstanceOf<Template<Args...>, Template> :std::true_type {};

	template<typename T, template<typename...> class Template>
	concept InstanceOf = DecideInstanceOf<T, Template>::value;


	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////   Operations on Lists   ////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	template<typename... Elements>
	struct List;


	template<InstanceOf<List> List_>
	constexpr bool is_empty_list_v = std::is_same_v<List_, List<>>;

	static_assert(is_empty_list_v<List<>> && !is_empty_list_v<List<int, double>>);


	/////////////////   Concat

	template<InstanceOf<List>, InstanceOf<List>> 
	struct Concat;

	template<typename... Elems_1, typename... Elems_2>
	struct Concat<List<Elems_1...>, List<Elems_2...>> { using type = List<Elems_1..., Elems_2...>; };

	template<InstanceOf<List> List_1, InstanceOf<List> List_2>
	using Concat_t = typename Concat<List_1, List_2>::type;

	static_assert(std::is_same_v<Concat_t<List<int, int>, List<double, nullptr_t>>, List<int, int, double, nullptr_t>>);


	/////////////////   InList

	template<typename, InstanceOf<List>> struct InList;

	template<typename Needle, InstanceOf<List> List_>
	constexpr bool in_list_v = InList<Needle, List_>::value;

	template<typename Needle, typename Elem_0, typename... Elems>
	struct InList<Needle, List<Elem_0, Elems...>> :std::bool_constant<
		std::is_same_v<Needle, Elem_0> || in_list_v<Needle, List<Elems...>>
	> {};

	template<typename Needle> 
	struct InList<Needle, List<>> :std::false_type {};

	static_assert(!in_list_v<char, List<double, int, int, void>>);
	static_assert(in_list_v<char, List<double, char, int, void>>);


	/////////////////   Intersection

	template<InstanceOf<List>, InstanceOf<List>>
	struct Intersection;

	template<InstanceOf<List> List_1, InstanceOf<List> List_2>
	using Intersection_t = typename Intersection<List_1, List_2>::type;

	template<typename Lhs_1, typename... Lhs_Tail, typename... Rhs>
	struct Intersection<List<Lhs_1, Lhs_Tail...>, List<Rhs...>>
	{
		using type = std::conditional_t<in_list_v<Lhs_1, List<Rhs...>>,
			Concat_t<
				List<Lhs_1>, 
				Intersection_t<List<Lhs_Tail...>, List<Rhs...>>
			>,
			Intersection_t<List<Lhs_Tail...>, List<Rhs...>>
		>;
	};

	template<typename... Rhs> 
	struct Intersection<List<>, List<Rhs...>> { using type = List<>; };

	static_assert(std::is_same_v<Intersection<List<bool, int, char>, List<float, double, bool>>::type, List<bool>>);


	/////////////////   Disjoint

	template<InstanceOf<List>, InstanceOf<List>> 
	struct Disjoint;

	template<InstanceOf<List> List_1, InstanceOf<List> List_2>
	constexpr bool disjoint_v = Disjoint<List_1, List_2>::value;

	template<typename... Lhs, typename... Rhs>
	struct Disjoint<List<Lhs...>, List<Rhs...>> :std::bool_constant<
		is_empty_list_v<Intersection_t<List<Lhs...>, List<Rhs...>>>
	> {};

	static_assert(disjoint_v<List<bool, int, char>, List<float, double>>);
	static_assert(!disjoint_v<List<bool, int, char>, List<float, double, int>>);


} //namespace bmath::intern::meta