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


	/////////////////   Cons

	template<typename, InstanceOf<List>>
	struct Cons;

	template<typename Head, InstanceOf<List> List_>
	using Cons_t = typename Cons<Head, List_>::type;

	template<typename Head, typename... Tail>
	struct Cons<Head, List<Tail...>> { using type = List<Head, Tail...>; };


	/////////////////   Concat

	template<InstanceOf<List>, InstanceOf<List>> 
	struct Concat;

	template<InstanceOf<List> List_1, InstanceOf<List> List_2>
	using Concat_t = typename Concat<List_1, List_2>::type;

	template<typename... Elems_1, typename... Elems_2>
	struct Concat<List<Elems_1...>, List<Elems_2...>> { using type = List<Elems_1..., Elems_2...>; };

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
			Cons_t<Lhs_1, Intersection_t<List<Lhs_Tail...>, List<Rhs...>>>,
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


	/////////////////   Remove

	template<typename, InstanceOf<List>>
	struct Remove;

	template<typename T, InstanceOf<List> List_>
	using Remove_t = typename Remove<T, List_>::type;

	template<typename T>
	struct Remove<T, List<>> { using type = List<>; };

	template<typename T, typename... Elems>
	struct Remove<T, List<T, Elems...>> { using type = Remove_t<T, List<Elems...>>; };

	template<typename T, typename Elem_0, typename... Elems>
	struct Remove<T, List<Elem_0, Elems...>>
	{
		using type = Cons_t<Elem_0, Remove_t<T, List<Elems...>>>;
	};

	static_assert(std::is_same_v<Remove_t<int, List<bool, void, int, double>>, List<bool, void, double>>);


	/////////////////   Index

	template<typename, InstanceOf<List>>
	struct Index;

	template<typename Needle, InstanceOf<List> List_>
	constexpr int index_v = Index<Needle, List_>::value;

	template<typename Needle>
	struct Index<Needle, List<>> :std::integral_constant<int, -1> {};

	template<typename Needle, typename... Tail>
	struct Index<Needle, List<Needle, Tail...>> :std::integral_constant<int, 0> {};

	template<typename Needle, typename Head, typename... Tail>
	struct Index<Needle, List<Head, Tail...>> 
	{
	private:
		static constexpr int tail_index = index_v<Needle, List<Tail...>>;
	public:
		static constexpr int value = (tail_index == -1) ? -1 : tail_index + 1;
	};

	static_assert(index_v<int, List<char, bool, float, int, double, long>> == 3);
	static_assert(index_v<int, List<char, bool, float, double, long>> == -1);





	template<typename T1, typename T2>
	struct Pair 
	{
		using fst = T1;
		using snd = T2;
	};

} //namespace bmath::intern::meta