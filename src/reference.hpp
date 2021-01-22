#pragma once

#include "termStore.hpp"

namespace bmath::intern {

	template<typename T> //it is advised to only use this concept to specify other concepts,
	concept AnyReference = requires(T a) { //if both const and mut ref are semantically allowed, only enable const
		{ a.store };
		{ a.type };
		{ a.index };
		{ a.typed_idx() };
		{ a.operator*() };
		{ a.operator->() };
	};

	template<typename Own_T, StoreLike Store_T>
	struct BasicNodeRef;


	//as most algorithms accessing an element of a term need also access to its store, both store and TypedIdx info
	//  are neatly bundled as a package here
	template<typename Type_T, StoreLike Store_T>
	struct BasicRef
	{
		using value_type = typename Store_T::value_type;
		static constexpr bool is_const = std::is_const_v<Store_T>;

		Store_T* const store; //actual pointer to have shallow constness
		std::uint32_t const index;
		Type_T const type;

		constexpr BasicTypedIdx<Type_T> typed_idx() const noexcept { return BasicTypedIdx<Type_T>(this->index, this->type); }

		constexpr BasicRef(Store_T& new_store, const BasicTypedIdx<Type_T> elem) noexcept
			:store(&new_store), index(elem.get_index()), type(elem.get_type()) {}

		constexpr BasicRef(Store_T& new_store, const std::uint32_t new_index, const Type_T new_type) noexcept
			:store(&new_store), index(new_index), type(new_type) {}

		constexpr BasicRef(const BasicRef<Type_T, std::remove_const_t<Store_T>>& ref) noexcept 
			:store(ref.store), index(ref.index), type(ref.type) {} //allow both const and mut to be initialized from mut

		constexpr auto& operator*() const { return store->at(index); }
		constexpr auto* operator->() const { return &store->at(index); }

		constexpr BasicRef new_at(const BasicTypedIdx<Type_T> elem) const noexcept 
		{ 
			return BasicRef(*this->store, elem); 
		}

		template<typename Own_T>
		constexpr auto new_as(const std::uint32_t new_index) const noexcept 
		{ 
			return BasicNodeRef<Own_T, Store_T>(*this->store, new_index); 
		}

		template<typename Own_T>
		constexpr auto cast() const noexcept 
		{
			return BasicNodeRef<Own_T, Store_T>(*this->store, this->index); 
		}
	}; //struct BasicRef




	//in contrast to BasicRef, this struct only stands for the single type Own_T in that Union_T thingy
	template<typename Own_T, StoreLike Store_T>
	struct BasicNodeRef
	{
		static_assert(!std::is_const_v<Own_T>, "a ref is const if its store is const");
		static_assert(std::is_convertible_v<typename Store_T::value_type, Own_T>);
		static constexpr bool is_const = std::is_const_v<Store_T>;
		using value_type = Own_T;

		Store_T* const store; //actual pointer to have shallow constness
		std::uint32_t const index;

		constexpr BasicNodeRef(Store_T& new_store, const std::uint32_t new_index) noexcept
			:store(&new_store), index(new_index) {}

		constexpr auto new_at(const std::size_t new_index) const noexcept 
		{ 
			return BasicNodeRef(*this->store, new_index); 
		}

		using Allowed_T = std::conditional_t<is_const, const Own_T, Own_T>;
		constexpr auto& operator*() const { return static_cast<Allowed_T&>(store->at(index)); }
		constexpr auto* operator->() const { return &static_cast<Allowed_T&>(store->at(index)); }
	};

} //namespace bmath::intern