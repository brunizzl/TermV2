#pragma once

#include "termStore.hpp"

namespace simp {

	//it is advised to only use this concept to specify other concepts,
	//  if both const and mut ref are semantically allowed, only enable const
	template<typename R> 
	concept Reference = 
		requires(R a, BasicTypedIdx<typename R::marker_type> typed_idx) { 
		{ a.operator*() } -> std::convertible_to<const typename R::value_type>;
		{ a.operator->() } -> std::convertible_to<const typename R::value_type*>;
		{ a.at(typed_idx) } -> std::same_as<R>;
	};






	//as not a direct pointer to the data of the store is contained here, but a pointer to the store, 
	//  it is allowed for the store to resize without letting the reference dangle.
	template<typename Type_T, StoreLike Store_T>
	struct BasicSaveRef
	{
		using marker_type = Type_T;
		using value_type = typename Store_T::value_type;

		Store_T* store; //actual pointer to have shallow constness
		std::uint32_t index;
		Type_T type;

		constexpr BasicTypedIdx<Type_T> typed_idx() const noexcept { return BasicTypedIdx<Type_T>(this->index, this->type); }

		constexpr BasicSaveRef(Store_T& new_store, const BasicTypedIdx<Type_T> elem) noexcept
			:store(&new_store), index(elem.get_index()), type(elem.get_type()) {}

		constexpr BasicSaveRef(Store_T& new_store, const std::uint32_t new_index, const Type_T new_type) noexcept
			:store(&new_store), index(new_index), type(new_type) {}

		//allow both const and mut to be initialized from mut
		constexpr operator BasicSaveRef<Type_T, const Store_T>() const noexcept requires (!std::is_const_v<Store_T>)
		{	return BasicSaveRef<Type_T, const Store_T>(*this->store, this->index, this->type);
		}

		constexpr auto& operator*() const { return this->store->at(index); }
		constexpr auto* operator->() const { return &this->store->at(index); }

		constexpr BasicSaveRef at(const BasicTypedIdx<Type_T> elem) const noexcept 
		{	return BasicSaveRef(*this->store, elem); 
		}

		constexpr void point_at_new_location(const BasicTypedIdx<Type_T> new_) noexcept
		{
			this->index = new_.get_index();
			this->type  = new_.get_type();
		}
	}; //struct BasicSaveRef


	//as most algorithms accessing an element of a term need also access to its other elements, both the pointer to the element
	//  and NodeIndex info are neatly bundled as a package here
	template<typename Type_T, typename Union_T>
	struct BasicUnsaveRef
	{
		using marker_type = Type_T;
		using value_type = Union_T;

		Union_T* ptr; //pointer to element 
		std::uint32_t index; //index of element in store
		Type_T type;

		constexpr auto typed_idx() const noexcept { return BasicTypedIdx<Type_T>(this->index, this->type); }

		constexpr BasicUnsaveRef(Union_T* const store_data, const std::uint32_t new_index, const Type_T new_type) noexcept
			:ptr(store_data + new_index), index(new_index), type(new_type) {}

		template<StoreLike Store_T> requires (std::is_same_v<std::remove_const_t<Union_T>, typename Store_T::value_type>)
		constexpr BasicUnsaveRef(const BasicSaveRef<Type_T, Store_T> init) noexcept
			:ptr(init.store->data() + init.index), index(init.index), type(init.type) {}

		constexpr Union_T& operator*() const { return *this->ptr; }
		constexpr Union_T* operator->() const { return this->ptr; }

		constexpr Union_T* store_data() const noexcept { return this->ptr - this->index; }

		constexpr BasicUnsaveRef at(const BasicTypedIdx<Type_T> elem) const noexcept
		{	return BasicUnsaveRef(this->store_data(), elem.get_index(), elem.get_type());
		}
	}; //struct BasicUnsaveRef


} //namespace simp