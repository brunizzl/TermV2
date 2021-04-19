#pragma once

#include "termStore.hpp"

namespace bmath::intern {

	//it is advised to only use this concept to specify other concepts,
	//  if both const and mut ref are semantically allowed, only enable const
	template<typename R> 
	concept Reference = 
		requires(R a, BasicTypedIdx<typename R::marker_type> typed_idx) { 
		{ a.operator*() } -> std::convertible_to<const typename R::value_type>;
		{ a.operator->() } -> std::convertible_to<const typename R::value_type*>;
		{ a.new_at(typed_idx) } -> std::same_as<R>;
	};







	template<typename Own_T, StoreLike Store_T>
	struct BasicNodeRef;


	//as not a direct pointer to the data of the store is contained here, but a pointer to the store, 
	//  it is allowed for the store to resize without letting the reference dangle.
	template<typename Type_T, StoreLike Store_T>
	struct BasicSaveRef
	{
		using value_type = typename Store_T::value_type;
		using marker_type = Type_T;
		static constexpr bool is_const = std::is_const_v<Store_T>;

		Store_T* const store; //actual pointer to have shallow constness
		std::uint32_t index;
		Type_T type;

		constexpr BasicTypedIdx<Type_T> typed_idx() const noexcept { return BasicTypedIdx<Type_T>(this->index, this->type); }

		constexpr BasicSaveRef(Store_T& new_store, const BasicTypedIdx<Type_T> elem) noexcept
			:store(&new_store), index(elem.get_index()), type(elem.get_type()) {}

		constexpr BasicSaveRef(Store_T& new_store, const std::uint32_t new_index, const Type_T new_type) noexcept
			:store(&new_store), index(new_index), type(new_type) {}

		constexpr BasicSaveRef(const BasicSaveRef<Type_T, std::remove_const_t<Store_T>>& ref) noexcept 
			:store(ref.store), index(ref.index), type(ref.type) {} //allow both const and mut to be initialized from mut

		constexpr auto& operator*() const { return store->at(index); }
		constexpr auto* operator->() const { return &store->at(index); }

		constexpr BasicSaveRef new_at(const BasicTypedIdx<Type_T> elem) const noexcept 
		{ 
			return BasicSaveRef(*this->store, elem); 
		}

		template<typename Own_T>
		constexpr auto cast() const noexcept 
		{
			return BasicNodeRef<Own_T, Store_T>(*this->store, this->index); 
		}
	}; //struct BasicSaveRef


	//as most algorithms accessing an element of a term need also access to its other elements, both the pointer to the element
	//  and NodeIndex info are neatly bundled as a package here
	template<typename Type_T, typename Union_T>
	struct BasicUnsaveRef
	{
		using marker_type = Type_T;
		using value_type = Union_T;
		static constexpr bool is_const = true;

		const Union_T* ptr; //pointer to element 
		std::uint32_t index; //index of element in store
		Type_T type;

		constexpr auto typed_idx() const noexcept { return BasicTypedIdx<Type_T>(this->index, this->type); }

		constexpr BasicUnsaveRef(const Union_T* const new_ptr, const std::uint32_t new_index, const Type_T new_type) noexcept
			:ptr(new_ptr), index(new_index), type(new_type) {}

		template<StoreLike Store_T> requires (std::is_same_v<Union_T, typename Store_T::value_type>)
		constexpr BasicUnsaveRef(const BasicSaveRef<Type_T, Store_T> init) noexcept
			:ptr(init.store->data() + init.index), index(init.index), type(init.type) {}


		constexpr const Union_T& operator*() const { return *this->ptr; }
		constexpr const Union_T* operator->() const { return this->ptr; }

		constexpr BasicUnsaveRef new_at(const BasicTypedIdx<Type_T> elem) const noexcept
		{
			const std::uint32_t new_index = elem.get_index();
			const Union_T* const new_ptr = this->ptr - this->index + new_index;
			return BasicUnsaveRef(new_ptr, new_index, elem.get_type());
		}

		constexpr const Union_T* raw_at(std::uint32_t idx) const noexcept { return this->ptr - this->index + idx; }

		constexpr const Union_T* store_data() const noexcept { return this->ptr - this->index; }
	}; //struct BasicSaveRef




	//in contrast to BasicSaveRef, this struct only stands for the single type Own_T in that Union_T thingy
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

		using Allowed_T = std::conditional_t<is_const, const Own_T, Own_T>;
		constexpr auto& operator*() const { return static_cast<Allowed_T&>(store->at(index)); }
		constexpr auto* operator->() const { return &static_cast<Allowed_T&>(store->at(index)); }
	};

} //namespace bmath::intern