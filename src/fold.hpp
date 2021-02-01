#pragma once

#include "arithmeticTerm.hpp"

namespace bmath::intern::fold {    

		template<typename Wrapped_T>
		struct Find //the simple_fold evaluation stops early if cut is true and returns value (and cut)
		{
			Wrapped_T value;
			bool cut = false;

			constexpr bool return_early() const noexcept { return this->cut; }
			constexpr Wrapped_T& operator*() noexcept { return this->value; }
			constexpr const Wrapped_T& operator*() const noexcept { return this->value; }
		};
		template<typename Wrapped_T> constexpr Find<Wrapped_T> done(const Wrapped_T w) { return { w, true  }; }
		template<typename Wrapped_T> constexpr Find<Wrapped_T> more(const Wrapped_T w) { return { w, false }; }
		
		struct FindTrue //the simple_fold evaluation stops early if cut is true and returns true
		{
			bool cut = false;

			constexpr bool return_early() const noexcept { return this->cut; }
			constexpr operator bool() const noexcept { return this->cut; }
			constexpr FindTrue(bool init) :cut(init) {}
			constexpr FindTrue() = default;
		};


		template<typename T, typename = void> 
		struct ReturnEarlyPossible :std::false_type {};

		template<typename T> 
		struct ReturnEarlyPossible <T, std::void_t<decltype(std::declval<T>().return_early())>> :std::true_type {};

		static_assert(ReturnEarlyPossible<Find<int>>::value);
		static_assert(ReturnEarlyPossible<FindTrue>::value);
		static_assert(!ReturnEarlyPossible<bool>::value);


		struct Void {}; //used if there is nothing to be returned from simple_fold

		//calls apply with every node (postorder),
		//Res_T might have nonstatic member return_early, to indicate if the fold may be stopped early, because the result is already known
		//not really a fold function in the classical sense, as there is no information accumulated - 
		//  eighter you have the final result or not. (or you only mutate the term and not return any result at all)
		template<typename Res_T, Reference R, CallableTo<Res_T, R> Apply>
		Res_T simple_fold(const R ref, Apply apply)
		{
			if (ref.type.is<Function>()) {
				for (const auto elem : fn::range(ref)) {
					const Res_T elem_res = fold::simple_fold<Res_T>(ref.new_at(elem), apply);
					if constexpr (ReturnEarlyPossible<Res_T>::value) { if (elem_res.return_early()) { return elem_res; } }
				}
			}
			return apply(ref); 
		} //simple_fold


		template<typename A, typename Res_T>
		concept Accumulator = requires(A a, Res_T r) {
			{ a.consume(r) };
			{ a.result() } -> std::convertible_to<Res_T>;
		};

		//this fold differentiates between recursive nodes (Function and ValueMatchVariable) and Leafes (values and variables)
		//Acc is constructed before a recursive call is made and consumes each recursive result. It thus needs to at least
		//  have a Constructor taking as arguments (BasicSaveRef<Union_T, Type_T, Const> ref, AccInit... init) 
		//  and a consume method taking as single parameter of type Res_T
		//  a result method taking no parameters and returning Res_T	
		template<typename Res_T, Accumulator<Res_T> Acc, Reference R, 
            CallableTo<Res_T, R> LeafApply, typename... AccInit>
		Res_T tree_fold(const R ref, LeafApply leaf_apply, const AccInit... init)
		{
			if (ref.type.is<Function>()) {
				Acc acc(ref, init...);
				for (const auto elem : fn::range(ref)) {
					acc.consume(fold::tree_fold<Res_T, Acc>(ref.new_at(elem), leaf_apply, init...));
				}
				return acc.result();
			}
			else {
				return leaf_apply(ref);
			}
		} //tree_fold


		//calls apply with every node (postorder), and assigns the result to the position the node is stored at.
		//a change of storage location while the function executes is tolerated.
		template<typename TypedIdx, Reference R, CallableTo<TypedIdx, R> Apply> requires (!R::is_const)
		[[nodiscard]] TypedIdx mutate_fold(const R ref, Apply apply)
		{
			if (ref.type.is<Function>()) {
				const auto range = fn::range(ref);
				const auto stop = end(range);
				auto iter = begin(range);
				for (; iter != stop; ++iter) {
					*iter = fold::mutate_fold<TypedIdx>(ref.new_at(*iter), apply);
				}
			}
			return apply(ref);
		} //mutate_fold

} //namespace bmath::intern::fold