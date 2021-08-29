#pragma once

#include <cassert>
#include <cstdlib>
#include <iostream>

#include "utility/misc.hpp"
#include "utility/meta.hpp"
#include "utility/queue.hpp"
#include "utility/vector.hpp"

#include "types.hpp"
#include "reference.hpp"


//control structures implemented without reursion, thus not limited by the callstack size
namespace simp::ctrl {

    using SaveRange = decltype(begin(std::declval<MutRef>()));
    using UnsaveRange = Range<NodeIndex const>;

    template<Reference R> struct ChooseRange;
    template<typename R> using ChooseRange_t = typename ChooseRange<R>::type;

    template<> struct ChooseRange<UnsaveRef> { using type = UnsaveRange; };
    template<> struct ChooseRange<MutRef>    { using type = SaveRange; };

    constexpr inline SaveRange iter_from_lambda(const MutRef ref)
    {
        assert(ref.type == Literal::lambda);
        constexpr std::size_t offset = offsetof(Lambda, definition);
        static_assert(offset % sizeof(NodeIndex) == 0,
            "Lambda::definition has to be alligned as NodeIndex to allow iterating");
        constexpr std::int32_t start_index = offset / sizeof(NodeIndex) - FApp::values_per_info;

        return SaveRange::build(*ref.store, ref.index, start_index, start_index + 1);
    } //iter_from_lambda

    constexpr inline UnsaveRange iter_from_lambda(const UnsaveRef ref)
    {
        assert(ref.type == Literal::lambda);
        NodeIndex const* const def = &ref->lambda.definition;
        return UnsaveRange{ def, def + 1 };
    } //iter_from_lambda

    constexpr inline SaveRange iter_from_application(const MutRef ref) { return begin(ref); }
    constexpr inline UnsaveRange iter_from_application(const UnsaveRef ref) { return { begin(ref), end(ref) }; }

    constexpr inline SaveRange empty_range(const MutRef ref) { return SaveRange::build(*ref.store, 0, 0, 0); }
    constexpr inline UnsaveRange empty_range(const UnsaveRef ref) { return { nullptr, nullptr }; }

    template<bool search_lambdas = false, NodeType f_app = Literal::f_app, typename Container, Reference R>
    constexpr inline bool add_range(Container& container, const R ref)
    {
        static_assert(f_app == Literal::f_app || f_app == PatternFApp{});
        assert(ref.type != Literal::f_app || f_app == Literal::f_app);
        assert(ref.type != PatternFApp{} || f_app == PatternFApp{});

        if (ref.type == f_app) {
            container.emplace_back(iter_from_application(ref));
            return true;
        }
        if constexpr (search_lambdas) if (ref.type == Literal::lambda) {
            container.emplace_back(iter_from_lambda(ref));
            return true;
        }
        return false;
    } //add_range




    using SearchQueue = Queue<UnsaveRange, 64>;

    //returns first subterm where pred is true, subterms are tested in unspecified order
    //enqueue adds range over subterms of curr_ref to queue
    template<std::predicate<UnsaveRef> Pred, Callable<SearchQueue&, UnsaveRef> Enqueue>
    NodeIndex search(const TermNode* const store_data, const NodeIndex head, Pred pred, Enqueue enqueue)
    {
        SearchQueue queue;
        queue.emplace_back(&head, &head + 1);
        do {
            UnsaveRange range = queue.pop_front();
            if (range.size() > 20) [[unlikely]] { 
                //if a function application has many parameters, these are looked at in smaller chunks.
                //this prevents building large queues in situations as "set(tup(0, 0), tup(0, 1), tup(0, 2), ..., tup(10, 9), tup(10, 10))"
                //it is not sufficient to split when a new range is added, as then all partial ranges would 
                //  still be in direct succession of each other, not soving the problem
                NodeIndex const* const split_point = range.iter + 16;
                queue.emplace_back(split_point, range.stop);
                range.stop = split_point;
            }
            do {
                assert(range.size() > 0); //holds, because every f_app at least holds the applied function and every lambda has a definition
                const NodeIndex curr_idx = *range.iter;
                const UnsaveRef curr_ref = UnsaveRef(store_data, curr_idx.get_index(), curr_idx.get_type());
                if (pred(curr_ref)) {
                    return curr_idx;
                }
                enqueue(queue, curr_ref);
            } while (++range.iter != range.stop);
        } while (queue.size());
        return invalid_index;
    } //search

    template<bool search_lambdas = false, NodeType f_app = Literal::f_app, std::predicate<UnsaveRef> Pred>
    inline NodeIndex search(const TermNode* const store_data, const NodeIndex head, Pred pred)
    {
        return search(store_data, head, pred, add_range<search_lambdas, f_app, SearchQueue, UnsaveRef>);
    } //search

    ////applies f to every subterm in postorder 
    //template<bool search_lambdas = true, NodeType f_app = Literal::f_app, Reference R, Procedure<R> F>
    //void transform(const R head, F f)
    //{
    //    struct StackFrame
    //    {
    //        ChooseRange_t<R> iter;
    //        R parent;
    //    };
    //
    //    StupidBufferVector<StackFrame, 16> stack;
    //
    //    const auto add_frame = [&stack](const R ref) {
    //        static_assert(f_app == Literal::f_app || f_app == PatternFApp{});
    //        assert(ref.type != Literal::f_app || f_app == Literal::f_app);
    //        assert(ref.type != PatternFApp{} || f_app == PatternFApp{});
    //
    //        const auto iter = [&] {
    //            switch (ref.type) {
    //            case f_app: return iter_from_application(ref);
    //            case NodeType(Literal::lambda): if constexpr (search_lambdas) return iter_from_lambda(ref);
    //            }
    //            return empty_range(ref);
    //        }();
    //        stack.emplace_back(iter, ref);
    //    };
    //
    //    add_frame(head);
    //    do {
    //    transform_current:
    //        auto& current = stack.back();
    //        if (!current.iter.at_end()) {
    //            add_frame(head.at(*current.iter));
    //            ++current.iter;
    //            goto transform_current;
    //        }
    //        f(current.parent);
    //        stack.pop_back();
    //    } while (stack.size());
    //} //transform
    //
    ////applies f to every subterm in preorder, result is new subterm
    //template<bool search_lambdas = true, NodeType f_app = Literal::f_app, Reference R, CallableTo<NodeIndex, R> F>
    //[[nodiscard]] NodeIndex transform(const R ref, F f)
    //{
    //    StupidBufferVector<ChooseRange_t<R>, 16> stack;
    //    const NodeIndex result = f(ref);
    //    if (add_range<search_lambdas, f_app>(stack, ref.at(result))) {
    //    transform_current:
    //        do {
    //            auto& current = stack.back();
    //            bool current_done = false;
    //            do {
    //                const NodeIndex sub_result = f(ref.at(*current));
    //                *current = sub_result;
    //                //functions recursive in only the last parameter 
    //                //  (looking at you, cons) will not grow the stack
    //                if ((++current).at_end()) {
    //                    stack.pop_back(); 
    //                    current_done = true;
    //                }
    //                if (add_range<search_lambdas, f_app>(stack, ref.at(sub_result))) {
    //                    goto transform_current;
    //                }
    //            } while (!current_done);
    //        } while (stack.size());
    //    }
    //    return result;
    //} //transform

    //applies f to every subterm in postorder, result is new subterm
    template<Reference R, CallableTo<NodeIndex, R> F>
    [[nodiscard]] NodeIndex transform(const R ref, F f)
    {
        if (ref.type == Literal::f_app || ref.type == PatternFApp{}) {
            const auto stop = end(ref);
            for (auto iter = begin(ref); iter != stop; ++iter) {
                *iter = transform(ref.at(*iter), f);
            }
        }
        if (ref.type == Literal::lambda) {
            ref->lambda.definition = transform(ref.at(ref->lambda.definition), f);
        }
        return f(ref);
    } //transform

    //applies f to every subterm in postorder
    template<Reference R, Procedure<R> F>
    void transform(const R ref, F f)
    {
        if (ref.type == Literal::f_app || ref.type == PatternFApp{}) {
            for (const auto sub : ref) {
                transform(ref.at(sub), f);
            }
        }
        if (ref.type == Literal::lambda) {
            transform(ref.at(ref->lambda.definition), f);
        }
        f(ref);
    } //transform

} //namespace simp::ctrl
