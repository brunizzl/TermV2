#pragma once

#include <cassert>
#include <cstdlib>

#include "utility/meta.hpp"
#include "utility/queue.hpp"
#include "utility/vector.hpp"

#include "types.hpp"
#include "reference.hpp"


//control structures implemented without reursion, thus not limited by the callstack size
namespace simp::ctrl {
    using bmath::intern::CallableTo;
    using bmath::intern::Callable;    
    using bmath::intern::Procedure;
    using bmath::intern::Reference;

    constexpr auto iter_from_lambda(const MutRef ref)
    {
        assert(ref.type == Literal::lambda);
        constexpr std::size_t offset = offsetof(Lambda, definition);
        static_assert(offset % sizeof(NodeIndex) == 0, 
            "Lambda::definition has to be alligned as NodeIndex to allow iterating");
        constexpr std::int32_t start_index = offset / sizeof(NodeIndex) -FApp::values_per_info;

        return decltype(begin(ref))::build(*ref.store, ref.index, start_index, start_index + 1);
    } //iter_from_lambda

    template<typename C, typename R, typename ElemInfo>
    concept Consumer = requires (C c, R ref, ElemInfo& info) {
        { c.consume(ref, info) };
        { c.make_info(ref) } -> std::convertible_to<ElemInfo>;
        { c.finalize(info) };
    };

    template<typename C, typename R, typename ElemInfo>
    concept ReturnEarly = requires (C c, R ref, ElemInfo& info) {
        { c.consume(ref, info) } -> std::convertible_to<bool>;
    };

    template<bool traverse_lambdas = false, typename StackInfo, Consumer<UnsaveRef, StackInfo> C>
    void traverse_preorder(TermNode const* const store_data, const NodeIndex head, C& c, StackInfo fst_info)
    {
        struct StackFrame
        {
            NodeIndex const* iter;
            NodeIndex const* stop;
            StackInfo info;
        };
        bmath::intern::StupidBufferVector<StackFrame, 64> stack;
        //c.make_info(...) only expects applications (and perhaps lambdas), which is not guaranteed to hold for head -> use fst_info
        stack.emplace_back(&head, &head + 1, fst_info); 
        do {
            StackFrame& top = stack.back();
            bool top_done = false; 
            do { //emmiting check at beginning relies on never encounterig an empty range as top
                const UnsaveRef ref = UnsaveRef(store_data, top.iter->get_index(), top.iter->get_type());

                if constexpr (ReturnEarly<C, UnsaveRef, StackInfo>) { if (c.consume(ref, top.info)) return; }
                else c.consume(ref, top.info);

                //functions recursive in only the last parameter (looking at you, cons) will not grow the stack
                if (++top.iter == top.stop) {
                    c.finalize(top.info);
                    stack.pop_back();
                    top_done = true;
                }
                if (ref.type == Literal::f_app || ref.type == PatternFApp{}) {
                    stack.emplace_back(ref->f_app.begin(), ref->f_app.end(), c.make_info(ref));
                    break;
                }
                if constexpr (traverse_lambdas) if (ref.type == Literal::lambda) {
                    NodeIndex const* const def_ptr = &ref->lambda.definition;
                    stack.emplace_back(def_ptr, def_ptr + 1, c.make_info(ref));
                    break;
                }
            } while (!top_done);
        } while (stack.size());
    } //traverse_preorder

    using SearchQueue = Queue<NodeIndex, 256>;

    //returns first subterm where pred is true, tested in breadth-first search
    //enqueue adds subterms of curr_ref to queue
    template<std::predicate<UnsaveRef> Pred, Procedure<UnsaveRef, SearchQueue&> Enqueue>
    NodeIndex search(const TermNode* const store_data, const NodeIndex head, Pred pred, Enqueue enqueue)
    {
        SearchQueue queue;
        queue.emplace_back(head);
        do {
            const NodeIndex curr_idx = queue.pop_front();
            const UnsaveRef curr_ref = UnsaveRef(store_data, curr_idx.get_index(), curr_idx.get_type());
            if (pred(curr_ref)) {
                return curr_idx;
            }
            enqueue(curr_ref, queue);
        } while (queue.size());
        return invalid_index;
    } //search

    template<bool search_lambdas = false, NodeType f_app = Literal::f_app, std::predicate<UnsaveRef> Pred>
    NodeIndex search(const TermNode* const store_data, const NodeIndex head, Pred pred)
    {
        const auto enqueue = [](const UnsaveRef ref, SearchQueue& queue) {
            static_assert(f_app == Literal::f_app || f_app == PatternFApp{});
            assert(ref.type != Literal::f_app || f_app == Literal::f_app);
            assert(ref.type != PatternFApp{} || f_app == PatternFApp{});
            if (ref.type == f_app) {
                for (const NodeIndex& sub : ref) {
                    queue.emplace_back(sub);
                }
            }
            if constexpr (search_lambdas) if (ref.type == Literal::lambda) {
                queue.emplace_back(ref->lambda.definition);
            }
        };
        return search(store_data, head, pred, enqueue);
    } //search
      
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
