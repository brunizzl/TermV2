#pragma once

#include <cassert>

#include "utility/meta.hpp"
#include "utility/queue.hpp"
#include "utility/vector.hpp"

#include "types.hpp"

//control structures implemented without reursion, thus not limited by the callstack size
namespace simp::ctrl {
    using bmath::intern::CallableTo;
    using bmath::intern::Callable;    

    template<typename C, typename R, typename ElemInfo>
    concept Consumer = requires (C c, R ref, ElemInfo& info) {
        { c.make_info(ref) };
        { c.consume(ref, info) };
        { c.finalize(info) };
    };

    template<typename ElemInfo, Consumer<UnsaveRef, ElemInfo> C>
    void traverse_app_preorder(const UnsaveRef ref, C& consumer, ElemInfo fst_info)
    {
        consumer.consume(ref, fst_info);

        if (ref.type == Literal::f_app) {
            struct StackElem
            {
                NodeIndex const* iter;
                NodeIndex const* const stop;
                ElemInfo info; 
            };
            bmath::intern::StupidBufferVector<StackElem, 128> stack;
            stack.emplace_back(ref->f_app.begin(), ref->f_app.end(), consumer.make_info(ref));

        iterate_over_top:
            do {
                StackElem& top = stack.back();
                while (top.iter != top.stop) {
                    const UnsaveRef current = ref.at(*(top.iter++));
                    consumer.consume(current, top.info);

                    if (current.type == Literal::f_app) {
                        stack.emplace_back(current->f_app.begin(), current->f_app.end(), consumer.make_info(current));
                        goto iterate_over_top;
                    }
                }
                consumer.finalize(top.info);
                stack.pop_back();
            } while (stack.size());
        }
    } //traverse_app_preorder

    //returns first subterm where pred is true, tested in pre-order
    //queue_subs adds subterms of curr_ref to queue
    template<std::predicate<UnsaveRef> Pred, Callable<UnsaveRef, Queue<NodeIndex, 128>&> QueueSubs>
    NodeIndex search(const TermNode* const store_data, const NodeIndex head, Pred pred, QueueSubs queue_subs)
    {
        Queue<NodeIndex, 128> queue;
        queue.emplace_back(head);
        do {
            const NodeIndex curr_idx = queue.pop_front();
            const UnsaveRef curr_ref = UnsaveRef(store_data, curr_idx.get_index(), curr_idx.get_type());
            if (pred(curr_ref)) {
                return curr_idx;
            }
            queue_subs(curr_ref, queue);
        } while (queue.size());
        return invalid_index;
    } //search

    //returns first subterm where pred is true, tested in pre-order
    template<bool queue_lambdas = false, NodeType f_app = Literal::f_app, std::predicate<UnsaveRef> Pred>
    NodeIndex search(const TermNode* const store_data, const NodeIndex head, Pred pred)
    {
        const auto queue_subs = [](const UnsaveRef ref, Queue<NodeIndex, 128>& queue) {
            static_assert(f_app == Literal::f_app || f_app == PatternFApp{});
            assert(ref.type != Literal::f_app || f_app == Literal::f_app);
            assert(ref.type != PatternFApp{} || f_app == PatternFApp{});
            if (ref.type == f_app) {
                for (const NodeIndex& sub : ref) {
                    queue.emplace_back(sub);
                }
            }
            if constexpr (queue_lambdas) {
                if (ref.type == Literal::lambda) {
                    queue.emplace_back(ref->lambda.definition);
                }
            }
        };
        return search(store_data, head, pred, queue_subs);
    } //search

} //namespace simp::ctrl
