#pragma once

#include "types.hpp"
#include "io.hpp"

#include <compare>
#include <concepts>
#include <string>

namespace simp {

    bool in_complex_subset(const Complex& nr, const nv::ComplexSubset domain);

    //only interested in final results -> a function call guaranteed to return something meeting restr results in false
    bool meets_restriction(const UnsaveRef ref, const nv::Native restr);

    namespace normalize {
        struct Options 
        {
            bool exact = true; //(only significant if eval_values) true: only exact operations are permitted
            bool eval_lambdas = true; //seems to be self explanatory
            bool normalize_lambdas = true; //true: all nested lambdas become transparent & unessecary indirections are removed
            bool remove_unary_assoc = true; //true: "f(a) -> a" for all associative f (e.g. sum, product, and...)
        };

        //will always evaluate exact operations and merge nested calls of an associative operation
        //  (meaning "f(as..., f(bs...), cs...) -> f(as..., bs..., cs...)" for associative f)
        //lambda_param_offset counts how many parameters all parent lambdas hold together 
        //  example: if subterm "a" would be combined in term "\x.\y z. (a + 7)", lambda_param_offset == 3
        //returns combined ref, might invalidate old one
        //may throw if: a lambda is called with to many parameters or something not callable is called
        [[nodiscard]] NodeIndex outermost(MutRef ref, const Options options, const unsigned lambda_param_offset);

        //applies outermost first to children then to itself
        //as an exception, calls to fn if nv::is_lazy(fn) are lazily evaluated 
        [[nodiscard]] NodeIndex recursive(MutRef ref, const Options options, const unsigned lambda_param_offset);
    } //namespace normalize


    //frees subtree starting at ref
    void free_tree(const MutRef ref);

    //copies tree starting at src_ref into dst_store
    template<bmath::intern::Reference R, bmath::intern::StoreLike S>
    [[nodiscard]] NodeIndex copy_tree(const R src_ref, S& dst_store);

    //orders node types by their "specificity", meaning that types potentially matching more things are generally sorted to the back
    constexpr int shallow_order(const UnsaveRef ref) {
        switch (ref.type) {
        case NodeType(Literal::complex):           return 0;
        case NodeType(SpecialMatch::value):        return 2;
        case NodeType(PatternUnsigned{}):          return 4;
        case NodeType(Literal::lambda):            return 8;
        case NodeType(Literal::lambda_param):      return 10;
        case NodeType(Literal::symbol):            return 2000;
        case NodeType(Literal::call):              return 2002;
        case NodeType(PatternCall{}):	           return 2002;
        case NodeType(SingleMatch::restricted):    return 2100;  //caution: these will compare equal, despite having a different structure in store
        case NodeType(SingleMatch::unrestricted):  return 2100;  //caution: these will compare equal, despite having a different structure in store
        case NodeType(SingleMatch::weak):          return 2100;  //caution: these will compare equal, despite having a different structure in store
        case NodeType(SpecialMatch::multi):        return 3000;
        case NodeType(Literal::native):
            using namespace nv;
            static_assert((unsigned)Native::COUNT < 1000, "adjust values >= 2000 to circumvent overlap");
            switch (Native(ref.index)) {
            default:                                 return ref.index + 1000;
            case Native(PatternConst::multi_marker): return 3001;
            }
        default:
            assert(false);
            BMATH_UNREACHABLE;
            return false;
        }
    } //shallow_order

    //lexicographic ordering edtending shallow_order, not meaningful in a math context
    std::strong_ordering compare_tree(const UnsaveRef fst, const UnsaveRef snd);

    //can not differentiate calls to same function and can not differentiate match variables from anything
    std::partial_ordering unsure_compare_tree(const UnsaveRef fst, const UnsaveRef snd);

    //returns first subterm where pred is true, tested in pre-order
    template<std::predicate<UnsaveRef> Pred>
    NodeIndex search(const UnsaveRef ref, Pred pred)
    {
        assert(ref.type != PatternCall{});
        if (pred(ref)) {
            return ref.typed_idx();
        }
        else if (ref.type == Literal::lambda) {
            return search(ref.new_at(ref->lambda.definition), pred);
        }
        else if (ref.type == Literal::call) {
            for (const NodeIndex subterm : ref->call) {
                const NodeIndex sub_res = search(ref.new_at(subterm), pred);
                if (sub_res != literal_nullptr) {
                    return sub_res;
                }
            }
        }
        return literal_nullptr;
    } //search

    namespace build_rule {

        // - multi match variables are primed
        // - every function call to nv::Comm in lhs is converted to PatternCall
        // - if a call in lhs contains at least one multi match, the call is converted to PatternCall
        RuleHeads prime_multi(Store& store, RuleHeads heads);

        //returns a fully assembled rule
        RuleHeads build_everything(Store& store, std::string& name);

    } //namespace build_rule
    
} //namespace simp
