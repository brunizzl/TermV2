#pragma once

#include "types.hpp"

#include <compare>

namespace simp {

    bool in_domain(const Complex& nr, const Domain domain);

    //only interested in final results -> a function call guaranteed to return something meeting restr results in false
    bool meets_restriction(const UnsaveRef ref, const Restriction restr);

    namespace combine {
        struct Options 
        {
            bool recurse = true; //true: first combines subterms of MathType (no recursion for any of PatternType!)
            bool eval_values = true; //true: sums, products... (and calls to true/ false) are evaluated as much as possible (e.g. "1 + a + 3 -> 4 + a")
            bool exact = true; //(only significant if eval_values) true: only exact operations are permitted
            bool eval_equality = true; //fn::FixedArity::eq and ...neq can always be evaluated -> evaluate them (not intelligent for pattern restrictions)
            bool eval_lambdas = true; //seems to be self explanatory
            bool normalize_lambdas = true; //true: all nested lambdas become transparent & unessecary indirections are removed
            bool remove_unary_assoc = true; //true: "f(a) -> a" for all associative f (e.g. sum, product, and...)
        };

        //will always evaluate exact operations and merge nested calls of an associative operation
        //  (meaning "f(as, f(bs...), cs...) -> f(as..., bs..., cs...)" for associative f)
        //lambda_param_offset counts how many parameters all parent lambdas hold together 
        //  example: if subterm "a" would be combined in term "\x.\y z. (a + 7)", lambda_param_offset == 3
        //returns combined ref, might invalidate old one
        //may throw if: a lambda is called with to many parameters or something not callable is called
        [[nodiscard]] TypedIdx combine_(MutRef ref, const Options options, const unsigned lambda_param_offset);
    } //namespace combine


    //frees subtree starting at ref
    void free_tree(const MutRef ref);

    //copies tree starting at src_ref into dst_store
    [[nodiscard]] TypedIdx copy_tree(const Ref src_ref, Store& dst_store);

    //lexicographic ordering, not meaningful in a math context
    std::strong_ordering compare_tree(const UnsaveRef fst, const UnsaveRef snd);
    
} //namespace simp
