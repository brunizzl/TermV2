#pragma once

#include "types.hpp"

namespace simp {

    namespace combine {
        struct Options 
        {
            bool recurse = true; //true: first combines subterms of MathType (no recursion for any of PatternType!)
            bool eval_values = true; //true: sums, products... are evaluated as much as possible ("1 + a + 3 -> 4 + a")
            bool exact = true; //(only significant if eval_values) true: only exact operations are permitted
            bool eval_lambdas = true; //true: lambdas are evaluated
            bool normalize_lambdas = true; //true: all nested lambdas become transparent & unessecary indirections are removed
            bool remove_unary_assoc = true; //true: "f(a) -> a" for all associative f (e.g. sum, product, and...)
            bool sort = true; //true: fn::Comm calls are sorted
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
    
} //namespace simp
