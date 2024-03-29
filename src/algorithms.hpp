#pragma once

#include <compare>
#include <concepts>
#include <string>
#include <compare>
#include <cassert>
#include <ranges>

#include "utility/meta.hpp"
#include "utility/vector.hpp"
#include "utility/array.hpp"

#include "types.hpp"
#include "io.hpp"

namespace simp {

    bool in_complex_subset(const Complex& nr, const nv::ComplexSubset domain);

    //only interested in final results -> a function call guaranteed to return something meeting restr results in false
    bool meets_restriction(const UnsaveRef ref, const nv::Native restr);

    //functions in namespace normalize behave differently depending on these values
    struct Options 
    {
        bool exact = true; //true: only exact operations are permitted
        bool remove_unary_assoc = true; //true: "f(a) -> a" for all associative f (e.g. sum, product, and...)
        bool eval_special = true; //replace is evaluated
        bool eval_lambda = true;
    };

    //return value of functions maybe changing a subterm, e.g. normalizing (directly below) or applying patterns
    struct AlteredIndex 
    { 
        NodeIndex term; 
        bool change; 
    };

    namespace normalize {

        //will always evaluate exact operations and merge nested calls of an associative operation
        //  (meaning "f(as..., f(bs...), cs...) -> f(as..., bs..., cs...)" for associative f)
        //  example: if subterm "a" would be combined in term "\x.\y z. (a + 7)", lambda_param_offset == 3
        //returns combined ref, might invalidate old one
        //may throw if: a lambda is called with to many parameters or something not applicable is called
        [[nodiscard]] AlteredIndex outermost(MutRef ref, const Options options);

        //applies outermost first to children then to itself
        //as an exception, calls to fn if nv::is_lazy(fn) are lazily evaluated 
        [[nodiscard]] NodeIndex recursive(MutRef ref, const Options options);
    } //namespace normalize


    //frees subtree starting at ref
    void free_tree(const MutRef ref);

    constexpr void free_app_shallow(const MutRef ref) noexcept
    {   assert(ref.type == Literal::f_app);
        if (ref.store->decr_at(ref.index) == 0) FApp::free(*ref.store, ref.index);
    }

    //assumes index to point at a single node, not a node block
    constexpr void free_node_shallow(Store& store, const std::size_t index) noexcept
    {   if (store.decr_at(index) == 0) store.free_one(index);
    }

    constexpr [[nodiscard]] NodeIndex share(Store& store, const NodeIndex idx) noexcept
    {   if (is_stored_node(idx.get_type())) store.incr_at(idx.get_index());
        return idx;
    }

    constexpr [[nodiscard]] NodeIndex share(const MutRef ref) noexcept
    {   if (is_stored_node(ref.type)) ref.store->incr_at(ref.index);
        return ref.typed_idx();
    }

    //copies tree starting at src_ref into dst_store
    template<Reference R, StoreLike S>
    [[nodiscard]] NodeIndex copy_tree(const R src_ref, S& dst_store);

    //orders node types by their "specificity", meaning that types potentially matching 
    //  more things are generally sorted to the back
    constexpr inline char shallow_order(const NodeType type) {
        constexpr auto table = arr::sort_first_keep_second(
            std::to_array<std::pair<NodeType, char>>({
                { Literal::complex         , 0  },
                { SpecialMatch::value      , 2  },
                { PatternUnsigned{}        , 4  },
                { Literal::lambda          , 8  },
                { Literal::lambda_param    , 10 },
                { Literal::symbol          , 20 },
                { Literal::f_app           , 30 },
                { PatternFApp{}            , 30 },
                { SingleMatch::restricted  , 50 },
                { SingleMatch::unrestricted, 50 },
                { SingleMatch::weak        , 50 },
                { SpecialMatch::multi      , 60 },
            }));
        static_assert(table.size() == (unsigned)NodeType::COUNT);

        assert(type < NodeType(NodeType::COUNT));
        return table[(unsigned)type];
    } //shallow_order

    //lexicographic ordering extending shallow_order, not meaningful in a math context
    std::strong_ordering compare_tree(const UnsaveRef fst, const UnsaveRef snd);

    //can not differentiate calls to same function from each other
    //  and can not differentiate match variables from anything
    std::partial_ordering unsure_compare_tree(const UnsaveRef fst, const UnsaveRef snd);

    //returns a function comparing two NodeIndices assumed they both point in store_data
    template<std::strong_ordering Ord>
    constexpr auto ordered(const TermNode* const store_data) 
    {
        return [store_data](const NodeIndex fst, const NodeIndex snd) {
            const UnsaveRef fst_ref = UnsaveRef(store_data, fst.get_index(), fst.get_type());
            const UnsaveRef snd_ref = UnsaveRef(store_data, snd.get_index(), snd.get_type());
            return compare_tree(fst_ref, snd_ref) == Ord;
        };
    } //ordered
    inline constexpr auto ordered_less = ordered<std::strong_ordering::less>;

    //deepest depth, where a parameter belonging to a lambda is held; ref is assumed to be lambda.definition in first call
    //if no parameter belonging to lambda could be found, a negative number is returned
    //a parameter is counted as 'belonging to the lambda' even if it only is part of a transparent lambda within, 
    //because evaluation of the outer lambda needs to reach the inner one to subtract the outer parameter count
    int owned_lambda_depth(const UnsaveRef ref);

    namespace build_rule {

        //turns "_Xn[_Xn' :<some_type>]" into "_Xn[<some_type>]"
        [[nodiscard]] RuleHead optimize_single_conditions(Store& store, RuleHead head);

        //value match variables intermediary form are bubbled up as high as possible
        //intermediary is converted to final form and ownership is decided (thus sorts bevore that)
        [[nodiscard]] RuleHead prime_value(Store& store, RuleHead head);

        // - every function call in lhs is converted to PatternFApp
        // - multi match variables are primed
        //note: as after this procedure there may be PatterCall instances present, this may be done as last real transformation
        [[nodiscard]] RuleHead prime_f_app(Store& store, RuleHead head);

        //returns a fully assembled rule
        [[nodiscard]] RuleHead build_everything(Store& store, std::string& name);

    } //namespace build_rule

    namespace match {

        //compares term starting at ref.index in ref.store with pattern starting at pn_ref.index in pn_ref.store
        //if match is succsessfull, match_state stores what pattern's match variables matched and equivalent is returned.
        //if match was not successfull, match_state is NOT reset and something else than equivalent is returned
        std::partial_ordering match_(const UnsaveRef pn_ref, const UnsaveRef ref, State& match_state);

        inline bool find_match(const UnsaveRef pn_ref, const UnsaveRef ref, State& match_state)
        {   return match_(pn_ref, ref, match_state) == std::partial_ordering::equivalent;
        }

        // expects pn_ref to already be matched to ref via match_state
        //if one exists, this function finds a different match of pn_ref in ref, appearing after the current one
        //  in all permutations
        bool rematch(const UnsaveRef pn_ref, const UnsaveRef ref, State & match_state);

        //returns an entry for each (non-multi) match variable
        std::vector<std::string> print_state(const State& state);

    } //namespace match

    //copies pn_ref like copy_tree, only match variables are replaced by their matched counterparts from src_store.
    [[nodiscard]] NodeIndex pattern_interpretation(const UnsaveRef pn_ref, const match::State& match_state,
        Store& store, const Options options);
    
} //namespace simp
