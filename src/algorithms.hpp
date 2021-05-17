#pragma once

#include <compare>
#include <concepts>
#include <string>
#include <compare>
#include <cassert>

#include "types.hpp"
#include "io.hpp"
#include "utility/meta.hpp"

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
    };

    namespace normalize {

        struct Result { NodeIndex res; bool change; };
        //will always evaluate exact operations and merge nested calls of an associative operation
        //  (meaning "f(as..., f(bs...), cs...) -> f(as..., bs..., cs...)" for associative f)
        //  example: if subterm "a" would be combined in term "\x.\y z. (a + 7)", lambda_param_offset == 3
        //returns combined ref, might invalidate old one
        //may throw if: a lambda is called with to many parameters or something not applicable is called
        [[nodiscard]] Result outermost(MutRef ref, const Options options);

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

    constexpr void share(const MutRef ref) noexcept
    {   if (is_stored_node(ref.type)) ref.store->incr_at(ref.index);
    }

    //copies tree starting at src_ref into dst_store
    template<bmath::intern::Reference R, bmath::intern::StoreLike S>
    [[nodiscard]] NodeIndex copy_tree(const R src_ref, S& dst_store);

    //orders node types by their "specificity", meaning that types potentially matching more things are generally sorted to the back
    constexpr int shallow_order(const UnsaveRef ref) {
        constexpr int after_symbol = std::numeric_limits<int>::max() - 10000;
        switch (ref.type) {
        case NodeType::COUNT:                      return -1;
        case NodeType(Literal::complex):           return 0;
        case NodeType(SpecialMatch::value):        return 2;
        case NodeType(PatternUnsigned{}):          return 4;
        case NodeType(Literal::lambda):            return 8;
        case NodeType(Literal::lambda_param):      return 10;
        case NodeType(Literal::symbol):
            assert(ref.index < after_symbol); //overflow of typed index happens sooner anyway
            switch (nv::Native(ref.index)) {
            default:                               return ref.index + 1000;
            }
        case NodeType(Literal::f_app):             return after_symbol + 100;
        case NodeType(PatternFApp{}):	           return after_symbol + 100;
        case NodeType(SingleMatch::restricted):    return after_symbol + 200;  //caution: these will compare equal, despite having a different structure in store
        case NodeType(SingleMatch::unrestricted):  return after_symbol + 200;  //caution: these will compare equal, despite having a different structure in store
        case NodeType(SingleMatch::weak):          return after_symbol + 200;  //caution: these will compare equal, despite having a different structure in store
        case NodeType(SpecialMatch::multi):        return after_symbol + 300;
        default:
            assert(false);
            BMATH_UNREACHABLE;
            return false;
        }
    } //shallow_order

    //lexicographic ordering extending shallow_order, not meaningful in a math context
    std::strong_ordering compare_tree(const UnsaveRef fst, const UnsaveRef snd);

    //can not differentiate calls to same commutative function from each other
    //  and can not differentiate match variables from anything
    std::partial_ordering unsure_compare_tree(const UnsaveRef fst, const UnsaveRef snd);

    //returns a function comparing two NodeIndices assumed they both point in store_data
    template<auto Ord, auto compare>
    constexpr auto ordered(const TermNode* const store_data) 
    {
        static_assert((void*)compare == (void*)compare_tree || (void*)compare == (void*)unsure_compare_tree);
        return [store_data](const NodeIndex fst, const NodeIndex snd) {
            const UnsaveRef fst_ref = UnsaveRef(store_data, fst.get_index(), fst.get_type());
            const UnsaveRef snd_ref = UnsaveRef(store_data, snd.get_index(), snd.get_type());
            return compare(fst_ref, snd_ref) == Ord;
        };
    } //ordered
    constexpr auto ordered_less = ordered<std::strong_ordering::less, compare_tree>;



    //returns first subterm where pred is true, tested in pre-order
    template<std::predicate<UnsaveRef> Pred>
    NodeIndex search(const UnsaveRef ref, Pred pred)
    {
        if (pred(ref)) {
            return ref.typed_idx();
        }
        else if (ref.type == Literal::f_app || ref.type == PatternFApp{}) {
            for (const NodeIndex subterm : ref->f_app) {
                const NodeIndex sub_res = search(ref.at(subterm), pred);
                if (sub_res != literal_nullptr) {
                    return sub_res;
                }
            }
        }
        else if (ref.type == Literal::lambda) {
            return search(ref.at(ref->lambda.definition), pred);
        }
        return literal_nullptr;
    } //search

    //applies f to every subterm in postorder, result is new subterm
    template<bmath::intern::Reference R, bmath::intern::CallableTo<NodeIndex, R> F>
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
    template<bmath::intern::Reference R, bmath::intern::Procedure<R> F>
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

    namespace build_rule {

        //turns "_Xn[_Xn' :<some_type>]" into "_Xn[<some_type>]"
        [[nodiscard]] RuleHead optimize_single_conditions(Store& store, RuleHead head);

        //value match variables intermediary form are bubbled up as high as possible
        //intermediary is converted to final form and ownership is decided (thus sorts bevore that)
        [[nodiscard]] RuleHead prime_value(Store& store, RuleHead head);

        // - every function call to nv::Comm in lhs is converted to PatternFApp
        // - if a call in lhs contains at least one multi match, the call is converted to PatternFApp
        // - multi match variables are primed
        //note: as after this procedure there may be PatterCall instances present, this may be done as last real transformation
        [[nodiscard]] RuleHead prime_f_app(Store& store, RuleHead head);

        //if the outermost node of lhs is associative and 
        //  eighter commutative and does not contain a multi match variable
        //  or non-commutative and multi match variables are not present in the front and back
        //these missing multi match variables are added
        [[nodiscard]] RuleHead add_implicit_multis(Store& store, RuleHead head);

        //returns a fully assembled rule
        [[nodiscard]] RuleHead build_everything(Store& store, std::string& name);

    } //namespace build_rule

    namespace match {

        //compares term starting at ref.index in ref.store with pattern starting at pn_ref.index in pn_ref.store
        //if match is succsessfull, match_state stores what pattern's match variables matched and equivalent is returned.
        //if match was not successfull, match_state is NOT reset and something else than equivalent is returned
        std::partial_ordering match_(const UnsaveRef pn_ref, const UnsaveRef ref, State& match_state);

        inline bool matches(const UnsaveRef pn_ref, const UnsaveRef ref, State& match_state)
        {   return match_(pn_ref, ref, match_state) == std::partial_ordering::equivalent;
        }

        // expects pn_ref to already be matched to ref via match_state
        //if one exists, this function finds a different match of pn_ref in ref, appearing after the current one
        //  in all permutations
        bool rematch(const UnsaveRef pn_ref, const UnsaveRef ref, State & match_state);

    } //namespace match

    //copies pn_ref like copy_tree, only match variables are replaced by their matched counterparts from src_store.
    [[nodiscard]] NodeIndex pattern_interpretation(const UnsaveRef pn_ref, const match::State& match_state,
        Store& store, const Options options);
    
} //namespace simp
