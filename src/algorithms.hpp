#pragma once

#include <compare>
#include <concepts>
#include <string>

#include "types.hpp"
#include "io.hpp"
#include "utility/meta.hpp"

namespace simp {

    bool in_complex_subset(const Complex& nr, const nv::ComplexSubset domain);

    //only interested in final results -> a function call guaranteed to return something meeting restr results in false
    bool meets_restriction(const UnsaveRef ref, const nv::Native restr);

    namespace normalize {
        struct Options 
        {
            bool exact = true; //true: only exact operations are permitted
            bool eval_haskell = true; //true: functions in nv::HaskellFn are evaluated
            bool normalize_lambdas = true; //true: all nested lambdas become transparent & unessecary indirections are removed
            bool remove_unary_assoc = true; //true: "f(a) -> a" for all associative f (e.g. sum, product, and...)
        };

        struct Result { NodeIndex res; bool change; };
        //will always evaluate exact operations and merge nested calls of an associative operation
        //  (meaning "f(as..., f(bs...), cs...) -> f(as..., bs..., cs...)" for associative f)
        //lambda_param_offset counts how many parameters all parent lambdas hold together 
        //  example: if subterm "a" would be combined in term "\x.\y z. (a + 7)", lambda_param_offset == 3
        //returns combined ref, might invalidate old one
        //may throw if: a lambda is called with to many parameters or something not callable is called
        [[nodiscard]] Result outermost(MutRef ref, const Options options, const unsigned lambda_param_offset);

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
            default:                               return ref.index + 1000;
            }
        default:
            assert(false);
            BMATH_UNREACHABLE;
            return false;
        }
    } //shallow_order

    //lexicographic ordering extending shallow_order, not meaningful in a math context
    std::strong_ordering compare_tree(const UnsaveRef fst, const UnsaveRef snd);

    //returns a function comparing two NodeIndices assumed their index points in store_data
    constexpr auto compare_less_in(const TermNode* const store_data) 
    {
        return [store_data](const NodeIndex fst, const NodeIndex snd) {
            const std::uint32_t fst_idx = fst.get_index();
            const std::uint32_t snd_idx = snd.get_index();
            const UnsaveRef fst_ref = UnsaveRef(store_data + fst_idx, fst_idx, fst.get_type());
            const UnsaveRef snd_ref = UnsaveRef(store_data + snd_idx, snd_idx, snd.get_type());
            return compare_tree(fst_ref, snd_ref) == std::strong_ordering::less;
        };
    } //compare_in

    //can not differentiate calls to same commutative function from each other
    //  and can not differentiate match variables from anything
    std::partial_ordering unsure_compare_tree(const UnsaveRef fst, const UnsaveRef snd);

    //returns first subterm where pred is true, tested in pre-order
    template<std::predicate<UnsaveRef> Pred>
    NodeIndex search(const UnsaveRef ref, Pred pred)
    {
        if (pred(ref)) {
            return ref.typed_idx();
        }
        else if (ref.type == Literal::lambda) {
            return search(ref.new_at(ref->lambda.definition), pred);
        }
        else if (ref.type == Literal::call || ref.type == PatternCall{}) {
            for (const NodeIndex subterm : ref->call) {
                const NodeIndex sub_res = search(ref.new_at(subterm), pred);
                if (sub_res != literal_nullptr) {
                    return sub_res;
                }
            }
        }
        return literal_nullptr;
    } //search

    //applies f to every subterm in postorder, result is new subterm
    template<bmath::intern::Reference R, bmath::intern::CallableTo<NodeIndex, R> F>
    [[nodiscard]] NodeIndex transform(const R ref, F f) 
    {
        if (ref.type == Literal::call || ref.type == PatternCall{}) {
            const auto stop = end(ref);
            for (auto iter = begin(ref); iter != stop; ++iter) {
                *iter = transform(ref.new_at(*iter), f);
            }
        }
        if (ref.type == Literal::lambda) {
            ref->lambda.definition = transform(ref.new_at(ref->lambda.definition), f);
        }
        return f(ref);
    } //transform

    //applies f to every subterm in postorder
    template<bmath::intern::Reference R, bmath::intern::Procedure<R> F>
    void transform(const R ref, F f)
    {
        if (ref.type == Literal::call || ref.type == PatternCall{}) {
            for (const auto sub : ref) {
                transform(ref.new_at(sub), f);
            }
        }
        if (ref.type == Literal::lambda) {
            transform(ref.new_at(ref->lambda.definition), f);
        }
        f(ref);
    } //transform

    namespace build_rule {

        //turns "_Xn[_Xn' :<some_type>]" into "_Xn[<some_type>]"
        RuleHead optimize_single_conditions(Store& store, RuleHead head);

        //value match variables intermediary form are bubbled up as high as possible
        //intermediary is converted to final form and ownership is decided (thus sorts bevore that)
        RuleHead prime_value(Store& store, RuleHead head);

        // - every function call to nv::Comm in lhs is converted to PatternCall
        // - if a call in lhs contains at least one multi match, the call is converted to PatternCall
        // - multi match variables are primed
        //note: as after this procedure there may be PatterCall instances present, this may be done as last real transformation
        RuleHead prime_call(Store& store, RuleHead head);

        //if the outermost node of lhs is associative and 
        //  eighter commutative and does not contain a multi match variable
        //  or non-commutative and multi match variables are not present in the front and back
        //these missing multi match variables are added
        RuleHead add_implicit_multis(Store& store, RuleHead head);

        //sets /unsets bits in data of PatternCall
        //note: will only change something if prime_call has already run
        void set_always_preceeding_next(const MutRef head_lhs);

        //sets /unsets bits in data of PatternCall
        //note: will only change something if prime_call has already run
        //note2: for less conservative results, first run set_always_preceeding_next()
        void set_rematchable(const MutRef head_lhs);

        //returns a fully assembled rule
        RuleHead build_everything(Store& store, std::string& name);

    } //namespace build_rule

    namespace match {

        //decides if a condition appended to a single match variable is met with the current match data
        bool test_condition(const UnsaveRef cond, const MatchData& match_data);

        //(transiently) evaluates .match_index of ValueMatch for a given start_val
        bmath::intern::OptionalComplex eval_value_match(const UnsaveRef ref, const Complex& start_val);

        //compares term starting at ref.index in ref.store with pattern starting at pn_ref.index in pn_ref.store
        //if match is succsessfull, match_data stores what pattern's match variables matched and true is returned.
        //if match was not succsessfull, match_data is NOT reset and false is returned
        bool match_(const UnsaveRef pn_ref, const UnsaveRef ref, MatchData& match_data);

        // expects pn_ref to already be matched to ref via match_data
        //if one exists, this function finds a different match of pn_ref in ref, appearing after the current one
        //  in all permutations
        bool rematch(const UnsaveRef pn_ref, const UnsaveRef ref, MatchData & match_data);

        //determines weather there is a way to match needle_ref in hay_ref (thus needle_ref is assumed to part of a pattern)
        //needle_i is the index of the first element in needle_ref to be matched. 
        //if needle_i is not zero, it is assumed, that all previous elements in needle_ref are already matched (=> the call happens in a rematch).
        //the first haystack_k elements of hay_ref will be skipped for the first match attemt.
        //it is assumed, that needle_ref and hay_ref are both calls to the same nv::Comm, where pn_ref is a PatternCall
        //returns true if a match was found
        bool find_permutation(const UnsaveRef needle_ref, const UnsaveRef hay_ref,
            MatchData& match_data, std::uint32_t needle_i, std::uint32_t hay_k);

        //analogous to find_permutation, but for non commutative pattern containing at least one multi_marker
        bool find_dilation(const UnsaveRef needle_ref, const UnsaveRef hay_ref,
            MatchData& match_data, std::uint32_t needle_i, std::uint32_t hay_k);

    } //namespace match

    //copies pn_ref like copy_tree, only match variables are replaced by their matched counterparts from src_store.
    [[nodiscard]] NodeIndex copy_pattern_interpretation(const UnsaveRef pn_ref, const match::MatchData& match_data,
        const Store& src_store, Store& dst_store, const unsigned lambda_param_offset);
    
} //namespace simp
