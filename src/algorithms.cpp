#include "algorithms.hpp"

#include <tuple>

#include "utility/bit.hpp"
#include "utility/vector.hpp"
#include "utility/misc.hpp"


#include "termVector.hpp"




namespace simp {
    namespace combine {

        void BMATH_FORCE_INLINE merge_associative_calls(MutRef& ref, const TypedIdx function)
        {
            assert(ref.type == MathType::call && function.get_type() == MathType::buildin && 
                fn::from_typed_idx(function).is<fn::Variadic>());
            bool found_nested = false;
            const Call& call = *ref;
            bmath::intern::StupidBufferVector<TypedIdx, 16> merged_calls = { function };
            for (const TypedIdx param : call.parameters()) {
                if (param.get_type() == MathType::call) {
                    const MutRef param_ref = ref.new_at(param);
                    if (param_ref->call.function() == function) {
                        found_nested = true;
                        for (const TypedIdx param_param : param_ref->call.parameters()) {
                            merged_calls.push_back(param_param);
                        }
                        Call::free(*ref.store, param_ref.index); //only free call itself not the (now copied) params
                        continue;
                    }
                }
                merged_calls.push_back(param);
            }
            if (found_nested) {
                if (call.capacity() >= merged_calls.size()) {
                    Call::emplace(*ref, merged_calls, call.capacity());
                }
                else {
                    Call::free(*ref.store, ref.index); //only free old call, not its parameters
                    ref.index = Call::build(*ref.store, merged_calls);
                }
            }
        } //merge_associative

        TypedIdx BMATH_FORCE_INLINE eval_buildin(const MutRef ref, const TypedIdx function, const bool exact)
        {
            //TODO
            return TypedIdx();
        } //eval_buildin

        [[nodiscard]] TypedIdx replace_lambda_params(const MutRef ref, const bmath::intern::StupidBufferVector<TypedIdx, 16>& params, 
            bmath::intern::BitVector& used_params)
        {
            if (ref.type == MathType::call) {
                const auto stop = end(ref);
                for (auto iter = begin(ref); iter != stop; ++iter) {
                    *iter = replace_lambda_params(ref.new_at(*iter), params, used_params);
                }
            }
            else if (ref.type == MathType::lambda) {
                ref->lambda.definition = replace_lambda_params(ref.new_at(ref->lambda.definition), params, used_params);
            }
            else if (ref.type == MathType::lambda_param) {
                if (ref.index >= params.size()) { 
                    //function is not fully evaluated but only curried -> new param index = old - (number evaluated)
                    return TypedIdx(ref.index - params.size(), MathType::lambda_param);
                }
                else if (used_params.test(ref.index)) {
                    return copy_tree(ref.new_at(params[ref.index]), *ref.store);
                }
                else {
                    used_params.set(ref.index);
                    return params[ref.index];
                }
            }
            return ref.typed_idx();
        } //replace_lambda_params

        [[nodiscard]] TypedIdx BMATH_FORCE_INLINE eval_lambda(const MutRef ref)
        {
            assert(ref.type == MathType::call);
            const Call& call = *ref;
            const MutRef lambda = ref.new_at(call.function());
            assert(lambda.type == MathType::lambda);
            bmath::intern::StupidBufferVector<TypedIdx, 16> params;
            for (const TypedIdx param : call.parameters()) {
                params.push_back(param);
            }
            const std::uint32_t lambda_param_count = lambda->lambda.param_count;
            if (lambda_param_count < params.size()) [[unlikely]] {
                throw "too many arguments for called lambda";
            }
            Call::free(*ref.store, ref.index); //only free call itself, neighter lambda nor lambda parameters
            bmath::intern::BitVector used_params(params.size());
            if (lambda_param_count == params.size()) {
                const TypedIdx definition = lambda->lambda.definition;
                ref.store->free_one(lambda.index);
                return replace_lambda_params(ref.new_at(definition), params, used_params);
            }
            else { //lambda is curried -> keep remaining lambda as lambda
                assert(lambda_param_count > params.size());
                lambda->lambda.definition = 
                    replace_lambda_params(ref.new_at(lambda->lambda.definition), params, used_params);
                lambda->lambda.param_count -= params.size();
                return lambda.typed_idx();
            }
        } //eval_lambda

        
        [[nodiscard]] TypedIdx add_offset(const MutRef ref, const unsigned lambda_param_offset)
        {
            if (ref.type == MathType::call) {
                for (TypedIdx& subterm : ref) {
                    subterm = add_offset(ref.new_at(subterm), lambda_param_offset);
                }
            }
            else if (ref.type == MathType::lambda) {
                Lambda& lambda = *ref;
                if (lambda.transparent) {
                    lambda.definition = add_offset(ref.new_at(lambda.definition), lambda_param_offset);
                }
            }
            else if (ref.type == MathType::lambda_param) {
                const std::uint32_t new_index = ref.index + lambda_param_offset;
                return TypedIdx(new_index, MathType::lambda_param);
            }
            return ref.typed_idx();
        }

        TypedIdx combine_(MutRef ref, const Options options, const unsigned lambda_param_offset)
        {
            if (ref.type == MathType::call) {                
                if (options.recurse) {
                    const auto stop = end(ref);                    
                    for (auto iter = begin(ref); iter != stop; ++iter) {
                        *iter = combine_(ref.new_at(*iter), options, lambda_param_offset);
                    }
                }
                const TypedIdx function = ref->call.function();
                switch (function.get_type()) {
                case Type(MathType::buildin): {
                    const fn::Buildin buildin_type = fn::from_typed_idx(function);
                    const bool associative = buildin_type.is<fn::Variadic>() && 
                        fn::is_associative(buildin_type.to<fn::Variadic>());
                    if (associative) {
                        merge_associative_calls(ref, function);
                    }
                    if (options.eval_values) {
                        const TypedIdx res = eval_buildin(ref, function, options.exact);
                        if (res != TypedIdx()) {
                            return res;
                        }
                    }
                    if (associative && options.remove_unary_assoc) {
                        const Call& call = *ref;
                        if (call.size() == 2u) { //only contains function type and single parameter
                            const TypedIdx single_param = call[1u];
                            Call::free(*ref.store, ref.index);
                            return single_param;
                        }
                    }
                    if (options.sort && buildin_type.is<fn::Comm>()) {
                        //TODO
                    }
                } break;
                case Type(MathType::lambda): {
                    if (options.eval_lambdas) {
                        return eval_lambda(ref);
                    }
                } break;
                case Type(MathType::boolean): {
                    const Call& call = *ref;
                    if (call.size() != 3u) [[unlikely]] {
                        throw "boolean may only be called as binary function";
                    }
                    if (options.eval_values || options.eval_lambdas) {
                        const TypedIdx res = [&]() {
                            if (function.get_index()) {
                                free_tree(ref.new_at(call[2]));
                                return call[1];
                            }
                            else {
                                free_tree(ref.new_at(call[1]));
                                return call[2];
                            }
                        }();
                        Call::free(*ref.store, ref.index);
                        return res;
                    }
                } break;
                }
            } //end ref.type == MathType::call
            else if (ref.type == MathType::lambda) {
                assert(!ref->lambda.transparent || lambda_param_offset != 0u && 
                    "no top-level lambda may be transparent!"); 
                if (options.recurse) {
                    const Lambda lambda = *ref;
                    const unsigned new_offset = lambda.transparent ?
                        lambda.param_count + lambda_param_offset :
                        lambda.param_count;
                    ref->lambda.definition = combine_(ref.new_at(lambda.definition), options, new_offset);
                }
                if (options.normalize_lambdas) {
                    Lambda& lambda = *ref;
                    if (!lambda.transparent && lambda_param_offset != 0u) {
                        lambda.transparent = true;
                        lambda.definition = add_offset(ref.new_at(lambda.definition), lambda_param_offset);
                    }
                    if (lambda.definition.get_type() == MathType::lambda) {
                        //relies on this beeing transparent
                        const UnsaveRef nested = ref.new_at(lambda.definition);
                        lambda.definition = nested->lambda.definition;
                        lambda.param_count += nested->lambda.param_count;
                        lambda.transparent = nested->lambda.transparent;
                        ref.store->free_one(nested.index);
                    }
                }
            } //end ref.type == MathType::lambda
            return ref.typed_idx();
        } //combine_

    } //namespace combine

    void free_tree(const MutRef ref)
    {
        switch (ref.type) {
        case Type(MathType::complex):
            ref.store->free_one(ref.index);
            break;
        case Type(MathType::symbol):
            Symbol::free(*ref.store, ref.index);
            break;
        case Type(MathType::call):
            if (ref->call.function().get_type().is<PatternBuildin>()) {
                ref.store->free_one(ref.index - 1u);                
            }
            for (const TypedIdx subtree : ref) {
                free_tree(ref.new_at(subtree));
            }
            Call::free(*ref.store, ref.index);
            break;
        case Type(MathType::lambda):
            free_tree(ref.new_at(ref->lambda.definition));
            ref.store->free_one(ref.index);
            break;
        case Type(SingleMatch::restricted):            
            if (const TypedIdx condition = ref->single_match.condition; 
                condition != TypedIdx()) 
            {
                free_tree(ref.new_at(condition));
            }
            ref.store->free_one(ref.index);
            break;
        case Type(ValueMatch::strong):
            free_tree(ref.new_at(ref->value_match.match_index));
            ref.store->free_one(ref.index);            
            break;
        default:
            assert(!is_node(ref.type));
        }
    } //free_tree

    TypedIdx copy_tree(const Ref src_ref, Store& dst_store)
    {
        if (!is_node(src_ref.type)) {
            return src_ref.typed_idx();
        }

        const auto insert_node = [&](const TermNode& n, Type type) {
            const std::size_t dst_index = dst_store.allocate_one();
            dst_store.at(dst_index) = n; 
            return TypedIdx(dst_index, type);            
        };

        switch (src_ref.type) {
        case Type(MathType::complex):
            return insert_node(*src_ref, src_ref.type);
        case Type(MathType::symbol): {
            const Symbol& src_var = *src_ref;
            const auto src_name = std::string(src_var.data(), src_var.size());
            const std::size_t dst_index = Symbol::build(dst_store, src_name);
            return TypedIdx(dst_index, src_ref.type);
        } break;
        case Type(MathType::call): {
            bmath::intern::StupidBufferVector<TypedIdx, 16> dst_subterms;
            const auto stop = end(src_ref);
            for (auto iter = begin(src_ref); iter != stop; ++iter) {
                dst_subterms.push_back(copy_tree(src_ref.new_at(*iter), dst_store));
            }
            if (dst_subterms.front().get_type() == PatternBuildin{}) { //also copy variadic metadata
                const std::size_t call_capacity = Call::smallest_fit_capacity(dst_subterms.size());
                const std::size_t nr_call_nodes = Call::_node_count(call_capacity);

                const std::size_t dst_index = dst_store.allocate_n(1u + nr_call_nodes) + 1u;
                dst_store.at(dst_index - 1u) = variadic_meta_data(src_ref);
                Call::emplace(dst_store.at(dst_index), dst_subterms, call_capacity);
                return TypedIdx(dst_index, src_ref.type);
            }
            else {
                const std::size_t dst_index = Call::build(dst_store, dst_subterms);
                return TypedIdx(dst_index, src_ref.type);
            }
        } break;
        case Type(MathType::lambda): {
            Lambda lambda = *src_ref;
            lambda.definition = copy_tree(src_ref.new_at(lambda.definition), dst_store);
            return insert_node(lambda, src_ref.type);
        } break;
        case Type(SingleMatch::restricted): {
            RestrictedSingleMatch var = *src_ref;
            if (var.condition != TypedIdx()) {
                var.condition = copy_tree(src_ref.new_at(var.condition), dst_store);
            }
            return insert_node(var, src_ref.type);
        } break;
        case Type(ValueMatch::strong): {
            StrongValueMatch var = *src_ref;
            var.match_index = copy_tree(src_ref.new_at(var.match_index), dst_store);
            return insert_node(var, src_ref.type);
        } break;
        default:
            assert(false);
            BMATH_UNREACHABLE;
            return TypedIdx();
        }
    } //copy_tree

    //namespace combine    
} //namespace simp
