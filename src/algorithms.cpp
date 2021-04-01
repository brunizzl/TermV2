#include "algorithms.hpp"

#include <tuple>

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

        void BMATH_FORCE_INLINE eval_lambda(MutRef& lambda_call)
        {
            //TODO
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
                        eval_lambda(ref);
                    }
                } break;
                case Type(MathType::boolean): {
                    const Call& call = *ref;
                    if (call.size() != 3u) [[unlikely]] {
                        throw "boolean may only be called as binary function";
                    }
                    if (options.eval_lambdas) {
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
                if (options.normalize_lambdas) {
                    Lambda& lambda = *ref;
                    if (!lambda.transparent && lambda_param_offset != 0u) {
                        lambda.transparent = true;
                        lambda.definition = add_offset(ref.new_at(lambda.definition), lambda_param_offset);
                    }
                    if (lambda.definition.get_type() == MathType::lambda) {
                        const MutRef nested = ref.new_at(lambda.definition);
                        lambda.param_count += nested->lambda.param_count;
                        lambda.definition = nested->lambda.definition;
                        ref.store->free_one(nested.index);
                    }
                }
                if (options.recurse) {
                    const Lambda lambda = *ref;
                    const unsigned new_offset = lambda.transparent ?
                        lambda.param_count + lambda_param_offset :
                        lambda.param_count;
                    ref->lambda.definition = combine_(ref.new_at(lambda.definition), options, new_offset);
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

    //namespace combine    
} //namespace simp
