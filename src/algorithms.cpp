#include "algorithms.hpp"

#include <tuple>
#include <cfenv>

#include "utility/bit.hpp"
#include "utility/vector.hpp"
#include "utility/misc.hpp"


#include "termVector.hpp"




namespace simp {

    bool in_domain(const Complex& nr, const Domain domain)
    {
        constexpr double max_save_int = 9007199254740991; //== 2^53 - 1, largest integer explicitly stored in double

        const double re = nr.real();
        const double im = nr.imag();

        bool accept = true;
        switch (domain) {
        case Domain::natural:   accept &= re > 0.0; [[fallthrough]];
        case Domain::natural_0: accept &= re >= 0.0; [[fallthrough]];
        case Domain::integer:   accept &= re - std::int64_t(re) == 0.0;
            accept &= (std::abs(re) <= max_save_int); [[fallthrough]];
        case Domain::real:      accept &= im == 0.0; [[fallthrough]];
        case Domain::complex:
            return accept;

        case Domain::negative:      return re < 0.0 && im == 0.0;
        case Domain::positive:      return re > 0.0 && im == 0.0;
        case Domain::not_negative:  return re >= 0.0 && im == 0.0;
        case Domain::not_positive:  return re <= 0.0 && im == 0.0;
        default:
            assert(false);
            BMATH_UNREACHABLE;
        }
    } //in_domain

    bool meets_restriction(const UnsaveRef ref, const Restriction restr)
    {
        using namespace bmath::intern;
        if (restr.is<Unrestricted>()) {
            return true;
        }
        else if (restr.is<Type>()) {
            return ref.type == restr;
        }
        else if (restr.is<fn::Buildin>()) {
            if (ref.type != MathType::call) {
                return false;
            }
            const TypedIdx function = ref->call.function();
            return fn::to_typed_idx(restr.to<fn::Buildin>()) == function;
        }
        else {
            assert(restr.is<Domain>());
            if (ref.type != MathType::complex) {
                return false;
            }
            return in_domain(*ref, restr.to<Domain>());
        }
    } //meets_restriction



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

        constexpr bool is_unary_function(const UnsaveRef ref) 
        { 
            if (ref.type == MathType::buildin) {
                const fn::Buildin f = fn::Buildin(ref.index);
                return f.is<fn::Variadic>() || fn::arity(f.to<fn::FixedArity>()) == 1u;
            }
            return ref.type == MathType::lambda;
        }

        Complex BMATH_FORCE_INLINE eval_unary_complex(fn::FixedArity f, const Complex param)
        {
            using namespace fn;
            assert(input_space(f) == MathType::complex && result_space(f) == MathType::complex);
            assert(arity(f) == 1u);
            if (param.imag() == 0.0) {
                const double real_param = param.real();
                switch (f) {
                case FixedArity::asinh: return std::asinh(real_param);
                case FixedArity::acosh: return (         real_param  >= 1.0 ? std::acosh(real_param) : std::acosh(param));
                case FixedArity::atanh: return (std::abs(real_param) <= 1.0 ? std::atanh(real_param) : std::atanh(param));
                case FixedArity::asin : return (std::abs(real_param) <= 1.0 ?  std::asin(real_param) :  std::asin(param));
                case FixedArity::acos : return (std::abs(real_param) <= 1.0 ?  std::acos(real_param) :  std::acos(param));
                case FixedArity::atan : return std::atan (real_param);
                case FixedArity::sinh : return std::sinh (real_param);
                case FixedArity::cosh : return std::cosh (real_param);
                case FixedArity::tanh : return std::tanh (real_param);
                case FixedArity::sqrt : return (         real_param  >= 0.0 ?  std::sqrt(real_param) :  std::sqrt(param));
                case FixedArity::exp  : return std::exp  (real_param);
                case FixedArity::sin  : return std::sin  (real_param);
                case FixedArity::cos  : return std::cos  (real_param);
                case FixedArity::tan  : return std::tan  (real_param);
                case FixedArity::abs  : return std::abs  (real_param);
                case FixedArity::arg  : return std::arg  (real_param);
                case FixedArity::ln   : return std::log  (real_param);
                case FixedArity::re   : return real_param;
                case FixedArity::im   : return 0.0;
                }
            }
            else {
                switch (f) {
                case FixedArity::asinh: return std::asinh(param);
                case FixedArity::acosh: return std::acosh(param);
                case FixedArity::atanh: return std::atanh(param);
                case FixedArity::asin : return std::asin (param);
                case FixedArity::acos : return std::acos (param);
                case FixedArity::atan : return std::atan (param);
                case FixedArity::sinh : return std::sinh (param);
                case FixedArity::cosh : return std::cosh (param);
                case FixedArity::tanh : return std::tanh (param);
                case FixedArity::sqrt : return std::sqrt (param);
                case FixedArity::exp  : return std::exp  (param);
                case FixedArity::sin  : return std::sin  (param);
                case FixedArity::cos  : return std::cos  (param);
                case FixedArity::tan  : return std::tan  (param);
                case FixedArity::abs  : return std::abs  (param);
                case FixedArity::arg  : return std::arg  (param);
                case FixedArity::ln   : return std::log  (param);
                case FixedArity::re   : return std::real (param);
                case FixedArity::im   : return std::imag (param);
                }
            }
            assert(false);
            return {};
        } //eval_unary_complex

        TypedIdx BMATH_FORCE_INLINE eval_buildin(const MutRef& ref, Options options, const TypedIdx function)
        {
            using namespace fn;
            using namespace bmath::intern;
            assert(function.get_type() == MathType::buildin);
            assert(ref.type == MathType::call);
            Call& call = *ref;
            assert(call.function() == function);
            const Buildin f = from_typed_idx(function);
            if (f.is<FixedArity>()) {
                if (const Restriction f_input = input_space(f); f_input != Unrestricted{}) {
                    for (const TypedIdx param : call.parameters()) {
                        if (!meets_restriction(ref.new_at(param), f_input)) {
                            return TypedIdx();
                        }
                    }
                }
                const FixedArity fixed_f = f.to<FixedArity>();
                const std::size_t f_arity = arity(fixed_f);
                if (f_arity + 1u != call.size()) [[unlikely]]
                    throw "wrong number of parameters in function call";

                const auto compute_and_replace = [&](auto compute) -> TypedIdx {
                    std::feclearexcept(FE_ALL_EXCEPT);
                    const Complex result = compute();
                    if (options.exact && std::fetestexcept(FE_ALL_EXCEPT)) { //operation failed to be exact
                        return TypedIdx();
                    }
                    //free all but last parameter of call
                    for (std::size_t i = 1; i < f_arity; i++) {
                        assert(call[i].get_type() == MathType::complex);
                        ref.store->free_one(call[i].get_index());
                    }
                    //store result in last parameter
                    const TypedIdx result_idx = call[f_arity];
                    assert(result_idx.get_type() == MathType::complex);
                    ref.store->at(result_idx.get_index()) = result;
                    Call::free(*ref.store, ref.index); //free call itself
                    return result_idx;
                };
                //parameters are now guaranteed to meet restriction given in fn::fixed_arity_table
                //also parameter cout is guaranteed to be correct
                if (f_arity == 1u && input_space(f) == MathType::complex && result_space(f) == MathType::complex) {
                    return compute_and_replace([&] { return eval_unary_complex(fixed_f, *ref.new_at(call[1u])); });
                }
                switch (fixed_f) {
                case FixedArity::id: {
                    const TypedIdx res = call[1];
                    Call::free(*ref.store, ref.index);
                    return res;
                } break;
                case FixedArity::pow: {
                    return compute_and_replace([&] {
                        const Complex& base = *ref.new_at(call[1]);
                        const Complex& expo = *ref.new_at(call[2]);
                        if (expo == 0.5) {
                            return (base.imag() == 0.0 && base.real() >= 0.0) ?
                                std::sqrt(base.real()) :
                                std::sqrt(base);
                        }
                        else if (in_domain(expo, Domain::natural_0)) {
                            const std::size_t nat_expo = expo.real();
                            return nat_pow(base, nat_expo);
                        }
                        return std::pow(base, expo);
                    });
                } break;
                case FixedArity::log: {
                    return compute_and_replace([&] {
                        const Complex& base = *ref.new_at(call[1]);
                        const Complex& argu = *ref.new_at(call[2]);
                        const Complex num = (argu.imag() == 0.0) ?
                            std::log(argu.real()) :
                            std::log(argu);
                        const Complex denom = (base.imag() == 0.0) ?
                            std::log(base.real()) :
                            std::log(base);
                        return num / denom;
                    });
                } break;
                case FixedArity::not_: {
                    const TypedIdx param = call[1u];
                    ref.store->free_one(ref.index);
                    return TypedIdx(!param.get_index(), MathType::boolean);
                } break;
                case FixedArity::fmap: {
                    const TypedIdx converter = call[1];
                    const TypedIdx convertee = call[2];
                    const MutRef converter_ref = ref.new_at(converter);
                    const MutRef convertee_ref = ref.new_at(convertee);
                    if (!is_unary_function(converter_ref) || convertee_ref.type != MathType::call) {
                        return TypedIdx();
                    }
                    const auto stop = end(convertee_ref);
                    auto iter = begin(convertee_ref);
                    std::size_t remaining_parameters = convertee_ref->call.size() - 1u;
                    if (remaining_parameters == 0u) [[unlikely]] {
                        static_assert(Call::min_capacity >= 3u, "assumes fmap call to only use one store slot");
                        ref.store->free_one(ref.index); //free call to fmap
                        free_tree(converter_ref);
                        return convertee;
                    }
                    options.recurse = false;
                    for (++iter; iter != stop; ++iter) {
                        static_assert(Call::min_capacity >= 2u, "assumes call to only use one store slot");
                        const std::size_t call_index = ref.store->allocate_one();
                        const TypedIdx converter_copy = --remaining_parameters ? 
                            copy_tree(converter_ref, *ref.store) : 
                            converter;
                        ref.store->at(call_index) = Call({ converter_copy, *iter });
                        const TypedIdx res = combine_(MutRef(*ref.store, call_index, MathType::call), 
                            options, 0);
                        *iter = res;
                    }
                    static_assert(Call::min_capacity >= 3u, "assumes fmap call to only use one store slot");
                    ref.store->free_one(ref.index); //free call to fmap
                    return combine_(convertee_ref, options, 0);
                } break;
                }
            } //end fixed arity
            else {
                assert(f.is<Variadic>());
                const auto eval_bool = [&](const TypedIdx neutral_value, const TypedIdx short_circuit_value) {
                    { //remove values not changing outcome (true for and, false for or)
                        const auto range = call.parameters();
                        const auto new_end = std::remove(range.begin(), range.end(), neutral_value);
                        const std::size_t new_param_count = new_end - range.begin();
                        if (!new_param_count) {
                            free_tree(ref);
                            return neutral_value;
                        }                    
                        call.size() = new_param_count + 1u; //+1 for function itself
                    }
                    { //evaluate if possible
                        const auto range = call.parameters();
                        const auto first_short_circuit = std::find(range.begin(), range.end(), short_circuit_value);
                        if (first_short_circuit != range.end()) {
                            free_tree(ref);
                            return short_circuit_value;
                        }
                        return TypedIdx();
                    }
                };
                const auto maybe_accumulate = [&](Complex& acc, const Complex& new_, auto operation) -> bool {
                    std::feclearexcept(FE_ALL_EXCEPT);
                    const Complex result = operation(acc, new_);
                    if (options.exact && std::fetestexcept(FE_ALL_EXCEPT)) { //operation failed to be exact
                        return false;
                    }
                    //operation successfull -> store result in acc
                    acc = result;
                    return true;
                };
                switch (f.to<Variadic>()) {
                case Variadic(Comm::and_): 
                    return eval_bool(TypedIdx(1, MathType::boolean), TypedIdx(0, MathType::boolean));
                case Variadic(Comm::or_):
                    return eval_bool(TypedIdx(0, MathType::boolean), TypedIdx(1, MathType::boolean));
                case Variadic(Comm::sum): {
                    auto range = call.parameters();
                    const TypedIdx* const stop = range.end();
                    TypedIdx* iter_1 = range.begin();
                    for (; iter_1 != stop; ++iter_1) {
                        if (iter_1->get_type() == MathType::complex) {
                            Complex& acc = *ref.new_at(*iter_1);
                            for (auto iter_2 = std::next(iter_1); iter_2 != stop; ++iter_2) {
                                if (iter_2->get_type() == MathType::complex) {
                                    Complex& summand = *ref.new_at(*iter_2);
                                    if (maybe_accumulate(acc, summand,
                                        [](const Complex& lhs, const Complex& rhs) { return lhs + rhs; })) 
                                    {
                                        *iter_2 = TypedIdx();
                                    }
                                }
                            }
                        }
                    }
                    const auto new_end = std::remove(range.begin(), range.end(), TypedIdx());
                    const std::size_t new_size = new_end - range.begin();
                    call.size() = new_size + 1u; //+1 for function info itself
                } break;
                case Variadic(Comm::product): {
                    //TODO
                } break;
                }
            } //end variadic
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
                        //if the function could be fully evaluated, the following steps can no longer be executed -> return
                        const TypedIdx res = eval_buildin(ref, options, function);
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
                        const auto compare = [&](const TypedIdx fst, const TypedIdx snd) {
                            return compare_tree(ref.new_at(fst), ref.new_at(snd)) == std::strong_ordering::less;
                        };
                        auto range = ref->call.parameters();
                        std::sort(range.begin(), range.end(), compare);
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

    //remove if c++20 catches up
    constexpr std::strong_ordering operator<=>(std::string_view fst, std::string_view snd)
    {
        if (std::operator<(fst, snd)) { return std::strong_ordering::less; }
        if (std::operator>(fst, snd)) { return std::strong_ordering::greater; }
        return std::strong_ordering::equal;
    } //makeshift operator<=>

    std::strong_ordering compare_tree(const UnsaveRef fst, const UnsaveRef snd)
    {
        if (fst.type != snd.type) {
            return fst.type <=> snd.type;
        }
        switch (fst.type) {
        case Type(MathType::complex): 
            return bmath::intern::compare_complex(*fst, *snd);
        case Type(MathType::symbol):
            return std::string_view(fst->symbol) <=> std::string_view(snd->symbol);
        case Type(MathType::call): {
            const auto fst_end = fst->call.end();
            const auto snd_end = snd->call.end();
            auto fst_iter = fst->call.begin();
            auto snd_iter = snd->call.begin();
            for (; fst_iter != fst_end && snd_iter != snd_end; ++fst_iter, ++snd_iter) {
                const std::strong_ordering cmp = 
                    compare_tree(fst.new_at(*fst_iter), snd.new_at(*snd_iter));
                if (cmp != std::strong_ordering::equal) {
                    return cmp;
                }
            }
            if (fst_iter != fst_end || snd_iter != snd_end) {
                return fst->call.size() <=> snd->call.size();
            }
            return std::strong_ordering::equal;
        } break;
        case Type(MathType::lambda): {
            const Lambda fst_lambda = *fst;
            const Lambda snd_lambda = *snd;
            if (fst_lambda.param_count != snd_lambda.param_count) {
                return fst_lambda.param_count <=> snd_lambda.param_count;
            }
            if (fst_lambda.transparent != snd_lambda.transparent) {
                return fst_lambda.transparent <=> snd_lambda.transparent;
            }
            return compare_tree(fst.new_at(fst_lambda.definition), snd.new_at(snd_lambda.definition));
        } break;
        case Type(SingleMatch::restricted): 
            return fst->single_match.match_data_index <=> snd->single_match.match_data_index;
        case Type(ValueMatch::strong):
            return fst->value_match.match_data_index <=> snd->value_match.match_data_index;
        default:
            assert(!is_node(fst.type));
            return fst.index <=> snd.index;
        }
    } //compare_tree
 
} //namespace simp
