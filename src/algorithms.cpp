#include "algorithms.hpp"

#include <tuple>
#include <cfenv>
#include <iostream>
#include <optional>
#include <algorithm>
#include <functional>
#include <bitset>

#include "utility/bit.hpp"
#include "utility/vector.hpp"
#include "utility/misc.hpp"


#include "termVector.hpp"
#include "io.hpp"
#include "rewrite.hpp"
#include "control.hpp"




namespace simp {

    bool in_complex_subset(const Complex& nr, const nv::ComplexSubset subset)
    {
        using namespace nv;
        constexpr double max_save_int = bmath::intern::nat_pow(2ull, 53) - 1; //largest integer explicitly stored in double

        const double re = nr.real();
        const double im = nr.imag();

        bool accept = true;
        switch (subset) {
        case ComplexSubset::natural:   accept &= re > 0.0;                       [[fallthrough]];
        case ComplexSubset::natural_0: accept &= re >= 0.0;                      [[fallthrough]];
        case ComplexSubset::integer:   accept &= re - std::int64_t(re) == 0.0;
                                       accept &= (std::abs(re) <= max_save_int); [[fallthrough]];
        case ComplexSubset::real:      accept &= im == 0.0;                      [[fallthrough]];
        case ComplexSubset::complex:
            return accept;

        case ComplexSubset::negative:      return re <  0.0 && im == 0.0;
        case ComplexSubset::positive:      return re >  0.0 && im == 0.0;
        case ComplexSubset::not_negative:  return re >= 0.0 && im == 0.0;
        case ComplexSubset::not_positive:  return re <= 0.0 && im == 0.0;
        default:
            assert(false);
            BMATH_UNREACHABLE;
        }
    } //in_complex_subset

    bool meets_restriction(const UnsaveRef ref, const nv::Native restr)
    {
        using namespace nv;
        if (restr.is<Restr>()) {
            switch (restr) {
            case Native(Restr::any):
                return true;
            case Native(Restr::boolean):
                return ref.type == Literal::symbol && Symbol(ref.index).is<Bool>();
            case Native(Restr::applicable):
                return bmath::intern::is_one_of<Literal::lambda, Literal::symbol>(ref.type);
            case Native(Restr::no_value):
                return ref.type != Literal::complex;
            case Native(Restr::not_neg_1):
                return ref.type != Literal::complex || ref->complex != 1.0;
            case Native(Restr::not_0):
                return ref.type != Literal::complex || ref->complex != 0.0;
            default:
                assert(false);
                BMATH_UNREACHABLE;
            }
        }
        else if (restr.is<NodeType>()) {
            return ref.type == restr;
        }
        else if (restr.is<Function_>()) {
            if (ref.type != Literal::f_app && ref.type != PatternFApp{}) {
                return false;
            }
            const NodeIndex function = ref->f_app.function();
            return from_native(restr) == function;
        }
        else {
            assert(restr.is<ComplexSubset>());
            if (ref.type != Literal::complex) {
                return false;
            }
            return in_complex_subset(*ref, restr.to<ComplexSubset>());
        }
    } //meets_restriction



    namespace normalize {

        [[nodiscard]] NodeIndex replace_lambda_params(const MutRef ref, const bmath::intern::StupidBufferVector<NodeIndex, 16>& params
            , const int max_depth, const bool transparent)
        {
            assert(ref.type != PatternFApp{});
            if (ref.type == Literal::f_app && max_depth > 0) {
                bmath::intern::StupidBufferVector<NodeIndex, 16> res_application;
                const auto stop = end(ref);
                for (auto iter = begin(ref); iter != stop; ++iter) {
                    res_application.push_back(replace_lambda_params(ref.at(*iter), params, max_depth - 1, transparent));
                }
                //TODO: maybe only build new if something changed
                const std::size_t res_index = FApp::build(*ref.store, res_application);
                return NodeIndex(res_index, Literal::f_app);
            }
            else if (ref.type == Literal::lambda && ref->lambda.transparent) {
                Lambda res_lambda = *ref;
                res_lambda.transparent = transparent;
                res_lambda.definition = replace_lambda_params(ref.at(res_lambda.definition), params, max_depth - 1, true);

                const std::size_t res_index = ref.store->allocate_one();
                ref.store->at(res_index) = res_lambda;
                return NodeIndex(res_index, Literal::lambda);
            }
            else if (ref.type == Literal::lambda_param) { 
                if (ref.index < params.size()) {
                    return share(*ref.store, params[ref.index]);
                }
                else {
                    assert(transparent);
                    return NodeIndex(ref.index - params.size(), Literal::lambda_param);
                }
            }
            return share(ref);
        } //replace_lambda_params

        [[nodiscard]] NodeIndex eval_lambda(const MutRef ref, const Options options)
        {
            assert(ref.type == Literal::f_app); //PatternFApp is not expected here!
            const FApp& f_app = *ref;
            assert(f_app.function().get_type() == Literal::lambda);
            const Lambda lambda = *ref.at(f_app.function());
            assert(!lambda.transparent);
            bmath::intern::StupidBufferVector<NodeIndex, 16> params;
            for (const NodeIndex param : f_app.parameters()) {
                params.push_back(param);
            }
            if (lambda.param_count != params.size()) [[unlikely]] {
                throw TypeError{ "wrong number of arguments for applied lambda", ref };
            }
            const NodeIndex replaced = replace_lambda_params(ref.at(lambda.definition), params, lambda.owned_depth, false);
            free_tree(ref);
            return normalize::recursive(ref.at(replaced), options);
        } //eval_lambda

        bool BMATH_FORCE_INLINE merge_associative_apps(MutRef& ref, const NodeIndex function)
        {
            assert(ref.type == Literal::f_app && //PatternFApp is not wanted here!
                function.get_type() == Literal::symbol &&
                to_symbol(function).is<nv::Variadic>());
            bool found_nested = false;
            const FApp& f_app = *ref;
            bmath::intern::StupidBufferVector<NodeIndex, 16> merged_apps = { function };
            for (const NodeIndex param : f_app.parameters()) {
                if (param.get_type() == Literal::f_app) {
                    const MutRef param_ref = ref.at(param);
                    if (param_ref->f_app.function() == function) {
                        found_nested = true;
                        for (const NodeIndex param_param : param_ref->f_app.parameters()) {
                            merged_apps.push_back(param_param);
                        }
                        free_app_shallow(param_ref);
                        continue;
                    }
                }
                merged_apps.push_back(param);
            }
            if (found_nested) {
                if (f_app.capacity() >= merged_apps.size()) {
                    FApp::emplace(*ref, merged_apps, f_app.capacity());
                }
                else {
                    free_app_shallow(ref); //only free old application, not its parameters
                    ref.index = FApp::build(*ref.store, merged_apps);
                }
            }
            return found_nested;
        } //merge_associative_apps

        constexpr bool is_unary_function(const UnsaveRef ref) 
        { 
            if (ref.type == Literal::symbol) {
                const Symbol f = Symbol(ref.index);
                return f.is<nv::Variadic>() || f.is<nv::FixedArity>() && nv::arity(f.to<nv::FixedArity>()) == 1u;
            }
            return ref.type == Literal::lambda;
        }

        Complex eval_unary_complex(nv::CtoC f, const Complex param)
        {
            using namespace nv;
            if (param.imag() == 0.0) {
                const double real_param = param.real();
                switch (f) {
                case CtoC::asinh: return std::asinh(real_param);
                case CtoC::acosh: return (         real_param  >= 1.0 ? std::acosh(real_param) : std::acosh(param));
                case CtoC::atanh: return (std::abs(real_param) <= 1.0 ? std::atanh(real_param) : std::atanh(param));
                case CtoC::asin : return (std::abs(real_param) <= 1.0 ?  std::asin(real_param) :  std::asin(param));
                case CtoC::acos : return (std::abs(real_param) <= 1.0 ?  std::acos(real_param) :  std::acos(param));
                case CtoC::atan : return std::atan (real_param);
                case CtoC::sinh : return std::sinh (real_param);
                case CtoC::cosh : return std::cosh (real_param);
                case CtoC::tanh : return std::tanh (real_param);
                case CtoC::sqrt : return (         real_param  >= 0.0 ?  std::sqrt(real_param) :  std::sqrt(param));
                case CtoC::exp  : return std::exp  (real_param);
                case CtoC::sin  : return std::sin  (real_param);
                case CtoC::cos  : return std::cos  (real_param);
                case CtoC::tan  : return std::tan  (real_param);
                case CtoC::abs  : return std::abs  (real_param);
                case CtoC::arg  : return std::arg  (real_param);
                case CtoC::ln   : return std::log  (real_param);
                case CtoC::re   : return real_param;
                case CtoC::im   : return 0.0;
                case CtoC::conj : return real_param;
                case CtoC::floor: return std::floor(real_param);
                case CtoC::ceil : return std::ceil (real_param);
                }
            }
            else {
                switch (f) {
                case CtoC::asinh:  return std::asinh(param);
                case CtoC::acosh:  return std::acosh(param);
                case CtoC::atanh:  return std::atanh(param);
                case CtoC::asin :  return std::asin (param);
                case CtoC::acos :  return std::acos (param);
                case CtoC::atan :  return std::atan (param);
                case CtoC::sinh :  return std::sinh (param);
                case CtoC::cosh :  return std::cosh (param);
                case CtoC::tanh :  return std::tanh (param);
                case CtoC::sqrt :  return std::sqrt (param);
                case CtoC::exp  :  return std::exp  (param);
                case CtoC::sin  :  return std::sin  (param);
                case CtoC::cos  :  return std::cos  (param);
                case CtoC::tan  :  return std::tan  (param);
                case CtoC::abs  :  return std::abs  (param);
                case CtoC::arg  :  return std::arg  (param);
                case CtoC::ln   :  return std::log  (param);
                case CtoC::re   :  return std::real (param);
                case CtoC::im   :  return std::imag (param);
                case CtoC::conj :  return std::conj (param);
                case CtoC::floor:  return std::floor(param.real()); //not expected to happen
                case CtoC::ceil :  return std::ceil (param.real()); //not expected to happen
                }
            }
            assert(false);
            return {};
        } //eval_unary_complex

        Complex eval_pow(const Complex base, const Complex expo)
        {
            if (expo == 0.5) {
                return (base.imag() == 0.0 && base.real() >= 0.0) ?
                    std::sqrt(base.real()) :
                    std::sqrt(base);
            }
            else if (in_complex_subset(expo, nv::ComplexSubset::integer)) {
                const long long int_expo = expo.real();
                const Complex res = bmath::intern::nat_pow(base, std::abs(int_expo));
                return int_expo < 0 ? 1.0 / res : res;
            }
            return std::pow(base, expo);
        } //eval_pow

        Complex eval_binary_complex(nv::CtoC f, const Complex param1, const Complex param2) 
        {
            using namespace nv;
            switch (f) {
            case CtoC::divide: //param1 is numerator, param2 is denominator
                return param1 / param2;
            case CtoC::pow: //param1 is base, param2 is exponent
                return eval_pow(param1, param2);
            case CtoC::log: { //param1 is base, param2 is argument
                const Complex num = (param2.imag() == 0.0) ?
                    std::log(param2.real()) :
                    std::log(param2);
                const Complex denom = (param1.imag() == 0.0) ?
                    std::log(param1.real()) :
                    std::log(param1);
                return num / denom;
            }
            }
            assert(false);
            return {};
        } //eval_binary_complex

        NodeIndex BMATH_FORCE_INLINE eval_native(const MutRef ref, const Options options, const NodeIndex function)
        {
            using namespace nv;
            assert(function.get_type() == Literal::symbol);
            assert(ref.type == Literal::f_app); //PatternFApp is not expected here!
            assert(ref->f_app.function() == function);

            const Symbol f = to_symbol(function);
            assert(f.is<Native>());
            if (f.is<FixedArity>()) {
                const auto params = ref->f_app.parameters();
                const FixedArity fixed_f = f.to<FixedArity>();
                {
                    const FixedInputSpace param_spaces = input_space(fixed_f);
                    const std::size_t params_size = params.size();
                    for (std::size_t i = 0; i < params_size; i++) {
                        const nv::Native param_space = param_spaces[i];
                        if (param_space != Restr::any && !meets_restriction(ref.at(params[i]), param_space)) {
                            return invalid_index;
                        }
                    }
                }
                const std::size_t f_arity = arity(fixed_f);
                assert(f_arity > 0u); //that would not make sense in a purely functional laguage
                if (params.size() != f_arity) return invalid_index;

                const auto compute_and_replace = [&](auto compute) -> NodeIndex {
                    std::feclearexcept(FE_ALL_EXCEPT);
                    const Complex result = compute();
                    if (options.exact && std::fetestexcept(FE_ALL_EXCEPT)) { //operation failed to be exact
                        return invalid_index;
                    }
                    free_tree(ref); 
                    const std::size_t new_index = ref.store->allocate_one();
                    ref.store->at(new_index) = result;
                    return NodeIndex(new_index, Literal::complex);
                };
                const auto is_ordered_as = [&](const std::strong_ordering ord) {
                    assert(params[0].get_type() == Literal::complex && params[1].get_type() == Literal::complex);
                    const Complex& fst = *ref.at(params[0]);
                    const Complex& snd = *ref.at(params[1]);
                    const bool res = bmath::intern::compare_complex(fst, snd) == ord;
                    free_tree(ref);
                    return res;
                };
                const auto allow_haskell_app = [&](const NodeIndex applicable, const NodeIndex f_app) {
                    assert(f_app.get_type() == Literal::f_app);
                    return applicable == ref.at(f_app)->f_app.function(); //will only work on shallow values
                };
                //parameters are now guaranteed to meet restriction given in nv::fixed_arity_table
                //also parameter cout is guaranteed to be correct
                if (fixed_f.is<CtoC>()) {
                    if (f_arity == 1u) {
                        return compute_and_replace([&] { return eval_unary_complex(
                            fixed_f.to<CtoC>(), *ref.at(params[0u])); });
                    }
                    if (f_arity == 2u) {
                        return compute_and_replace([&] { return eval_binary_complex(
                            fixed_f.to<CtoC>(), *ref.at(params[0u]), *ref.at(params[1u])); });
                    }
                }
                switch (fixed_f) {
                case FixedArity(Bool::true_): {
                    const NodeIndex res = params[0];
                    free_tree(ref.at(params[1]));
                    free_app_shallow(ref);
                    return res;
                } break;
                case FixedArity(Bool::false_): {
                    const NodeIndex res = params[1];
                    free_tree(ref.at(params[0]));
                    free_app_shallow(ref);
                    return res;
                } break;
                case FixedArity(MiscFn::id): {
                    const NodeIndex res = params[0];
                    free_app_shallow(ref);
                    return res;
                } break;
                case FixedArity(ToBool::not_): {
                    const NodeIndex param = params[0];
                    if (param == literal_false) {
                        free_app_shallow(ref);
                        return literal_true;
                    }
                    if (param == literal_true) {
                        free_app_shallow(ref);
                        return literal_false;
                    }
                } break;
                case FixedArity(ToBool::eq):
                case FixedArity(ToBool::neq): if (options.eval_special) {
                    const std::strong_ordering ord = compare_tree(ref.at(params[0]), ref.at(params[1]));
                    free_tree(ref);
                    return bool_to_typed_idx((fixed_f == ToBool::eq) ^ (ord != std::strong_ordering::equal));
                } break;
                case FixedArity(ToBool::greater):
                    return bool_to_typed_idx(is_ordered_as(std::strong_ordering::greater));
                case FixedArity(ToBool::smaller):
                    return bool_to_typed_idx(is_ordered_as(std::strong_ordering::less));
                case FixedArity(ToBool::greater_eq):
                    return bool_to_typed_idx(!is_ordered_as(std::strong_ordering::less));
                case FixedArity(ToBool::smaller_eq):
                    return bool_to_typed_idx(!is_ordered_as(std::strong_ordering::greater));
                case FixedArity(HaskellFn::map): if (allow_haskell_app(params[0], params[2])) {
                    assert(!is_stored_node(params[0].get_type())); //first parameter does not need to be freed
                    const NodeIndex converter = params[1];
                    const MutRef converter_ref = ref.at(params[1]);
                    const MutRef convertee_ref = ref.at(params[2]);
                    assert(convertee_ref.type == Literal::f_app);

                    share_with_n(converter_ref, convertee_ref->f_app.parameters().size());
                    const auto stop = end(convertee_ref);
                    auto iter = begin(convertee_ref);
                    for (++iter; iter != stop; ++iter) {
                        static_assert(FApp::min_capacity >= 2u, "assumes application to only use one store slot");
                        const std::size_t app_index = ref.store->allocate_one();
                        ref.store->at(app_index) = FApp({ converter, *iter });
                        *iter = outermost(ref.at(NodeIndex(app_index, Literal::f_app)), options).res;
                    }
                    free_app_shallow(ref); //free app of map
                    free_tree(converter_ref);
                    return outermost(convertee_ref, options).res;
                } break; 
                }
            } //end fixed arity
            else {
                assert(f.is<Variadic>());
                auto params = ref->f_app.parameters(); //not const to allow to set params to new position if store reallocates

                //note: params last elements become invalid after calling this function
                const auto remove_shallow_value = [&](const NodeIndex to_remove) {
                    assert(ref->f_app.parameters().begin() == params.begin()); //make sure params are still valid
                    const auto new_end = std::remove(params.begin(), params.end(), to_remove);
                    const std::size_t new_param_count = new_end - params.begin();
                    ref->f_app.shrink_size_to(new_param_count + 1u); //+1 for function info itself
                    return new_param_count;
                };
                const auto eval_bool = [&](const NodeIndex neutral_value, const NodeIndex short_circuit_value) {
                    if (remove_shallow_value(neutral_value) == 0u) { //remove values not changing outcome (true for and, false for or)
                        free_app_shallow(ref); //zero size remaining -> no subterms
                        return neutral_value;
                    }                    
                    { //evaluate if possible
                        const auto first_short_circuit = std::find(params.begin(), params.end(), short_circuit_value);
                        if (first_short_circuit != params.end()) {
                            free_tree(ref);
                            return short_circuit_value;
                        }
                        return invalid_index;
                    }
                };
                const auto maybe_accumulate = [&](Complex& acc, const Complex& new_, const auto operation) {
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
                    return eval_bool(literal_true, literal_false);
                case Variadic(Comm::or_):
                    return eval_bool(literal_false, literal_true);
                case Variadic(Comm::sum): {
                    const auto end_complex = std::find_if(params.begin(), params.end(),
                        [](const NodeIndex i) { return i.get_type() != Literal::complex; });

                    if (auto iter = params.begin(); iter != end_complex) {
                        const MutRef fst = ref.at(*iter);
                        Complex acc = *fst;
                        std::size_t remaining = params.size() - 1u; //already subtract fst
                        while (++iter != end_complex) {
                            const MutRef current = ref.at(*iter);
                            if (maybe_accumulate(acc, *current, std::plus<Complex>())) {
                                free_node_shallow(*current.store, current.index);
                                *iter = invalid_index;
                                remaining--;
                            }
                        }

                        if (remaining == 0u) {
                            free_node_shallow(*fst.store, fst.index);
                            free_app_shallow(ref);
                            return parse::build_value(*ref.store, acc);
                        }
                        else if (acc == 0.0) [[unlikely]] {
                            free_node_shallow(*fst.store, fst.index);
                            params.front() = invalid_index;
                        }
                        else if (remaining != params.size() - 1u) {
                            free_node_shallow(*fst.store, fst.index);
                            const NodeIndex new_val = parse::build_value(*ref.store, acc); //could change stores data position 
                            params = ref->f_app.parameters(); //update params to (possibly) new position
                            params.front() = new_val;
                        }
                        remove_shallow_value(invalid_index);
                    }
                } break;
                case Variadic(Comm::prod): {
                    const auto end_complex = std::find_if(params.begin(), params.end(),
                        [](const NodeIndex i) { return i.get_type() != Literal::complex; });

                    if (auto iter = params.begin(); iter != end_complex) {
                        const MutRef fst = ref.at(*iter);
                        Complex acc = *fst;
                        std::size_t remaining = params.size() - 1u; //already subtract fst
                        while (++iter != end_complex) {
                            const MutRef current = ref.at(*iter);
                            if (maybe_accumulate(acc, *current, std::multiplies<Complex>())) {
                                free_node_shallow(*current.store, current.index);
                                *iter = invalid_index;
                                remaining--;
                            }
                        }
                        if (options.exact) { //evaluate exact division
                            const auto is_value_pow_app = [ref](const NodeIndex n) {
                                if (n.get_type() != Literal::f_app) return false;
                                const FApp& app = *ref.at(n);
                                return app[0] == from_native(nv::CtoC::pow) &&
                                    app[1].get_type() == Literal::complex &&
                                    app[2].get_type() == Literal::complex;
                            };
                            static_assert(shallow_order(UnsaveRef(nullptr, 0, Literal::complex)) < shallow_order(UnsaveRef(nullptr, 0, Literal::f_app)));
                            auto iter = std::find_if(end_complex, params.end(), is_value_pow_app);
                            const auto end_pow = std::find_if(iter, params.end(), [&](const NodeIndex n) { return !is_value_pow_app(n); });

                            for (; iter != end_pow; ++iter) {
                                const MutRef current = ref.at(*iter);
                                const FApp& pow = *current;
                                Complex base = *ref.at(pow[1]);
                                const Complex negative_expo = -ref.at(pow[2])->complex;
                                if (maybe_accumulate(base, negative_expo, normalize::eval_pow) &&
                                    maybe_accumulate(acc, base, std::divides<Complex>()))
                                {
                                    free_tree(current);
                                    *iter = invalid_index;
                                    remaining--;
                                }
                            }
                        }

                        if (remaining == 0u) {
                            free_node_shallow(*fst.store, fst.index);
                            free_app_shallow(ref);
                            return parse::build_value(*ref.store, acc);
                        }
                        else if (acc == 1.0) [[unlikely]] {
                            free_node_shallow(*fst.store, fst.index);
                            params.front() = invalid_index;
                        }
                        else if (remaining != params.size() - 1u) {
                            free_node_shallow(*fst.store, fst.index);
                            const NodeIndex new_val = parse::build_value(*ref.store, acc); //could change stores data position 
                            params = ref->f_app.parameters(); //update params to (possibly) new position
                            params.front() = new_val;
                        }
                        remove_shallow_value(invalid_index);
                    }
                } break;
                case Variadic(Comm::set): if (params.size() >= 2) { 
                    auto iter = params.begin();
                    UnsaveRef fst = ref.at(*iter);
                    for (++iter; iter != params.end(); ++iter) {
                        UnsaveRef snd = ref.at(*iter); //remove duplicates (remember: already sorted)
                        if (compare_tree(fst, snd) == std::strong_ordering::equal) {
                            free_tree(ref.at(*iter));
                            *iter = invalid_index;
                        }
                        fst = snd;
                    }
                    remove_shallow_value(invalid_index);
                } break;
                }
            } //end variadic
            return invalid_index;
        } //eval_native

        void BMATH_FORCE_INLINE sort(MutRef ref)
        {
            assert((ref.type == Literal::f_app || ref.type == PatternFApp{}) &&
                ref->f_app.function().get_type() == Literal::symbol && 
                to_symbol(ref->f_app.function()).is<nv::Comm>());
            auto range = ref->f_app.parameters();
            std::sort(range.begin(), range.end(), ordered_less(ref.store->data()));
        } //sort

        Result outermost(MutRef ref, const Options options)
        {
            assert(ref.type != PatternFApp{});
            if (ref.type == Literal::f_app) {  
                bool change = false;
                const NodeIndex function = ref->f_app.function();
                switch (function.get_type()) {
                case NodeType(Literal::symbol): {
                    const Symbol symbol = to_symbol(function);
                    const bool associative = symbol.is<nv::Variadic>() &&
                        nv::is_associative(symbol.to<nv::Variadic>());
                    if (associative) {
                        change |= merge_associative_apps(ref, function);
                    }
                    if (symbol.is<nv::Comm>()) {
                        sort(ref);
                    }
                    if (symbol.is<nv::Native>()) {
                        if (const NodeIndex res = eval_native(ref, options, function);
                            res != invalid_index)
                        {
                            //if the function could be fully evaluated, the following step(s) can no longer be executed -> return
                            return { res, true };
                        }
                    }
                    if (associative && options.remove_unary_assoc) {
                        const FApp& f_app = *ref;
                        if (f_app.size() == 2u) { //only contains function type and single parameter
                            const NodeIndex single_param = f_app[1u];
                            free_app_shallow(ref);
                            return { single_param, true };
                        }
                    }
                } break;
                case NodeType(Literal::lambda): {
                    assert(!ref.at(function)->lambda.transparent);
                    const NodeIndex evaluated = eval_lambda(ref, options);
                    return { evaluated, true };
                } break;
                }
                return { ref.typed_idx(), change };
            }
            return { ref.typed_idx(), false };
        } //outermost

        NodeIndex recursive(const MutRef ref, const Options options)
        {
            assert(ref.type != PatternFApp{});
            if (ref.type == Literal::f_app) {
                auto iter = begin(ref);
                *iter = normalize::recursive(ref.at(*iter), options);
                if (nv::is_lazy(*iter)) [[unlikely]] {
                    const NodeIndex result = normalize::outermost(ref, options).res;
                    return normalize::recursive(ref.at(result), options);
                }
                else {
                    for (++iter; !iter.at_end(); ++iter) {
                        *iter = normalize::recursive(ref.at(*iter), options);
                    }
                }
            }
            return normalize::outermost(ref, options).res;
        } //recursive

    } //namespace normalize

    void free_tree(const MutRef ref)
    {
        assert(ref.type != PatternFApp{});
        switch (ref.type) {
        case NodeType(Literal::complex):
        case NodeType(SpecialMatch::multi):
            if (ref.store->decr_at(ref.index) != 0) return;
            break;
        case NodeType(Literal::lambda):
            if (ref.store->decr_at(ref.index) != 0) return;
            free_tree(ref.at(ref->lambda.definition));
            break;
        case NodeType(Literal::f_app):
            if (ref.store->decr_at(ref.index) != 0) return;
            for (const NodeIndex subtree : ref) {
                free_tree(ref.at(subtree));
            }
            FApp::free(*ref.store, ref.index);
            return;
        case NodeType(SingleMatch::restricted):
            if (ref.store->decr_at(ref.index) != 0) return;
            free_tree(ref.at(ref->single_match.condition));
            break;
        case NodeType(SpecialMatch::value):
            if (ref.store->decr_at(ref.index) != 0) return;
            free_tree(ref.at(ref->value_match.inverse));
            break;
        default:
            assert(!is_stored_node(ref.type));
            return;
        }
        ref.store->free_one(ref.index);
    } //free_tree

    template<bmath::intern::Reference R, bmath::intern::StoreLike S>
    NodeIndex copy_tree(const R src_ref, S& dst_store)
    {
        const auto insert_node = [&](const TermNode n, NodeType type) {
            const std::size_t dst_index = dst_store.allocate_one();
            dst_store.at(dst_index) = n; 
            return NodeIndex(dst_index, type);            
        };

        switch (src_ref.type) {
        case NodeType(Literal::complex):
            return insert_node(*src_ref, src_ref.type);
        case NodeType(Literal::lambda): {
            Lambda lambda = *src_ref;
            lambda.definition = copy_tree(src_ref.at(lambda.definition), dst_store);
            return insert_node(lambda, src_ref.type);
        } break;
        case NodeType(Literal::f_app):
        case NodeType(PatternFApp{}): {
            bmath::intern::StupidBufferVector<NodeIndex, 16> dst_subterms;
            const auto stop = end(src_ref);
            for (auto iter = begin(src_ref); iter != stop; ++iter) {
                dst_subterms.push_back(copy_tree(src_ref.at(*iter), dst_store));
            }
            if (src_ref.type == PatternFApp{}) { //also copy variadic metadata
                const std::size_t app_capacity = FApp::smallest_fit_capacity(dst_subterms.size());
                const std::size_t nr_app_nodes = FApp::_node_count(app_capacity);

                const std::size_t dst_index = dst_store.allocate_n(1u + nr_app_nodes) + 1u;
                dst_store.at(dst_index - 1u) = f_app_info(src_ref);
                FApp::emplace(dst_store.at(dst_index), dst_subterms, app_capacity);
                return NodeIndex(dst_index, src_ref.type);
            }
            else {
                const std::size_t dst_index = FApp::build(dst_store, dst_subterms);
                return NodeIndex(dst_index, src_ref.type);
            }
        } break;
        case NodeType(SingleMatch::restricted): {
            RestrictedSingleMatch var = *src_ref;
            var.condition = copy_tree(src_ref.at(var.condition), dst_store);
            return insert_node(var, src_ref.type);
        } break;
        case NodeType(SpecialMatch::multi):
            return insert_node(*src_ref, src_ref.type);
        case NodeType(SpecialMatch::value): {
            ValueMatch var = *src_ref;
            var.inverse = copy_tree(src_ref.at(var.inverse), dst_store);
            return insert_node(var, src_ref.type);
        } break;
        default:
            assert(!is_stored_node(src_ref.type)); 
            return src_ref.typed_idx();
        }
    } //copy_tree
    template NodeIndex copy_tree(const Ref, Store&);
    template NodeIndex copy_tree(const Ref, MonotonicStore&);
    template NodeIndex copy_tree(const UnsaveRef, Store&);
    template NodeIndex copy_tree(const UnsaveRef, MonotonicStore&);

    std::uint32_t single_match_index(const UnsaveRef ref) 
    {
        assert(!is_stored_node(ref.type) || ref.type == SingleMatch::restricted);
        return ref.type == SingleMatch::restricted ?
            ref->single_match.match_state_index :
            ref.index;
    }

    std::strong_ordering compare_tree(const UnsaveRef fst, const UnsaveRef snd)
    {
        if (const int fst_order = shallow_order(fst), 
                      snd_order = shallow_order(snd); 
            fst_order != snd_order) 
        {
            return fst_order <=> snd_order;
        }
        switch (fst.type) {
        case NodeType(Literal::complex): 
            return bmath::intern::compare_complex(*fst, *snd);
        case NodeType(PatternFApp{}):
        case NodeType(Literal::f_app): {
            const auto fst_end = fst->f_app.end();
            const auto snd_end = snd->f_app.end();
            auto fst_iter = fst->f_app.begin();
            auto snd_iter = snd->f_app.begin();
            for (;;) {
                if (const auto cmp = compare_tree(fst.at(*fst_iter), snd.at(*snd_iter)); 
                    cmp != std::strong_ordering::equal) 
                {   return cmp;
                }
                ++fst_iter;
                ++snd_iter;
                if (fst_iter == fst_end) 
                    return snd_iter == snd_end ? 
                        std::strong_ordering::equal : 
                        std::strong_ordering::less;
                if (snd_iter == snd_end) 
                    return std::strong_ordering::greater;
            }
        } break;
        case NodeType(Literal::lambda): {
            const Lambda fst_lambda = *fst;
            const Lambda snd_lambda = *snd;
            if (fst_lambda.param_count != snd_lambda.param_count) {
                return fst_lambda.param_count <=> snd_lambda.param_count;
            }
            if (fst_lambda.transparent != snd_lambda.transparent) {
                return fst_lambda.transparent <=> snd_lambda.transparent;
            }
            return compare_tree(fst.at(fst_lambda.definition), snd.at(snd_lambda.definition));
        } break;
        case NodeType(SingleMatch::restricted): 
            return fst->single_match.match_state_index <=> single_match_index(snd);
        case NodeType(SpecialMatch::multi):
            return fst->multi_match.match_state_index <=> snd->multi_match.match_state_index;
        case NodeType(SpecialMatch::value):
            return fst->value_match.match_state_index <=> snd->value_match.match_state_index;
        default:
            assert(!is_stored_node(fst.type));
            return fst.index <=> single_match_index(snd);
        }
    } //compare_tree

    std::partial_ordering unsure_compare_tree(const UnsaveRef fst, const UnsaveRef snd)
    {
        const auto is_unordered = [](const NodeType t) { return t.is<SingleMatch>() || t == SpecialMatch::multi; };
        if (is_unordered(fst.type) || is_unordered(snd.type)) {
            return std::partial_ordering::unordered;
        }
        if (const int fst_order = shallow_order(fst),
                      snd_order = shallow_order(snd);
            fst_order != snd_order)
        {   return fst_order <=> snd_order;
        }
        switch (fst.type) {
        case NodeType(Literal::complex):
            return bmath::intern::compare_complex(*fst, *snd);
        case NodeType(PatternFApp{}):
        case NodeType(Literal::f_app): {
            auto fst_iter = fst->f_app.begin();
            auto snd_iter = snd->f_app.begin();     
            const NodeIndex fst_f = *fst_iter;
            const NodeIndex snd_f = *snd_iter;
            if (const auto cmp_fs = unsure_compare_tree(fst.at(fst_f), snd.at(snd_f)); 
                cmp_fs != std::partial_ordering::equivalent)
            {   return cmp_fs;
            }
            else if (fst_f.get_type() == Literal::symbol && to_symbol(fst_f).is<nv::Variadic>()) {
                return std::partial_ordering::unordered;
            }
            const auto fst_end = fst->f_app.end();
            const auto snd_end = snd->f_app.end();
            for (++fst_iter, ++snd_iter; fst_iter != fst_end && snd_iter != snd_end; ++fst_iter, ++snd_iter) {
                const std::partial_ordering cmp =
                    unsure_compare_tree(fst.at(*fst_iter), snd.at(*snd_iter));
                if (cmp != std::partial_ordering::equivalent) {
                    return cmp;
                }
            }
            if (fst_iter != fst_end || snd_iter != snd_end) {
                return fst->f_app.size() <=> snd->f_app.size();
            }
            return std::partial_ordering::equivalent;
        } break;
        case NodeType(Literal::lambda): {
            const Lambda fst_lambda = *fst;
            const Lambda snd_lambda = *snd;
            if (fst_lambda.param_count != snd_lambda.param_count) {
                return fst_lambda.param_count <=> snd_lambda.param_count;
            }
            if (fst_lambda.transparent != snd_lambda.transparent) {
                return fst_lambda.transparent <=> snd_lambda.transparent;
            }
            return unsure_compare_tree(fst.at(fst_lambda.definition), snd.at(snd_lambda.definition));
        } break;
        case NodeType(SpecialMatch::value):
            return std::partial_ordering::unordered;
        default:
            assert(!is_stored_node(fst.type) && !is_stored_node(snd.type));
            return fst.index <=> snd.index;
        }
    } //unsure_compare_tree

    int owned_lambda_depth(const UnsaveRef ref)
    {
        if (ref.type == Literal::f_app) {
            int sub_max = std::numeric_limits<int>::min();
            for (const NodeIndex sub : ref) {
                sub_max = std::max(sub_max, owned_lambda_depth(ref.at(sub)));
            }
            return sub_max + 1;
        }
        else if (ref.type == Literal::lambda && ref->lambda.transparent) {
            return owned_lambda_depth(ref.at(ref->lambda.definition)) + 1;
        }
        else if (ref.type == Literal::lambda_param) {
            return 1;
        }
        return std::numeric_limits<int>::min();
    } //find_lambda_depth


    namespace build_rule {

        RuleHead optimize_single_conditions(Store& store, RuleHead head)
        {
            const auto build_very_basic_pattern = [](Store& s, std::string& name) {
                RuleHead h = parse::raw_rule(s, name, parse::IAmInformedThisRuleIsNotUsableYet{});
                h = build_rule::prime_f_app(s, h);
                return h;
            };
            static const auto buildin_conditions = RuleSet({
                { "x :t            | x :'_SingleMatch', t :'symbol' = t" },
                { "x < 0           | x :'_SingleMatch'              = '_Negative'" },
                { "x > 0           | x :'_SingleMatch'              = '_Positive'" },
                { "x <= 0          | x :'_SingleMatch'              = '_NotPositive'" },
                { "x >= 0          | x :'_SingleMatch'              = '_NotNegative'" },
                { "!(x :'complex') | x :'_SingleMatch'              = '_NoValue'" },
                { "x != -1         | x :'_SingleMatch'              = '_NotNeg1'" },
                { "x != 0          | x :'_SingleMatch'              = '_Not0'" },
            }, build_very_basic_pattern);

            static const auto compute_optimisations = RuleSet({
                { "'_Divide'(a^(-1) * bs..., c)         = '_Divide'('prod'(bs...), a * c)" },
                { "'_Divide'(a ^  n * bs..., c) | n < 0 = '_Divide'('prod'(bs...), a ^ (-n) * c)" },
                { "        a ^ (-1) * bs...             = '_Divide'('prod'(bs...), a)" },
                { "        a ^ n    * bs...     | n < 0 = '_Divide'('prod'(bs...), a ^ (-n))" },
            }, build_very_basic_pattern);

            const auto optimize = [](const MutRef r) {
                if (r.type == SingleMatch::restricted) {
                    r->single_match.condition =
                        shallow_apply_ruleset(buildin_conditions, r.at(r->single_match.condition), { .eval_special = false });
                    r->single_match.condition =
                        greedy_apply_ruleset(compute_optimisations, r.at(r->single_match.condition), { .eval_special = false });
                }
                return r.typed_idx();
            };
            head.lhs = ctrl::transform(MutRef(store, head.lhs), optimize);
            return head;
        } //optimize_single_conditions

        RuleHead prime_value(Store& store, RuleHead heads)
        {
            const auto build_basic_pattern = [](Store& s, std::string& name) {
                RuleHead h = parse::raw_rule(s, name, parse::IAmInformedThisRuleIsNotUsableYet{});
                h = build_rule::optimize_single_conditions(s, h);
                h = build_rule::prime_f_app(s, h);
                return h;
            };
            static const auto bubble_up = RuleSet({
                { "       '_VM'(idx, domain, match) + a + cs... | a :'complex' = '_VM'(idx, domain, match - a) + cs..." },
                { "       '_VM'(idx, domain, match) * a * cs... | a :'complex' = '_VM'(idx, domain, match / a) * cs..." },
                { "       '_VM'(idx, domain, match) ^ 2                        = '_VM'(idx, domain, 'sqrt'(match))" },
                { "'sqrt'('_VM'(idx, domain, match))                           = '_VM'(idx, domain, match ^ 2)" },
                }, build_basic_pattern);
            heads.lhs = greedy_apply_ruleset(bubble_up, MutRef(store, heads.lhs), { .eval_special = false });

            static const auto to_domain = RuleSet({
                { "v :t   = t" },
                { "v < 0  = '_Negative'" },
                { "v > 0  = '_Positive'" },
                { "v <= 0 = '_NotPositive'" },
                { "v >= 0 = '_NotNegative'" },
                }, build_basic_pattern);
            const auto build_value_match = [](const MutRef r) {
                if (r.type == Literal::f_app && r->f_app.function() == from_native(nv::PatternFn::value_match)) {
                    FApp& f_app = *r;
                    assert(f_app[1].get_type().is<PatternUnsigned>());
                    const std::uint32_t match_state_index = f_app[1].get_index();
                    const NodeIndex inverse = std::exchange(f_app[3], literal_null);
                    const NodeIndex domain = shallow_apply_ruleset(to_domain, r.at(std::exchange(f_app[2], literal_null)), 
                        { .remove_unary_assoc = false, .eval_special = false });
                    if (domain.get_type() != Literal::symbol || !to_symbol(domain).is<nv::ComplexSubset>()) {
                        throw TypeError{ "value match restrictions may only be of form \"<value match> :<complex subset>\"", r };
                    }
                    free_tree(r);
                    const std::size_t result_index = r.store->allocate_one();
                    r.store->at(result_index) = ValueMatch{ .match_state_index = match_state_index,
                                                            .inverse = inverse,
                                                            .domain = to_symbol(domain).to<nv::ComplexSubset>(),
                                                            .owner = false };
                    return NodeIndex(result_index, SpecialMatch::value);
                }
                return r.typed_idx();
            };
            heads.lhs = ctrl::transform(MutRef(store, heads.lhs), build_value_match);
            heads.rhs = ctrl::transform(MutRef(store, heads.rhs), build_value_match);
            heads.lhs = normalize::recursive(MutRef(store, heads.lhs), { .eval_special = false }); //order value match to right positions in commutative

            std::bitset<match::State::max_value_match_count> encountered = 0;
            const auto set_owner = [&encountered](const MutRef r) {
                if (r.type == SpecialMatch::value) {
                    ValueMatch& var = *r;
                    if (!encountered.test(var.match_state_index)) {
                        encountered.set(var.match_state_index);
                        var.owner = true;
                    }
                }
            };
            ctrl::transform(MutRef(store, heads.lhs), set_owner);

            return heads;
        } //prime_value

        //turns every f_app in lhs into a PatternFApp, but nothing is changed from the default values
        //note: after this step, no more manipulation on the raw pattern via patterns are possible
        NodeIndex swap_lhs_f_apps(const MutRef lhs_ref)
        {
            const auto swap_app = [](const MutRef ref) {
                if (ref.type == Literal::f_app) {
                    bmath::intern::StupidBufferVector<NodeIndex, 16> subterms;
                    for (const NodeIndex sub : ref) {
                        subterms.push_back(sub);
                    }
                    free_app_shallow(ref);
                    const std::size_t app_capacity = FApp::smallest_fit_capacity(subterms.size());
                    const std::size_t nr_app_nodes = FApp::_node_count(app_capacity);

                    const std::size_t res_index = ref.store->allocate_n(1u + nr_app_nodes) + 1u;
                    ref.store->at(res_index - 1u) = FAppInfo{};
                    FApp::emplace(ref.store->at(res_index), subterms, app_capacity);
                    return NodeIndex(res_index, PatternFApp{});
                }
                return ref.typed_idx();
            };
            return ctrl::transform(lhs_ref, swap_app);
        } //swap_lhs_f_apps

        //to be rematchable one has to eighter contain a rematchable subterm or be 
        //a pattern f_app with at least one of the following properties:
        //   - in a non-commutative f_app at least two parameters are multi match variables 
        //     e.g. "tup(xs..., 1, 2, ys...)" but not "tup(xs..., 1, 2, tup(ys...))"
        //   - in a commutative f_app a multi match and one or more parameters containing an owned single match are held
        //     e.g. "a + bs..." or "2 a + bs..." but not "2 + bs..."
        //   - in a commutative f_app two or more parameters hold an owned single match and are relatively unordered
        //     e.g. "a^2 + b^2" or "a^2 + b" but not "a^2 + 2 b"
        //note: "relatively unordered" refers to the bits set by set_always_preceeding_next
        bool is_rematchable(const UnsaveRef ref)
        {
            const auto app_is_rematchable = [](const UnsaveRef r) {
                if (r.type == PatternFApp{}) {
                    const auto info = f_app_info(r);
                    switch (info.strategy) {
                    case MatchStrategy::permutation: {
                        const auto has_owned_single = [r](const NodeIndex n) {
                            return ctrl::search<true, PatternFApp{}>(r.store_data(), n, [](const UnsaveRef rr) {
                                return rr.type == SingleMatch::restricted || rr.type == SingleMatch::unrestricted;
                                }) != invalid_index;
                        };
                        const std::span<const NodeIndex> params = r->f_app.parameters();
                        if (info.preceeded_by_multi) {
                            return std::count_if(params.begin(), params.end(), has_owned_single) > 0;
                        }
                        std::array<bool, match::SharedFAppEntry::max_params_count> single_owners = {};
                        for (std::size_t i = 0; i < params.size(); i++) {
                            single_owners[i] = has_owned_single(params[i]);
                        }
                        for (std::size_t i = 0; i < params.size(); i++) {
                            if (!single_owners[i]) continue;
                            if (info.always_preceeding_next.test(i)) continue;
                            for (std::size_t k = i + 1u; k < params.size(); k++) {
                                if (single_owners[k]) return true;
                            }
                        }
                        return false;
                    } break;
                    case MatchStrategy::dilation:
                        return info.preceeded_by_multi.count() > 1;
                    case MatchStrategy::backtracking:
                        return true;
                    case MatchStrategy::linear:
                        return false;
                    }
                }
                return false;
            };
            return ctrl::search<true, PatternFApp{}>(ref.store_data(), ref.typed_idx(), app_is_rematchable) != invalid_index;
        } //is_rematchable

        using EquivalenceTable = std::array<std::array<bool, match::State::max_single_match_count>,
            match::State::max_single_match_count>;

        std::string eq_table_to_string(const EquivalenceTable& t) {
            std::size_t max_interesting = 0;
            for (std::size_t i = 0; i < t.size(); i++) {
                for (std::size_t k = 0; k < t.size(); k++) {
                    if (t[i][k] && i != k) 
                        max_interesting = std::max({ max_interesting, i + 1, k + 1 });
                }
            }
            if (max_interesting == 0) {
                return "";
            }
            std::string res;
            for (std::size_t i = 0; i < max_interesting; i++) {
                for (std::size_t k = 0; k < max_interesting; k++) {
                    res += (t[i][k] ? 'x' : ' ');
                    res += ' ';
                }
                res += "\n";
            }
            return res;
        } //eq_table_to_string

        //two single match variables are equivalent, if swapping instances in whole pattern out for another, 
        //  the same pattern results.
        //idea: copy whole pattern, swap pairs of single match variables, test if pattern is unchanged, then variables are equivalent 
        EquivalenceTable build_equivalence_table(const UnsaveRef lhs_ref)
        {
            constexpr std::size_t max_singles = match::State::max_single_match_count;
            std::array<bool, max_singles> occurs_unrestricted = {};
            const auto catalog_occurence = [&occurs_unrestricted](const UnsaveRef r) {
                if (r.type == SingleMatch::unrestricted) {
                    occurs_unrestricted.at(r.index) = true;
                }
            };
            ctrl::transform(lhs_ref, catalog_occurence);

            Store copy_store;
            NodeIndex head_copy = copy_tree(lhs_ref, copy_store);

            const auto swap_singles = [&](const std::size_t i, const std::size_t k) {
                const auto swap_single = [i, k](MutRef r) {
                    assert(r.type != PatternFApp{});
                    if (r.type.is<SingleMatch>()) {
                        const auto replace_with = [&r](const std::size_t new_idx) {
                            if (r.type == SingleMatch::restricted) r->single_match.match_state_index = new_idx;
                            else                                   r.index = new_idx;
                        };
                        const std::uint32_t current = single_match_index(r);
                        if (current == i) replace_with(k);
                        if (current == k) replace_with(i);
                    }
                    else if (r.type == Literal::f_app &&
                        r->f_app.function().get_type() == Literal::symbol &&
                        to_symbol(r->f_app.function()).is<nv::Comm>())
                    {
                        normalize::sort(r);
                    }
                    return r.typed_idx();
                };
                head_copy = ctrl::transform(MutRef(copy_store, head_copy), swap_single);
            }; //lambda swap_singles

            EquivalenceTable equivalence_table = {};
            for (std::size_t i = 0; i + 1u < max_singles; i++) {
                equivalence_table[i][i] = true; //every variable is equivalent to itself
                if (!occurs_unrestricted[i]) continue;
                for (std::size_t k = i + 1u; k < max_singles; k++) {
                    if (!occurs_unrestricted[k]) continue;
                    assert(compare_tree(lhs_ref, Ref(copy_store, head_copy)) == std::strong_ordering::equal);
                    swap_singles(i, k);
                    if (compare_tree(lhs_ref, Ref(copy_store, head_copy)) == std::strong_ordering::equal) {
                        equivalence_table[i][k] = true;
                        equivalence_table[k][i] = true;
                    }
                    swap_singles(i, k); //swap back to start fresh for next round
                }
            }
            return equivalence_table;
        } //build_equivalence_table

        //determines if fst and second are equivalent, if single match variables are considered equivalent as in eq
        bool equivalent(const UnsaveRef fst, const UnsaveRef snd, const EquivalenceTable& eq)
        {
            assert(fst.type != Literal::f_app && snd.type != Literal::f_app);
            if (fst.type == PatternFApp{} && snd.type == PatternFApp{}) {
                const FAppInfo fst_info = f_app_info(fst);
                const FAppInfo snd_info = f_app_info(snd);
                if (fst_info.preceeded_by_multi != snd_info.preceeded_by_multi)
                {   return false;
                }
                const auto fst_end = fst->f_app.end();
                const auto snd_end = snd->f_app.end();
                auto fst_iter = fst->f_app.begin();
                auto snd_iter = snd->f_app.begin();
                for (; fst_iter != fst_end && snd_iter != snd_end; ++fst_iter, ++snd_iter) {
                    if (!equivalent(fst.at(*fst_iter), snd.at(*snd_iter), eq)) {
                        return false;
                    }
                }
                return fst_iter == fst_end && snd_iter == snd_end;
            }
            else if (fst.type == Literal::lambda && snd.type == Literal::lambda) {
                const Lambda fst_lambda = *fst;
                const Lambda snd_lambda = *snd;
                return fst_lambda.param_count == snd_lambda.param_count &&
                    fst_lambda.transparent == snd_lambda.transparent &&
                    equivalent(fst.at(fst_lambda.definition), snd.at(snd_lambda.definition), eq);
            }
            else if (fst.type.is<SingleMatch>() && snd.type.is<SingleMatch>()) {
                return eq[single_match_index(fst)][single_match_index(snd)];
            }
            return compare_tree(fst, snd) == std::strong_ordering::equal;
        } //equivalent

        void shallow_set_always_preceeding_next(const MutRef r, const EquivalenceTable& eq)
        {
            if (r.type == PatternFApp{} && f_app_info(r).strategy == MatchStrategy::permutation) {
                auto& bits = f_app_info(r).always_preceeding_next;
                const auto params = r->f_app.parameters();
                for (std::size_t i = 0; i + 1 < params.size(); i++) {
                    const auto i_ref = r.at(params[i]);
                    const auto next_ref = r.at(params[i + 1]);
                    bool i_always_preceeding_next =
                        unsure_compare_tree(i_ref, next_ref) == std::partial_ordering::less ||
                        equivalent(i_ref, next_ref, eq);
                    bits.set(i, i_always_preceeding_next);
                }
            }
        } //shallow_set_always_preceeding_next

        struct MultiChange
        {
            std::uint32_t identifier; //original creation index
            MultiMatch new_;
        };

        //changes f_app to pattern f_app if appropriate and change multis to multi_marker
        [[nodiscard]] NodeIndex build_lhs_multis_and_pattern_f_apps(MutRef ref, std::vector<MultiChange>& multi_changes, 
            unsigned& pattern_app_index, const EquivalenceTable& eq) 
        {
            assert(ref.type != Literal::f_app && "run swap_lhs_f_apps bevor running this function");
            if (ref.type == PatternFApp{}) {
                //build subterms
                const auto stop = end(ref);
                for (auto iter = begin(ref); iter != stop; ++iter) {
                    *iter = build_lhs_multis_and_pattern_f_apps(
                        ref.at(*iter), multi_changes, pattern_app_index, eq);
                }
                //build current
                const std::size_t this_multi_count = std::count_if(ref->f_app.begin(), ref->f_app.end(),
                    [](const NodeIndex i) { return i.get_type() == SpecialMatch::multi; });

                auto& info = f_app_info(ref);

                const auto [prime, commuts] = [&] {
                    const NodeIndex f = ref->f_app.function();
                    if (f.get_type() == Literal::symbol) {
                        using namespace nv;
                        const Symbol symbol = to_symbol(f);

                        if (symbol.is<Variadic>() && is_associative(symbol.to<Variadic>())) {
                            info.associative = true;
                        }

                        if (symbol.is<Comm>()) {
                            if (this_multi_count > 1) 
                                throw TypeError{ "too many multis in commutative", ref };
                            info.strategy = MatchStrategy::permutation;
                            return std::tuple{ true, true };
                        }
                        if (symbol.is<FixedArity>() && this_multi_count > 0)
                            throw TypeError{ "multi match illegal in fixed arity", ref };
                    }

                    if (this_multi_count > 0u) {
                        info.strategy = MatchStrategy::dilation;
                        return std::tuple{ true, false };
                    }
                    else if (is_rematchable(ref)) {
                        info.strategy = MatchStrategy::backtracking;
                    }
                    else {//required as default, as otherwise is_rematchable above will always return true
                        assert(info.strategy == MatchStrategy::linear);
                    }
                    return std::tuple{ false, false };
                } ();
                //relies on the lambda above to set the strategy
                shallow_set_always_preceeding_next(ref, eq);

                { //set rematchable_params
                    const std::span<NodeIndex> params = ref->f_app.parameters();
                    info.rematchable_params = 0;
                    for (std::size_t i = 0; i < params.size(); i++) {
                        info.rematchable_params.set(i, is_rematchable(ref.at(params[i])));
                    }
                    assert(info.strategy != MatchStrategy::linear || info.rematchable_params.none());
                    assert(info.strategy != MatchStrategy::backtracking || info.rematchable_params.any());
                }
                if (prime) { //give current own match data index and change multis to final form
                    const unsigned this_pattern_index = pattern_app_index++;
                    info.match_state_index = this_pattern_index;
                    std::uint32_t pos_mod_multis = 0; //only is incremented, when no multi is encountered
                    FApp& f_app = *ref;
                    for (NodeIndex& param : f_app.parameters()) {
                        if (param.get_type() == SpecialMatch::multi) {
                            const unsigned identifier = ref.at(param)->multi_match.match_state_index;
                            multi_changes.emplace_back(identifier,
                                MultiMatch{ this_pattern_index, commuts ? -1u : pos_mod_multis });
                            free_tree(ref.at(param));
                            param = invalid_index;
                            if (info.preceeded_by_multi.test(pos_mod_multis)) {
                                throw TypeError{ "two multis in direct succession are illegal in lhs", ref };
                            }
                            info.preceeded_by_multi.set(commuts ? 0 : pos_mod_multis);
                        }
                        else {
                            pos_mod_multis++;
                        }
                    }
                    const auto new_end = std::remove(f_app.begin(), f_app.end(), invalid_index);
                    f_app.shrink_size_to(new_end - f_app.begin());
                }
            }
            else if (ref.type == Literal::lambda) {
                ref->lambda.definition = build_lhs_multis_and_pattern_f_apps(
                    ref.at(ref->lambda.definition), multi_changes, pattern_app_index, eq);
            }
            return ref.typed_idx();
        } //build_lhs_multis_and_pattern_f_apps

        RuleHead prime_f_app(Store& store, RuleHead head)
        {
            const EquivalenceTable eq_table = build_equivalence_table(Ref(store, head.lhs));

            head.lhs = build_rule::swap_lhs_f_apps(MutRef(store, head.lhs));

            std::vector<MultiChange> multi_changes;
            unsigned pattern_app_count = 0; //keeps track of how many conversions to PatternFApp using State have already been made 

            head.lhs = build_lhs_multis_and_pattern_f_apps(
                MutRef(store, head.lhs), multi_changes, pattern_app_count, eq_table);
            if (pattern_app_count > match::State::max_pattern_f_app_count) {
                throw TypeError{ "too many pattern applications using State", Ref(store, head.lhs) };
            }

            const auto prime_rhs = [&multi_changes](const MutRef r) {
                if (r.type == SpecialMatch::multi) {
                    const unsigned identifier = r->multi_match.match_state_index;
                    const auto data = std::find_if(multi_changes.begin(), multi_changes.end(),
                        [identifier](const MultiChange& c) { return c.identifier == identifier; });
                    assert(data != multi_changes.end());
                    r->multi_match = data->new_;
                }
            };
            ctrl::transform(MutRef(store, head.rhs), prime_rhs);
            return head;
        } //prime_f_app

        RuleHead add_implicit_multis(Store& store, RuleHead head)
        {
            const MutRef lhs_ref = MutRef(store, head.lhs);
            if (lhs_ref.type != PatternFApp{})
                return head;
            if (!lhs_ref->f_app.parameters().size())
                return head;
            const NodeIndex f = lhs_ref->f_app.function();
            if (f.get_type() != Literal::symbol)
                return head;
            const Symbol f_native = to_symbol(f);
            if (!f_native.is<nv::Variadic>()) 
                return head;
            const nv::Variadic f_variadic = f_native.to<nv::Variadic>();
            if (!nv::is_associative(f_variadic))
                return head;

            auto& info = f_app_info(lhs_ref);
            if (f_variadic.is<nv::Comm>() && !info.preceeded_by_multi) {
                info.preceeded_by_multi = 1;
                const auto multi_parent = info.match_state_index;
                const std::size_t rhs_multi_index = store.allocate_one();
                store.at(rhs_multi_index) = MultiMatch{ .match_state_index = multi_parent, .index_in_params = -1u };
                const std::size_t temp_head_rhs_index = 
                    FApp::build(store, std::to_array({ f, head.rhs, NodeIndex(rhs_multi_index, SpecialMatch::multi) }));
                head.rhs = normalize::outermost(
                    MutRef(store, temp_head_rhs_index, Literal::f_app), 
                    { .remove_unary_assoc = false }).res;
                return head;
            }
            if (f_variadic.is<nv::NonComm>()) {
                assert(false); //TODO
            }
            return head;
        } //add_implicit_multis

        RuleHead build_everything(Store& store, std::string& name)
        {
            RuleHead head = parse::raw_rule(store, name, parse::IAmInformedThisRuleIsNotUsableYet{});
            head = build_rule::optimize_single_conditions(store, head);
            head = build_rule::prime_value(store, head);
            head = build_rule::prime_f_app(store, head);
            head = build_rule::add_implicit_multis(store, head);

            return head;
        } //build_everything

    } //namespace build_rule


    namespace match {

        template<bmath::intern::Callable<NodeIndex> F>
        void for_each_multi(const MultiMatch multi, const match::State& state, F f)
        {
            const SharedFAppEntry& entry = state.f_app_entries[multi.match_state_index];
            const Ref matched_ref = Ref(*state.haystack, entry.match_idx);
            assert(matched_ref.type == Literal::f_app);

            if (multi.index_in_params == -1u) { //commutative -> multi might not be one pice
                auto iter = begin(matched_ref); //currently pointing at function
                std::uint32_t i = 0;
                for (++iter; !iter.at_end(); ++iter) {
                    if (!entry.index_matched(i++)) {
                        f(*iter);
                    }
                }
            }
            else { //only part of matched_ref's unmatched parameters belongs to multi (but part is continuous)
                const std::uint32_t begin_params_index =
                    multi.index_in_params > 0 ?
                    //+1u because we only want the thingies after the match listed here
                    entry.match_positions[multi.index_in_params - 1u] + 1u :
                    0u;
                const std::uint32_t end_params_index =
                    entry.match_positions[multi.index_in_params] != SharedFAppEntry::MatchPos_T(-1) ?
                    entry.match_positions[multi.index_in_params] :
                    matched_ref->f_app.parameters().size();

                //+ 1u in both cases, because the function itself resides at index 0
                auto iter = decltype(begin(std::declval<Ref>()))
                    ::build(*matched_ref.store, matched_ref.index, begin_params_index + 1u, end_params_index + 1u);

                for (; !iter.at_end(); ++iter) {
                    f(*iter);
                }
            }
        } //for_each_multi

        auto make_ref_maker(const State& match_state, const TermNode* const store_data)
        {
            return [store_data, &match_state](const NodeIndex i) -> UnsaveRef {
                const NodeType ref_type = i.get_type();
                const std::uint32_t ref_index = i.get_index();
                if (ref_type == SingleMatch::weak && store_data != match_state.haystack->data()) {
                    const auto entry = match_state.single_vars[ref_index];
                    assert(entry.is_set());
                    return match_state.make_ref(entry.match_idx);
                }
                return UnsaveRef(store_data, ref_index, ref_type);
            };
        } //make_ref_maker

        using RefMaker = decltype(make_ref_maker(std::declval<State>(), nullptr));

        bmath::intern::OptionalDouble eval_condition(const UnsaveRef cond, const State& match_state)
        {
            if (cond.type == Literal::complex) {
                return cond->complex.imag() == 0.0 ?
                    cond->complex.real() :
                    bmath::intern::OptionalDouble{};
            }

            if (cond.type != Literal::f_app) return {};
            const FApp& f_app = *cond;
            const NodeIndex function = f_app.function();
            if (function.get_type() != Literal::symbol) return {};
            using namespace nv;
            const Symbol f = to_symbol(function);
            const auto params = f_app.parameters();
            const auto make_ref = make_ref_maker(match_state, cond.store_data());

            if (f == Comm::sum) {
                bmath::intern::OptionalDouble acc = 0.0;
                for (const NodeIndex param : params) {
                    acc += eval_condition(make_ref(param), match_state);
                    if (!acc) break;
                }
                return acc;
            }
            else if (f == Comm::prod) {
                bmath::intern::OptionalDouble acc = 1.0;
                for (const NodeIndex param : params) {
                    acc *= eval_condition(make_ref(param), match_state);
                    if (!acc) break;
                }
                return acc;
            }
            else if (f.is<CtoC>()) {
                const auto arr = arity(f.to<FixedArity>());
                if (arr == 1u) {
                    const auto param = eval_condition(make_ref(params[0]), match_state);
                    if (!param) return {};
                    const Complex res = normalize::eval_unary_complex(f.to<CtoC>(), param.val);
                    return res.imag() == 0.0 ? res.real() : bmath::intern::OptionalDouble{};
                }
                if (arr == 2u) {
                    const auto param1 = eval_condition(make_ref(params[0]), match_state);
                    if (!param1) return {};
                    const auto param2 = eval_condition(make_ref(params[1]), match_state);
                    if (!param2) return {};
                    const Complex res = normalize::eval_binary_complex(f.to<CtoC>(), param1.val, param2.val);
                    return res.imag() == 0.0 ? res.real() : bmath::intern::OptionalDouble{};
                }
            }
            return {};
        } //eval_condition

        //decides if a condition appended to a single match variable is met with the current match data
        bool test_condition(const UnsaveRef cond, const State& match_state)
        {
            const auto make_ref = make_ref_maker(match_state, cond.store_data());
            assert(cond.type == Literal::f_app);
            const FApp& f_app = *cond;
            using namespace nv;
            const Symbol f = to_symbol(f_app.function());
            const auto params = f_app.parameters();

            const auto compare_params = [&]() {
                const bmath::intern::OptionalDouble fst = eval_condition(make_ref(params[0]), match_state);
                const bmath::intern::OptionalDouble snd = eval_condition(make_ref(params[1]), match_state);
                return fst.val <=> snd.val;
            };

            switch (f) {
            case Symbol(PatternFn::of_type): {
                const Symbol restr = to_symbol(params[1]);
                if (restr.is<ComplexSubset>()) {
                    const double to_test = eval_condition(make_ref(params[0]), match_state).val;
                    return in_complex_subset(to_test, restr.to<ComplexSubset>());
                }
                assert(restr.is<Native>());
                return meets_restriction(make_ref(params[0]), restr.to<Native>());
            }
            case Symbol(ToBool::contains): {
                const UnsaveRef fst_ref = make_ref(params[0]);
                const UnsaveRef snd_ref = make_ref(params[1]);
                const auto is_snd = [&](const UnsaveRef r) { 
                    return compare_tree(snd_ref, r) == std::strong_ordering::equal;
                };
                return ctrl::search<true>(fst_ref.store_data(), fst_ref.typed_idx(), is_snd) != invalid_index;
            } break;
            case Symbol(ToBool::not_):
                return !test_condition(cond.at(params[0]), match_state);
            case Symbol(ToBool::eq):
                return compare_params() == 0;
            case Symbol(ToBool::neq): {
                const std::partial_ordering res = compare_params();
                return res != std::partial_ordering::equivalent && res != std::partial_ordering::unordered;
            } break;
            case Symbol(ToBool::greater):
                return compare_params() > 0;
            case Symbol(ToBool::smaller):
                return compare_params() < 0;
            case Symbol(ToBool::greater_eq):
                return compare_params() >= 0;
            case Symbol(ToBool::smaller_eq):
                return compare_params() <= 0;
            case Symbol(Comm::and_):
                for (const NodeIndex param : params) {
                    if (!test_condition(cond.at(param), match_state)) return false;
                }
                return true;
            case Symbol(Comm::or_):
                for (const NodeIndex param : params) {
                    if (test_condition(cond.at(param), match_state)) return true;
                }
                return false;
            }
            assert(false);
            BMATH_UNREACHABLE;
            return false;
        } //test_condition

        //(transiently) evaluates .inverse of ValueMatch for a given start_val
        bmath::intern::OptionalComplex eval_value_match(const UnsaveRef ref, const Complex& start_val)
        {
            return bmath::intern::OptionalComplex();
        } //eval_value_match

        //determines weather there is a way to match needle_ref in hay_ref (thus needle_ref is assumed to part of a pattern)
        //needle_i is the index of the first element in needle_ref to be matched. 
        //if needle_i is not zero, it is assumed, that all previous elements in needle_ref are already matched (=> the call happens in a rematch).
        //the first haystack_k elements of hay_ref will be skipped for the first match attemt.
        //it is assumed, that needle_ref and hay_ref are both applications of the same nv::Comm, where pn_ref is a PatternFApp
        //returns true if a match was found
        bool find_permutation(const UnsaveRef pn_ref, const UnsaveRef hay_ref, State& match_state, std::int32_t needle_i, std::int32_t hay_k)
        {
            assert(pn_ref.type == PatternFApp{} && hay_ref.type == Literal::f_app);
            const NodeIndex f = pn_ref->f_app.function();
            assert(f == hay_ref->f_app.function()); //maybe too strict if the future allows to search unordered in unknown apps

            const std::span<const NodeIndex> needles = pn_ref->f_app.parameters();
            const std::span<const NodeIndex> haystack = hay_ref->f_app.parameters();
            assert(std::is_sorted(needles.begin(), needles.end(), ordered_less(pn_ref.store_data())));
            assert(std::is_sorted(haystack.begin(), haystack.end(), ordered_less(hay_ref.store_data())));

            if (needles.size() > haystack.size()) {
                return false;
            }

            const FAppInfo info = f_app_info(pn_ref);
            SharedFAppEntry& needles_data = match_state.f_app_entries[info.match_state_index];
            assert(needle_i == 0u || needles_data.match_idx == hay_ref.typed_idx() && "rematch only with same subterm");
            needles_data.match_idx = hay_ref.typed_idx();

            //keeps track of wether a param in haystack is currently associated with a param in needles or not
            auto currently_matched = bmath::intern::BitVector(haystack.size()); 

            //if needles_data.match_positions[i] == assoc_match_indicator, then needle i has not matched exactly one parameter of haystack, 
            //  but an arbitrary ammount, as the needle i is a weak match variable currently associated with a function application of the same associative 
            //  function f as pn_ref and hay_ref.
            constexpr auto assoc_match_indicator = (SharedFAppEntry::MatchPos_T)-1u;

            const auto find_associatively_matched = [&](const UnsaveRef needle_matched_ref, bmath::intern::BitVector& matched_by_needle) {
                int k = 0;
                for (const NodeIndex matched_param : needle_matched_ref->f_app.parameters()) {
                    const UnsaveRef matched_param_ref = hay_ref.at(matched_param);
                    for (;;) {
                        while (currently_matched.test(k) && k != haystack.size()) k++;
                        if (k == haystack.size()) return false;

                        const std::strong_ordering ord = compare_tree(matched_param_ref, hay_ref.at(haystack[k]));
                        if (ord == std::strong_ordering::equal) {
                            matched_by_needle.set(k);
                            k++;
                            break; //leave inner loop, find next matched_param in haystack
                        }
                        else if (ord == std::strong_ordering::less) {                            
                            return false; //haystack elems size monotonically increases -> no hope
                        }
                        assert(ord == std::strong_ordering::greater);
                        k++;
                    }
                }
                return true;
            };

            const auto set_matched = [&] {
                for (std::int32_t i = 0; i < needle_i; i++) {
                    const auto match_pos = needles_data.match_positions[i];
                    if (match_pos == assoc_match_indicator) [[unlikely]] {
                        const UnsaveRef needle_ref = pn_ref.at(needles[i]);
                        assert(info.associative && needle_ref.type == SingleMatch::weak);
                        const UnsaveRef needle_matched_ref = hay_ref.at(match_state.single_vars[needle_ref.index].match_idx);
                        assert(needle_matched_ref.type == Literal::f_app && needle_matched_ref->f_app.function() == f);
                        const bool found_all = find_associatively_matched(needle_matched_ref, currently_matched);
                        assert(found_all);
                    }
                    else {
                        currently_matched.set(match_pos);
                    }
                }
            };
            set_matched();

            while (needle_i < needles.size()) {
                const UnsaveRef needle_ref = pn_ref.at(needles[needle_i]);

                //if we currently match in application of associative symbol "f" and needle is currently also matched to an application of "f", 
                //  we wont find the full needle in our haystack, but perhaps its parameters
                if (info.associative && needle_ref.type == SingleMatch::weak) {
                    const UnsaveRef needle_matched_ref = hay_ref.at(match_state.single_vars[needle_ref.index].match_idx);
                    if (needle_matched_ref.type == Literal::f_app && needle_matched_ref->f_app.function() == f) [[unlikely]] {
                        //matched_by_needle buffers needle's matches until the whole needle is confirmed to lie within the haystack, 
                        //  to not require cumbersome resetting of currently_matched, if only parts of needle where found
                        auto matched_by_needle = bmath::intern::BitVector(haystack.size());
                        if (find_associatively_matched(needle_matched_ref, matched_by_needle)) {
                            currently_matched |= matched_by_needle;
                            //normally jump to prepare_next_needle, but this is too different from the usual
                            needles_data.match_positions[needle_i] = assoc_match_indicator; 
                            hay_k = 0;
                            needle_i++;
                            continue;
                        }
                        goto rematch_last_needle;
                    }
                }

                for (; hay_k < haystack.size(); hay_k++) {
                    if (currently_matched.test(hay_k)) continue;

                    const auto cmp = match_(needle_ref, hay_ref.at(haystack[hay_k]), match_state);
                    if (cmp == std::partial_ordering::equivalent) goto prepare_next_needle;
                    //current needle is smaller than current hay, but hay will only get bigger -> no chance
                    if (cmp == std::partial_ordering::less)       goto rematch_last_needle;
                }
            rematch_last_needle:
                if (needle_i == 0) {
                    return false; //failed to match first needle -> there is no hope
                }
                needle_i--;
                hay_k = needles_data.match_positions[needle_i];
                if (hay_k == assoc_match_indicator) [[unlikely]] { 
                    //perhaps there are more efficient ways to do this, but i expect to only seldom encounter this situation anyway
                    currently_matched.reset_all(); 
                    set_matched();
                    continue;
                }
                else if (!info.rematchable_params.test(needle_i) ||
                    !rematch(pn_ref.at(needles[needle_i]), hay_ref.at(haystack[hay_k]), match_state))
                { //could not rematch last successfully-matched needle-hay pair with each other again
                  //  -> try succeeding elements in haystack in next loop iteration for that needle
                    currently_matched.reset(hay_k);
                    hay_k++;
                    continue;
                }
            prepare_next_needle:
                currently_matched.set(hay_k);                   //already set if needle was rematched
                needles_data.match_positions[needle_i] = hay_k; //already set if needle was rematched
                hay_k = info.always_preceeding_next.test(needle_i) ?
                    hay_k + 1 :
                    0;
                needle_i++;
            }
            return info.preceeded_by_multi || currently_matched.count() == haystack.size();
        } //find_permutation

        //analogous to find_permutation, but for non commutative pattern containing at least one multi_marker
        bool find_dilation(const UnsaveRef pn_ref, const UnsaveRef hay_ref, State& match_state, std::int32_t needle_i)
        {
            std::int32_t hay_k = 0; //unlike in find_permutation, this is not a parameter
            assert(pn_ref.type == PatternFApp{} && hay_ref.type == Literal::f_app);

            const std::span<const NodeIndex> needles = pn_ref->f_app.parameters();
            const std::span<const NodeIndex> haystack = hay_ref->f_app.parameters();
            const FAppInfo info = f_app_info(pn_ref);

            SharedFAppEntry& needles_data = match_state.f_app_entries[info.match_state_index];
            assert(needle_i == 0 || needles_data.match_idx == hay_ref.typed_idx() && "rematch only with same subterm");
            needles_data.match_idx = hay_ref.typed_idx();

            //case of beeing called by rematch
            if (needle_i != 0) {
                assert(needle_i == needles.size());
                goto rematch_last_needle;
            }

            if (!needles.size()) {
                assert(info.preceeded_by_multi.count() == 1u);
                return true;
            }
            if (!haystack.size()) {
                return false;
            }
            //how may elements of haystack can still be skipped (aka matched with a multi) without matching a (real) needle
            //note: this test is not neccessairy, only improves performace in such cases
            if ((int)haystack.size() - hay_k - (int)needles.size() + needle_i < 0) {
                return false;
            }
            
        match_current_needle: 
            {
                assert(hay_k < haystack.size());
                const UnsaveRef needle_ref = pn_ref.at(needles[needle_i]);
                do {
                    if (matches(needle_ref, hay_ref.at(haystack[hay_k]), match_state)) {
                        goto prepare_next_needle;
                    }
                } while (info.preceeded_by_multi.test(needle_i) && ++hay_k < haystack.size());
                goto rematch_last_needle;
            }
        prepare_next_needle: 
            {
                needles_data.match_positions[needle_i] = hay_k; //already set if needle was rematched
                needle_i++;
                hay_k++;
                if (needle_i == needles.size()) {
                    if (info.preceeded_by_multi.test(needle_i) || hay_k == haystack.size()) {
                        return true;
                    }
                    goto rematch_last_needle;
                }
                goto match_current_needle;
            }
        rematch_last_needle: 
            while (needle_i-- != 0) {
                hay_k = needles_data.match_positions[needle_i];
                if (info.rematchable_params.test(needle_i)) {
                    if (rematch(pn_ref.at(needles[needle_i]), hay_ref.at(haystack[hay_k]), match_state)) {
                        goto prepare_next_needle;
                    }
                }
                if (info.preceeded_by_multi.test(needle_i)) {
                    hay_k++;
                    goto match_current_needle;
                }
            }
            return false;
        } //find_dilation

        //assumes the first (i - 1) params to already be matched, starts backtracking by remaching param (i - 1)
        //returns true if backtracking was succesfull
        bool BMATH_FORCE_INLINE track_back(const UnsaveRef pn_ref, const UnsaveRef ref, const std::span<const NodeIndex> pn_params,
            const std::span<const NodeIndex> params, State& match_state, std::uint32_t i)
        {
            assert(pn_params.size() == params.size());
            assert(pn_params.size() > 0u);
            assert(i > 0u);
            const auto rematchable = f_app_info(pn_ref).rematchable_params;
            for (;;) {
                i--;
                while (!rematchable.test(i) ||
                    !rematch(pn_ref.at(pn_params[i]), ref.at(params[i]), match_state))
                {
                    if (i == 0u)            return false;
                    i--;
                } do {
                    i++;
                    if (i == params.size()) return true;
                }                 
                while (matches(pn_ref.at(pn_params[i]), ref.at(params[i]), match_state));
            }
        } //track_back

        std::partial_ordering match_(const UnsaveRef pn_ref, const UnsaveRef ref, State& match_state)
        {
            assert(pn_ref.type != Literal::f_app);
            if (pn_ref.type.is<Literal>() && pn_ref.type != ref.type) {
                return shallow_order(pn_ref) <=> shallow_order(ref);
            }

            const auto to_order = [](const bool b) { return b ?
                std::partial_ordering::equivalent :
                std::partial_ordering::unordered; };

            switch (pn_ref.type) {
            case NodeType(Literal::complex):
                return bmath::intern::compare_complex(pn_ref->complex, ref->complex);
            case NodeType(Literal::symbol):
            case NodeType(Literal::lambda_param):
                return pn_ref.index <=> ref.index;
            case NodeType(Literal::lambda): {
                const Lambda pn_lambda = *pn_ref;
                const Lambda lambda = *ref;
                if (pn_lambda.param_count != lambda.param_count) {
                    return pn_lambda.param_count <=> lambda.param_count;
                }
                if (pn_lambda.transparent != lambda.transparent) {
                    return pn_lambda.transparent <=> lambda.transparent;
                }
                return match_(pn_ref.at(pn_lambda.definition), ref.at(lambda.definition), match_state);
            }
            case NodeType(PatternFApp{}): {
                if (ref.type != Literal::f_app) {
                    constexpr int app_order = shallow_order(UnsaveRef(nullptr, 0, Literal::f_app));
                    return app_order <=> shallow_order(ref);
                }
                if (const auto cmp = match_(pn_ref.at(pn_ref->f_app.function()), ref.at(ref->f_app.function()), match_state);
                    cmp != std::partial_ordering::equivalent)
                {   return cmp;
                }

                const MatchStrategy strat = f_app_info(pn_ref).strategy;
                switch (strat) {
                case MatchStrategy::permutation:
                    return to_order(find_permutation(pn_ref, ref, match_state, 0, 0));
                case MatchStrategy::dilation:
                    return to_order(find_dilation(pn_ref, ref, match_state, 0));
                default: {
                    assert(strat == MatchStrategy::linear || strat == MatchStrategy::backtracking);
                    const std::span<const NodeIndex> pn_params = pn_ref->f_app.parameters();
                    const std::span<const NodeIndex> params = ref->f_app.parameters();
                    if (pn_params.size() != params.size()) {
                        return std::partial_ordering::unordered; //TODO: would the ordering still hold if we return greater / smaller?
                    }

                    auto pn_iter = pn_params.begin();
                    const auto pn_start = pn_params.begin();

                    const auto stop = params.end();
                    for (auto iter = params.begin(); iter != stop; ++pn_iter, ++iter) {
                        const auto cmp = match_(pn_ref.at(*pn_iter), ref.at(*iter), match_state);
                        if (cmp != std::partial_ordering::equivalent) {
                            if (strat == MatchStrategy::backtracking && pn_iter != pn_start) {
                                return to_order(track_back(pn_ref, ref, pn_params, params,
                                    match_state, pn_iter - pn_start));
                            }
                            return cmp;
                        }
                    }
                    return std::partial_ordering::equivalent;
                }
                }
            }
            case NodeType(SingleMatch::restricted): {
                const RestrictedSingleMatch var = *pn_ref;
                SharedSingleMatchEntry& entry = match_state.single_vars[var.match_state_index];
                entry.match_idx = ref.typed_idx();
                const bool restriction_fulfilled = [&] {
                    if (var.condition.get_type() == Literal::symbol) {
                        assert(to_symbol(var.condition).is<nv::Native>());
                        return meets_restriction(ref, to_symbol(var.condition).to<nv::Native>());
                    }
                    return test_condition(pn_ref.at(var.condition), match_state);
                }();
                return to_order(restriction_fulfilled);
            }
            case NodeType(SingleMatch::unrestricted): {
                match_state.single_vars[pn_ref.index].match_idx = ref.typed_idx();
                return std::partial_ordering::equivalent;
            }
            case NodeType(SingleMatch::weak): {
                const SharedSingleMatchEntry entry = match_state.single_vars[pn_ref.index];
                assert(entry.is_set());
                //as the order of this depends on the point where the variable is set, 
                //  no conclusion besides "matches" or "matches not" can be drawn.
                // -> the ordering infomation of compare_tree has to be thrown out
                return to_order(compare_tree(ref.at(entry.match_idx), ref) == std::strong_ordering::equivalent);
            }
            case NodeType(SpecialMatch::value): {
                if (ref.type != Literal::complex) { //only this test allows us to pass *ref to evaluate this_value
                    constexpr int value_order = shallow_order(UnsaveRef(nullptr, 0, Literal::complex));
                    return value_order <=> shallow_order(ref);
                }
                const ValueMatch& var = *pn_ref;
                const bmath::intern::OptionalComplex this_value = eval_value_match(pn_ref.at(var.inverse), *ref);
                if (!this_value || !in_complex_subset(*this_value, var.domain)) {
                    return std::partial_ordering::unordered;
                }
                else {
                    auto& match_info = match_state.value_entry(var);
                    if (var.owner) {
                        match_info.value = *this_value;
                        return std::partial_ordering::equivalent;
                    }
                    assert(match_info.is_set());
                    return bmath::intern::compare_complex(*this_value, match_info.value);
                }
            } break;
            case NodeType(SpecialMatch::multi): //multi is matched in PatternFApp, not here
            default:
                assert(false);
                BMATH_UNREACHABLE;
                return std::partial_ordering::unordered;
            }
        } //match_

        bool rematch(const UnsaveRef pn_ref, const UnsaveRef ref, State& match_state)
        {
            assert(pn_ref.type != Literal::f_app);
            if (pn_ref.type == PatternFApp{}) {
                assert(ref.type == Literal::f_app);
                switch (f_app_info(pn_ref).strategy) {
                case MatchStrategy::permutation: {
                    const SharedFAppEntry& entry = match_state.f_app_entry(pn_ref);
                    assert(entry.match_idx == ref.typed_idx());

                    const auto needle_params = pn_ref->f_app.parameters();
                    const std::uint32_t needle_i = needle_params.size() - 1u;
                    const std::uint32_t hay_k = entry.match_positions[needle_i] + 1u;

                    return find_permutation(pn_ref, ref, match_state, needle_i, hay_k);
                } break;
                case MatchStrategy::dilation:
                    return find_dilation(pn_ref, ref, match_state, pn_ref->f_app.parameters().size());
                case MatchStrategy::backtracking: {
                    const std::span<const NodeIndex> pn_params = pn_ref->f_app.parameters();
                    const std::span<const NodeIndex> params = ref->f_app.parameters();
                    return track_back(pn_ref, ref, pn_params, params, match_state, pn_params.size());
                } break;
                case MatchStrategy::linear:
                    assert(false); //why would one try to rematch something not rematchable?
                    BMATH_UNREACHABLE;
                }
            }
            else if (pn_ref.type == Literal::lambda) {
                return rematch(pn_ref.at(pn_ref->lambda.definition), ref.at(ref->lambda.definition), match_state);
            }
            return false;
        } //rematch

    } //namespace match


    NodeIndex pattern_interpretation(const UnsaveRef pn_ref, const match::State& match_state, 
        Store& store, const Options options)
    {
        const auto insert_node = [&](const TermNode node, const NodeType type) {
            const std::size_t dst_index = store.allocate_one();
            store.at(dst_index) = node;
            return NodeIndex(dst_index, type);
        };

        switch (pn_ref.type) {
        case NodeType(Literal::complex): {
            return insert_node(*pn_ref, pn_ref.type);
        } break;
        case NodeType(Literal::symbol):
            return pn_ref.typed_idx();
        case NodeType(Literal::lambda): {
            Lambda dst_lam = *pn_ref;
            dst_lam.definition = pattern_interpretation(
                pn_ref.at(dst_lam.definition), match_state, store, options);
            return insert_node(dst_lam, pn_ref.type);
        } break;
        case NodeType(Literal::lambda_param):
            return pn_ref.typed_idx();
        case NodeType(Literal::f_app): {
            bmath::intern::StupidBufferVector<NodeIndex, 16> dst_subterms;
            const auto stop = end(pn_ref);
            auto iter = begin(pn_ref);
            for (; iter != stop; ++iter) {
                if (iter->get_type() == SpecialMatch::multi) {
                    const MultiMatch multi = *pn_ref.at(*iter);
                    match::for_each_multi(multi, match_state, 
                        [&](const NodeIndex n) { dst_subterms.push_back(share(store, n)); });
                } 
                else {
                    dst_subterms.push_back(pattern_interpretation(
                        pn_ref.at(*iter), match_state, store, options));
                }
            } //end for
            const std::size_t dst_index = FApp::build(store, dst_subterms);
            return normalize::outermost(MutRef(store, dst_index, pn_ref.type), options).res;
        } break;
        case NodeType(SingleMatch::weak): {            
            const match::SharedSingleMatchEntry& entry = match_state.single_vars[pn_ref.index];
            assert(entry.is_set());
            return share(store, entry.match_idx);
        } break;
        case NodeType(SpecialMatch::value): {
            const Complex& val = match_state.value_entry(*pn_ref).value;
            return insert_node(val, Literal::complex);
        } break;
        default:
            assert(false);
            BMATH_UNREACHABLE;
            return invalid_index;
        }
    } //pattern_interpretation
 
} //namespace simp
