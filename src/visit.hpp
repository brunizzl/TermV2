#pragma once	//NOLINT

#include "baseTerm.hpp"

namespace bmath::intern {

	template<Modifier modifier, typename F1_F, typename Log_F, typename Pow_F, typename Pro_F, typename Sum_F, typename Val_F, typename Var_F, typename vCo_F>
	auto base_visit(Base_Term<modifier>* term, F1_F f1_f, Log_F log_f, Pow_F pow_f, Pro_F pro_f, Sum_F sum_f, Val_F val_f, Var_F var_f, vCo_F vCo_f)
	{
		switch (term->type) {
		case Type::function_1: return f1_f(static_cast<Function_1<modifier>*>(term));
		case Type::logarithm: return log_f(static_cast<Logarithm<modifier>*>(term));
		case Type::power: return pow_f(static_cast<Power<modifier>*>(term));		
		case Type::product: return pro_f(static_cast<Product<modifier>*>(term));	
		case Type::sum: return sum_f(static_cast<Sum<modifier>*>(term));			
		case Type::value: return val_f(static_cast<Value<modifier>*>(term));		
		case Type::variable: 
			if constexpr (modifier == Modifier::regular)
				return var_f(static_cast<Regular_Variable*>(term));
			if constexpr (modifier == Modifier::pattern) 
				return var_f(static_cast<Pattern_Variable*>(term));
		case Type::variadic_comprehension:
			if constexpr (modifier == Modifier::regular)
				assert(false); return var_f(static_cast<Regular_Variable*>(term));
			if constexpr (modifier == Modifier::pattern) 
				return vCo_f(static_cast<Variadic_Comprehension*>(term));
		default:
			assert(false); return var_f(static_cast<Regular_Variable*>(term));
		}
	}

	template<Modifier modifier, typename F1_F, typename Log_F, typename Pow_F, typename Pro_F, typename Sum_F, typename Val_F, typename Var_F, typename vCo_F>
	auto base_visit(const Base_Term<modifier>* term, F1_F f1_f, Log_F log_f, Pow_F pow_f, Pro_F pro_f, Sum_F sum_f, Val_F val_f, Var_F var_f, vCo_F vCo_f)
	{
		switch (term->type) {
		case Type::function_1: return f1_f(static_cast<const Function_1<modifier>*>(term));	
		case Type::logarithm: return log_f(static_cast<const Logarithm<modifier>*>(term));	
		case Type::power: return pow_f(static_cast<const Power<modifier>*>(term));			
		case Type::product: return pro_f(static_cast<const Product<modifier>*>(term));		
		case Type::sum: return sum_f(static_cast<const Sum<modifier>*>(term));				
		case Type::value: return val_f(static_cast<const Value<modifier>*>(term));			
		case Type::variable: 
			if constexpr (modifier == Modifier::regular)
				return var_f(static_cast<const Regular_Variable*>(term));
			if constexpr (modifier == Modifier::pattern) 
				return var_f(static_cast<const Pattern_Variable*>(term));
		case Type::variadic_comprehension:
			if constexpr (modifier == Modifier::regular)
				assert(false); return var_f(static_cast<const Regular_Variable*>(term));
			if constexpr (modifier == Modifier::pattern) 
				return vCo_f(static_cast<const Variadic_Comprehension*>(term));
		default:
			assert(false); return var_f(static_cast<const Regular_Variable*>(term));
		}
	}

	template<typename F1_F, typename Log_F, typename Pow_F, typename Pro_F, typename Sum_F, typename Val_F, typename Var_F>
	auto regular_visit(Regular_Term* term, F1_F f1_f, Log_F log_f, Pow_F pow_f, Pro_F pro_f, Sum_F sum_f, Val_F val_f, Var_F var_f)
	{
		return base_visit(term, f1_f, log_f, pow_f, pro_f, sum_f, val_f, var_f, val_f);
	}

	template<typename F1_F, typename Log_F, typename Pow_F, typename Pro_F, typename Sum_F, typename Val_F, typename Var_F>
	auto regular_visit(const Regular_Term* term, F1_F f1_f, Log_F log_f, Pow_F pow_f, Pro_F pro_f, Sum_F sum_f, Val_F val_f, Var_F var_f)
	{
		return base_visit(term, f1_f, log_f, pow_f, pro_f, sum_f, val_f, var_f, val_f);
	}

	template<typename F1_F, typename Log_F, typename Pow_F, typename Pro_F, typename Sum_F, typename Val_F, typename Var_F, typename vCo_F>
	auto pattern_visit(Pattern_Term* term, F1_F f1_f, Log_F log_f, Pow_F pow_f, Pro_F pro_f, Sum_F sum_f, Val_F val_f, Var_F var_f, vCo_F vCo_f)
	{
		return base_visit(term, f1_f, log_f, pow_f, pro_f, sum_f, val_f, var_f, vCo_f);
	}

	template<typename F1_F, typename Log_F, typename Pow_F, typename Pro_F, typename Sum_F, typename Val_F, typename Var_F, typename vCo_F>
	auto pattern_visit(const Pattern_Term* term, F1_F f1_f, Log_F log_f, Pow_F pow_f, Pro_F pro_f, Sum_F sum_f, Val_F val_f, Var_F var_f, vCo_F vCo_f)
	{
		return base_visit(term, f1_f, log_f, pow_f, pro_f, sum_f, val_f, var_f, vCo_f);
	}
}