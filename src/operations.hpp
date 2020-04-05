#pragma once	//NOLINT

#include <cassert>
#include <string_view>

#include "baseTerm.hpp"

namespace bmath::intern {

	//types names are sorted by length (used to be required, as the type_subterm() function searched for par_op at not only the beginning of the name string)
	//(comments are corresponding std::complex functions)
	enum class Par_Op_Type
	{
		log10,			//log10()
		asinh,			//asinh()
		acosh,			//acosh()
		atanh,			//atanh()
		asin,			//asin()
		acos,			//acos()
		atan,			//atan()
		sinh,			//sinh()
		cosh,			//cosh()
		tanh,			//tanh()
		sqrt,			//sqrt()
		exp,			//exp()
		sin,			//sin()
		cos,			//cos()
		tan,			//tan()
		abs,			//abs()
		arg,			//arg()
		ln,				//log()
		re,				//real()
		im,				//imag()
	};

	//array with instance of every Par_Op_Type to iterate over them
	static const Par_Op_Type all_par_op_types[] = { Par_Op_Type::log10, Par_Op_Type::asinh, Par_Op_Type::acosh, Par_Op_Type::atanh,
													Par_Op_Type::asin, Par_Op_Type::acos, Par_Op_Type::atan, Par_Op_Type::sinh,
													Par_Op_Type::cosh, Par_Op_Type::tanh, Par_Op_Type::sqrt, Par_Op_Type::exp,
													Par_Op_Type::sin, Par_Op_Type::cos, Par_Op_Type::tan, Par_Op_Type::abs,
													Par_Op_Type::arg, Par_Op_Type::ln, Par_Op_Type::re, Par_Op_Type::im };

	constexpr std::string_view name_of(Par_Op_Type op_type)
	{
		switch (op_type) {
		case Par_Op_Type::log10:	return { "log10(" };
		case Par_Op_Type::asinh:	return { "asinh(" };
		case Par_Op_Type::acosh:	return { "acosh(" };
		case Par_Op_Type::atanh:	return { "atanh(" };
		case Par_Op_Type::asin:		return { "asin(" };
		case Par_Op_Type::acos:		return { "acos(" };
		case Par_Op_Type::atan:		return { "atan(" };
		case Par_Op_Type::sinh:		return { "sinh(" };
		case Par_Op_Type::cosh:		return { "cosh(" };
		case Par_Op_Type::tanh:		return { "tanh(" };
		case Par_Op_Type::sqrt:		return { "sqrt(" };
		case Par_Op_Type::exp:		return { "exp(" };
		case Par_Op_Type::sin:		return { "sin(" };
		case Par_Op_Type::cos:		return { "cos(" };
		case Par_Op_Type::tan:		return { "tan(" };
		case Par_Op_Type::abs:		return { "abs(" };
		case Par_Op_Type::arg:		return { "arg(" };
		case Par_Op_Type::ln:		return { "ln(" };
		case Par_Op_Type::re:		return { "re(" };
		case Par_Op_Type::im:		return { "im(" };
		}
		assert(false);
		return {};
	}

	template<Base_Type base_type>
	class Parentheses_Operator final : public Base_Term<base_type>
	{
	public:
		Par_Op_Type op_type;
		Base_Term<base_type>* argument;

		~Parentheses_Operator() { delete this->argument; }

		virtual void to_str(std::string& str, Derived_Type parent_type) const override
		{
			str.append(name_of(this->op_type));
			this->argument->to_str(str, this->get_type());
			str.append(')');
		}

		virtual /*constexpr*/ Derived_Type get_type() const override { return Derived_Type::par_operator; }

		static /*constexpr*/ Parentheses_Operator* down_cast(Base_Term<base_type>* base_ptr)
			{ assert(base_ptr->get_type() == Derived_Type::par_operator); return static_cast<Parentheses_Operator*>(base_ptr); }
	};

	using Regular_Par_Op = Parentheses_Operator<Base_Type::regular>;
	using Pattern_Par_Op = Parentheses_Operator<Base_Type::pattern>;
	static_assert(!std::is_same<Regular_Par_Op, Pattern_Par_Op>::value);


	template<Base_Type base_type>
	class Logarithm final : public Base_Term<base_type>
	{
	public:
		Base_Term<base_type>* base;
		Base_Term<base_type>* argument;

		~Logarithm() { delete this->base; delete this->argument; }

		virtual void to_str(std::string& str, Derived_Type parent_type) const override
		{
			str.append("log(");
			this->base->to_str(str, this->get_type());
			str.push_back(',');
			this->argument->to_str(str, this->get_type());
			str.push_back(')');
		}
		
		virtual /*constexpr*/ Derived_Type get_type() const override { return Derived_Type::generic_log; }

		static /*constexpr*/ Logarithm* down_cast(Base_Term<base_type>* base_ptr)
			{ assert(base_ptr->get_type() == Derived_Type::generic_log); return static_cast<Logarithm*>(base_ptr); }
	};

	using Regular_Log = Logarithm<Base_Type::regular>;
	using Pattern_Log = Logarithm<Base_Type::pattern>;
	static_assert(!std::is_same<Regular_Log, Pattern_Log>::value);


	template<Base_Type base_type>
	class Power final : public Base_Term<base_type>
	{
	public:
		Base_Term<base_type>* base;
		Base_Term<base_type>* exponent;

		~Power() { delete this->base; delete this->exponent; }

		virtual void to_str(std::string& str, Derived_Type parent_type) const override
		{
			str.push_back('(');
			this->base->to_str(str, this->get_type());
			str.push_back('^');
			this->exponent->to_str(str, this->get_type());
			str.push_back(')');
		}

		virtual /*constexpr*/ Derived_Type get_type() const override { return Derived_Type::power; }

		static /*constexpr*/ Power* down_cast(Base_Term<base_type>* base_ptr)
			{ assert(base_ptr->get_type() == Derived_Type::power); return static_cast<Power*>(base_ptr); }
	};

} //namespace bmath::intern