#pragma once	//NOLINT

#include <cassert>
#include <string_view>

#include "baseTerm.hpp"
#include "internalFunctions.hpp"

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
	static constexpr Par_Op_Type all_par_op_types[] = { Par_Op_Type::log10, Par_Op_Type::asinh, Par_Op_Type::acosh, Par_Op_Type::atanh,
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

	template<Modifier modifier>
	class Parenthesis_Operator final : public Base_Term<modifier>
	{
	public:
		typedef Base_Term<modifier> Base;

		Par_Op_Type op_type;
		Base* argument;

		Parenthesis_Operator(Par_Op_Type op_type_, Base* argument_) :op_type(op_type_), argument(argument_) {}
		~Parenthesis_Operator() { delete this->argument; }

		virtual void to_str(std::string& str, Type parent_type) const override
		{
			str.append(name_of(this->op_type));
			this->argument->to_str(str, this->get_type());
			str.push_back(')');
		}

		virtual /*constexpr*/ Type get_type() const override { return Type::par_operator; }

		static /*constexpr*/ Parenthesis_Operator* down_cast(Base* base_ptr)
			{ assert(base_ptr->get_type() == Type::par_operator); return static_cast<Parenthesis_Operator*>(base_ptr); }

		static /*constexpr*/ const Parenthesis_Operator* down_cast(const Base* base_ptr)
			{ assert(base_ptr->get_type() == Type::par_operator); return static_cast<const Parenthesis_Operator*>(base_ptr); }

		virtual /*constexpr*/ std::partial_ordering lexicographical_compare(const Base& snd) const override
		{
			if (const auto compare_types = order::compare_uniqueness(this->get_type(), snd.get_type()); compare_types != std::partial_ordering::equivalent) {
				return compare_types;
			}
			else {
				const Parenthesis_Operator* const snd_par_op = Parenthesis_Operator::down_cast(&snd);
				if (this->op_type != snd_par_op->op_type) {
					return this->op_type <=> snd_par_op->op_type;
				}
				else {
					return this->argument->lexicographical_compare(*snd_par_op->argument);
				}
			}
		}

		virtual /*constexpr*/ bool equals(const Base& snd) const override
		{
			if (this->get_type() != snd.get_type()) {
				return false;
			}
			else {
				const Parenthesis_Operator* const snd_par_op = Parenthesis_Operator::down_cast(&snd);
				return this->op_type == snd_par_op->op_type && this->argument->equals(*snd_par_op->argument);
			}
		}

		virtual /*constexpr*/ void for_each(std::function<void(Base* this_ptr, Type this_type, Base** this_storage_key)> func, Base** this_storage_key) override
		{
			this->argument->for_each(func, &this->argument);
			func(this, this->get_type(), this_storage_key);
		}
	};

	using Regular_Par_Op = Parenthesis_Operator<Modifier::regular>;
	using Pattern_Par_Op = Parenthesis_Operator<Modifier::pattern>;


	template<Modifier modifier>
	class Logarithm final : public Base_Term<modifier>
	{
	public:
		typedef Base_Term<modifier> Base;

		Base* base;
		Base* argument;

		Logarithm(Base* base_, Base* argument_) : base(base_), argument(argument_) {}
		~Logarithm() { delete this->base; delete this->argument; }

		virtual void to_str(std::string& str, Type parent_type) const override
		{
			str.append("log(");
			this->base->to_str(str, this->get_type());
			str.push_back(',');
			this->argument->to_str(str, this->get_type());
			str.push_back(')');
		}
		
		virtual /*constexpr*/ Type get_type() const override { return Type::logarithm; }

		static /*constexpr*/ Logarithm* down_cast(Base* base_ptr)
			{ assert(base_ptr->get_type() == Type::logarithm); return static_cast<Logarithm*>(base_ptr); }

		static /*constexpr*/ const Logarithm* down_cast(const Base* base_ptr)
			{ assert(base_ptr->get_type() == Type::logarithm); return static_cast<const Logarithm*>(base_ptr); }

		virtual /*constexpr*/ std::partial_ordering lexicographical_compare(const Base& snd) const override
		{
			if (const auto compare_types = order::compare_uniqueness(this->get_type(), snd.get_type()); compare_types != std::partial_ordering::equivalent) {
				return compare_types;
			}
			else {
				const Logarithm* const snd_log = Logarithm::down_cast(&snd);
				if (const auto compare_base = this->base->lexicographical_compare(*snd_log->base); compare_base != std::partial_ordering::equivalent) {
					return compare_base;
				}
				else {
					return this->argument->lexicographical_compare(*snd_log->argument);
				}
			}
		}

		virtual /*constexpr*/ bool equals(const Base& snd) const override
		{
			if (this->get_type() != snd.get_type()) {
				return false;
			}
			else {
				const Logarithm* const snd_log = Logarithm::down_cast(&snd);
				return this->base->equals(*snd_log->base) && this->argument->equals(*snd_log->argument);
			}
		}

		virtual /*constexpr*/ void for_each(std::function<void(Base* this_ptr, Type this_type, Base** this_storage_key)> func, Base** this_storage_key) override
		{
			this->base->for_each(func, &this->base);
			this->argument->for_each(func, &this->argument);
			func(this, this->get_type(), this_storage_key);
		}
	};

	using Regular_Log = Logarithm<Modifier::regular>;
	using Pattern_Log = Logarithm<Modifier::pattern>;


	template<Modifier modifier>
	class Power final : public Base_Term<modifier>
	{
	public:
		typedef Base_Term<modifier> Base;

		Base* base;
		Base* exponent;

		Power(Base* base_, Base* exponent_) : base(base_), exponent(exponent_) {}
		~Power() { delete this->base; delete this->exponent; }

		virtual void to_str(std::string& str, Type parent_type) const override
		{
			str.push_back('(');
			this->base->to_str(str, this->get_type());
			str.push_back('^');
			this->exponent->to_str(str, this->get_type());
			str.push_back(')');
		}

		virtual /*constexpr*/ Type get_type() const override { return Type::power; }

		static /*constexpr*/ Power* down_cast(Base* base_ptr)
			{ assert(base_ptr->get_type() == Type::power); return static_cast<Power*>(base_ptr); }

		static /*constexpr*/ const Power* down_cast(const Base* base_ptr)
			{ assert(base_ptr->get_type() == Type::power); return static_cast<const Power*>(base_ptr); }

		virtual /*constexpr*/ std::partial_ordering lexicographical_compare(const Base& snd) const override
		{
			if (const auto compare_types = order::compare_uniqueness(this->get_type(), snd.get_type()); compare_types != std::partial_ordering::equivalent) {
				return compare_types;
			}
			else {
				const Power* const snd_power = Power::down_cast(&snd);
				if (const auto compare_base = this->base->lexicographical_compare(*snd_power->base); compare_base != std::partial_ordering::equivalent) {
					return compare_base;
				}
				else {
					return this->exponent->lexicographical_compare(*snd_power->exponent);
				}
			}
		}

		virtual /*constexpr*/ bool equals(const Base& snd) const override
		{
			if (this->get_type() != snd.get_type()) {
				return false;
			}
			else {
				const Power* const snd_power = Power::down_cast(&snd);
				return this->base->equals(*snd_power->base) && this->exponent->equals(*snd_power->exponent);
			}
		}

		virtual /*constexpr*/ void for_each(std::function<void(Base* this_ptr, Type this_type, Base** this_storage_key)> func, Base** this_storage_key) override
		{
			this->base->for_each(func, &this->base);
			this->exponent->for_each(func, &this->exponent);
			func(this, this->get_type(), this_storage_key);
		}
	};

	using Regular_Power = Power<Modifier::regular>;
	using Pattern_Power = Power<Modifier::pattern>;

} //namespace bmath::intern