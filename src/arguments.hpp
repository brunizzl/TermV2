#pragma once	//NOLINT

#include <cassert>

#include "baseTerm.hpp"

namespace bmath::intern {



	template<Base_Type base_type>
	class Value final : public Base_Term<base_type>, public std::complex<double>
	{
	public:
		Value(double re, double im) :std::complex<double>(re, im) {}
		virtual ~Value() {} //no pointers to other terms are owned -> nothing to do here

		virtual void to_str(std::string& str, Derived_Type parent_type) const override { str.append("value"); }

		virtual /*constexpr*/ Derived_Type get_type() const override { return Derived_Type::value; }

		static constexpr Value<base_type>* down_cast(Base_Term<base_type>* base_ptr)
			{ assert(base_ptr->get_type() == Derived_Type::value); return static_cast<Value<base_type>*>(base_ptr); }
	};

	using Regular_Value = Value<Base_Type::regular>;
	using Pattern_Value = Value<Base_Type::pattern>;
	static_assert(!std::is_same<Regular_Value, Pattern_Value>::value);	//the reason to use templates with Base_Type is to create different types.


	class Regular_Variable final : public Regular_Term
	{
	public:
		const std::string name;

		virtual void to_str(std::string& str, Derived_Type parent_type) const override { str.append(this->name); }

		virtual /*constexpr*/ Derived_Type get_type() const override { return Derived_Type::variable; }

		static /*constexpr*/ Regular_Variable* down_cast(Regular_Term* base_ptr)
			{ assert(base_ptr->get_type() == Derived_Type::variable); return static_cast<Regular_Variable*>(base_ptr); }
	};

	class Pattern_Variable final : public Pattern_Term
	{
	public:
		const std::string name;

		Regular_Term* matched_term;

		virtual void to_str(std::string& str, Derived_Type parent_type) const override { str.append('{' + this->name + '}'); }

		virtual /*constexpr*/ Derived_Type get_type() const override { return Derived_Type::variable; }

		static /*constexpr*/ Pattern_Variable* down_cast(Pattern_Term* base_ptr)
			{ assert(base_ptr->get_type() == Derived_Type::variable); return static_cast<Pattern_Variable*>(base_ptr); }

	};

} //namespace bmath::intern