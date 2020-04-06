#pragma once	//NOLINT

#include <cassert>

#include "baseTerm.hpp"

namespace bmath::intern {



	template<Modifier modifier>
	class Value final : public Base_Term<modifier>
	{
	public:
		std::complex<double> number;

		Value(double re, double im) :number(re, im) {}
		virtual ~Value() {} //no pointers to other terms are owned -> nothing to do here

		virtual void to_str(std::string& str, Type parent_type) const override { str.append("value"); }

		virtual /*constexpr*/ Type get_type() const override { return Type::value; }

		static /*constexpr*/ Value<modifier>* down_cast(Base_Term<modifier>* base_ptr)
			{ assert(base_ptr->get_type() == Type::value); return static_cast<Value<modifier>*>(base_ptr); }

		static /*constexpr*/ const Value<modifier>* down_cast(const Base_Term<modifier>* base_ptr)
			{ assert(base_ptr->get_type() == Type::value); return static_cast<const Value<modifier>*>(base_ptr); }

		virtual /*constexpr*/ std::partial_ordering lexicographical_compare(const Base_Term<modifier>& snd) const override
		{
			if (const auto compare_types = order::compare_uniqueness(this->get_type(), snd.get_type()); compare_types != std::partial_ordering::equivalent) {
				return compare_types;
			}
			else {
				const Value* const snd_value = Value::down_cast(&snd);
				if (this->number.real() != snd_value->number.real()) {
					return this->number.real() <=> snd_value->number.real();
				}
				else {
					return this->number.imag() <=> snd_value->number.imag();
				}
			}
		}
	};

	using Regular_Value = Value<Modifier::regular>;
	using Pattern_Value = Value<Modifier::pattern>;


	class Regular_Variable final : public Regular_Term
	{
	public:
		const std::string name;

		virtual void to_str(std::string& str, Type parent_type) const override { str.append(this->name); }

		virtual /*constexpr*/ Type get_type() const override { return Type::variable; }

		static /*constexpr*/ Regular_Variable* down_cast(Regular_Term* base_ptr)
			{ assert(base_ptr->get_type() == Type::variable); return static_cast<Regular_Variable*>(base_ptr); }

		static /*constexpr*/ const Regular_Variable* down_cast(const Regular_Term* base_ptr)
			{ assert(base_ptr->get_type() == Type::variable); return static_cast<const Regular_Variable*>(base_ptr); }
	};

	class Pattern_Variable final : public Pattern_Term
	{
	public:
		const std::string name;

		Regular_Term* matched_term;

		virtual void to_str(std::string& str, Type parent_type) const override { str.append('{' + this->name + '}'); }

		virtual /*constexpr*/ Type get_type() const override { return Type::variable; }

		static /*constexpr*/ Pattern_Variable* down_cast(Pattern_Term* base_ptr)
			{ assert(base_ptr->get_type() == Type::variable); return static_cast<Pattern_Variable*>(base_ptr); }

		static /*constexpr*/ const Pattern_Variable* down_cast(const Pattern_Term* base_ptr)
			{ assert(base_ptr->get_type() == Type::variable); return static_cast<const Pattern_Variable*>(base_ptr); }
	};

} //namespace bmath::intern