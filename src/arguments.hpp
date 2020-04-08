#pragma once	//NOLINT

#include <cassert>
#include <sstream>

#include "baseTerm.hpp"
#include "internalFunctions.hpp"

namespace bmath::intern {

	template<Modifier modifier>
	class Value final : public Base_Term<modifier>
	{
	public:
		typedef Base_Term<modifier> Base;

		std::complex<double> number;

		Value(std::complex<double> number_) :number(number_) {}
		virtual ~Value() {} //no pointers to other terms are owned -> nothing to do here

		virtual void to_str(std::string& str, Type parent_type) const override { 
			std::stringstream stream;
			stream << this->number;
			str.append(stream.str()); 
		}

		virtual /*constexpr*/ Type get_type() const override { return Type::value; }

		static /*constexpr*/ Value* down_cast(Base* base_ptr)
			{ assert(base_ptr->get_type() == Type::value); return static_cast<Value*>(base_ptr); }

		static /*constexpr*/ const Value* down_cast(const Base* base_ptr)
			{ assert(base_ptr->get_type() == Type::value); return static_cast<const Value*>(base_ptr); }

		virtual /*constexpr*/ std::partial_ordering lexicographical_compare(const Base& snd) const override
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

		virtual /*constexpr*/ bool equals(const Base& snd) const override
		{
			if (this->get_type() != snd.get_type()) {
				return false;
			}
			else {
				const Value* const snd_value = Value::down_cast(&snd);
				return this->number == snd_value->number;
			}
		}

		virtual /*constexpr*/ void for_each(std::function<void(Base* this_ptr, Type this_type, Base** this_storage_key)> func, Base** this_storage_key) override
		{
			func(this, this->get_type(), this_storage_key);
		}
	};

	using Regular_Value = Value<Modifier::regular>;
	using Pattern_Value = Value<Modifier::pattern>;


	class Regular_Variable final : public Regular_Term
	{
	public:
		const std::string name;

		Regular_Variable(std::string_view name_) : name(name_) {}
		~Regular_Variable() {}

		virtual void to_str(std::string& str, Type parent_type) const override { str.append(this->name); }

		virtual /*constexpr*/ Type get_type() const override { return Type::variable; }

		static /*constexpr*/ Regular_Variable* down_cast(Regular_Term* base_ptr)
			{ assert(base_ptr->get_type() == Type::variable); return static_cast<Regular_Variable*>(base_ptr); }

		static /*constexpr*/ const Regular_Variable* down_cast(const Regular_Term* base_ptr)
			{ assert(base_ptr->get_type() == Type::variable); return static_cast<const Regular_Variable*>(base_ptr); }

		virtual /*constexpr*/ std::partial_ordering lexicographical_compare(const Regular_Term& snd) const override;

		virtual /*constexpr*/ bool equals(const Regular_Term& snd) const override;

		virtual /*constexpr*/ void for_each(std::function<void(Regular_Term* this_ptr, Type this_type, Regular_Term** this_storage_key)> func, Regular_Term** this_storage_key) override
		{
			func(this, this->get_type(), this_storage_key);
		}
	};

	//Pattern_Term is meant to be compared against Regular_Term. A Pattern_Variable can stand for any arbitrary term and its subterms, but it -
	//always represents this same thing for the (possibly) multiple times it occurs in a Pattern_Term. This behavior breakes the tree structure of a pattern,
	//as one all parts of a pattern holding Pattern_Variable "x" need to point to the same instance.
	class Pattern_Variable final : public Pattern_Term
	{
	public:
		const std::string name;
		Regular_Term* matched_term;

		Pattern_Variable(std::string_view name_) : name(name_), matched_term(nullptr) {}
		~Pattern_Variable() {} //matched term is not owned -> nothing to be deleted here

		virtual void to_str(std::string& str, Type parent_type) const override { str.append('{' + this->name + '}'); }

		virtual /*constexpr*/ Type get_type() const override { return Type::variable; }

		static /*constexpr*/ Pattern_Variable* down_cast(Pattern_Term* base_ptr)
			{ assert(base_ptr->get_type() == Type::variable); return static_cast<Pattern_Variable*>(base_ptr); }

		static /*constexpr*/ const Pattern_Variable* down_cast(const Pattern_Term* base_ptr)
			{ assert(base_ptr->get_type() == Type::variable); return static_cast<const Pattern_Variable*>(base_ptr); }

		virtual /*constexpr*/ std::partial_ordering lexicographical_compare(const Pattern_Term& snd) const override;

		virtual /*constexpr*/ bool equals(const Pattern_Term& snd) const override;

		virtual /*constexpr*/ void for_each(std::function<void(Pattern_Term* this_ptr, Type this_type, Pattern_Term** this_storage_key)> func, Pattern_Term** this_storage_key) override
		{
			func(this, this->get_type(), this_storage_key);
		}
	};

} //namespace bmath::intern