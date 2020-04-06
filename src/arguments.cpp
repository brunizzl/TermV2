
#include "arguments.hpp"

std::partial_ordering bmath::intern::Regular_Variable::lexicographical_compare(const Regular_Term& snd) const
{
	if (const auto compare_types = order::compare_uniqueness(this->get_type(), snd.get_type()); compare_types != std::partial_ordering::equivalent) {
		return compare_types;
	}
	else {
		const Regular_Variable* const snd_variable = Regular_Variable::down_cast(&snd);
		//return this->name <=> snd_variable->name;
		if (this->name == snd_variable->name) {
			return std::partial_ordering::equivalent;
		}
		else if (this->name > snd_variable->name) {
			return std::partial_ordering::greater;
		}
		else {
			return std::partial_ordering::less;
		}
	}
}

bool bmath::intern::Regular_Variable::equals(const Regular_Term& snd) const
{
	if (this->get_type() != snd.get_type()) {
		return false;
	}
	else {
		const Regular_Variable* const snd_variable = Regular_Variable::down_cast(&snd);
		return this->name == snd_variable->name;
	}
}

std::partial_ordering bmath::intern::Pattern_Variable::lexicographical_compare(const Pattern_Term& snd) const
{
	if (const auto compare_types = order::compare_uniqueness(this->get_type(), snd.get_type()); compare_types != std::partial_ordering::equivalent) {
		return compare_types;
	}
	else {
		const Pattern_Variable* const snd_variable = Pattern_Variable::down_cast(&snd);
		if (this->matched_term != nullptr && snd_variable->matched_term != nullptr) {
			return this->matched_term->lexicographical_compare(*snd_variable->matched_term);
		}
		if (this->matched_term == nullptr && snd_variable->matched_term == nullptr) {
			//return this->name <=> snd_variable->name;
			if (this->name == snd_variable->name) {
				return std::partial_ordering::equivalent;
			}
			else if (this->name > snd_variable->name) {
				return std::partial_ordering::greater;
			}
			else {
				return std::partial_ordering::less;
			}
		}
		return this->matched_term <=> snd_variable->matched_term;
	}
}

bool bmath::intern::Pattern_Variable::equals(const Pattern_Term& snd) const
{
	if (this->get_type() != snd.get_type()) {
		return false;
	}
	else {
		const Pattern_Variable* const snd_variable = Pattern_Variable::down_cast(&snd);
		if (this->matched_term != nullptr && snd_variable->matched_term != nullptr) {
			return this->matched_term->equals(*snd_variable->matched_term);
		}
		if (this->matched_term == nullptr && snd_variable->matched_term == nullptr) {
			return this->name == snd_variable->name;
		}
		return false;
	}
}