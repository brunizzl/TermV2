#pragma once	//NOLINT

#include "baseTerm.hpp"

namespace bmath::intern {

	namespace order {
		//used to sort terms by uniqueness
		//more unique is greater
		std::strong_ordering compare_uniqueness(Type fst, Type snd);

		//used to determine if parentheses are needed when displaying term with fst and snd only one layer apart
		//higher operator precedence is greater
		std::strong_ordering compare_precedence(Type fst, Type snd);

	} //namespace order

	namespace construction {

	} //namespace construction

	namespace matching {

	} //namespace matching

	inline std::complex<double> add(std::complex<double> fst, std::complex<double> snd) { return fst + snd; }
	inline std::complex<double> sub(std::complex<double> fst, std::complex<double> snd) { return fst - snd; }
	inline std::complex<double> mul(std::complex<double> fst, std::complex<double> snd) { return fst * snd; }
	inline std::complex<double> div(std::complex<double> fst, std::complex<double> snd) { return fst / snd; }

} //namespace bmath::intern