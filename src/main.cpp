
#include "stupidTests.hpp"
using namespace bmath::intern;

/*
TODO:

important:
 - let type_table in ioarithmetic use names of fn
 - make pattern and usual term two distinct types (again...)
	enables: 
		- variadic commutative can have table with extra info of operands, allowing faster matches / fails, also index in match_data circumvents O(n) lookup
		    info may contain: owns match variables (also rematchable vs. not rematchable) vs. has only unowned match variables vs. ordinary term
			                  compare structure to predecessor: same structure vs. identical vs. unordered vs. always occurs after vs. always occurs bevore
		- MathUnion only composed of Complex, String and Params -> easier to debug
 - pattern::match::copy should copy in same store (again...)
 - change permutation_equals (returning bool) to permutation_compare returning one of {match, smaller, larger, unordered} -> binary search for math types?
       if compares unordered, it should clean up after itself, not requiring caller calling reset_own_matches
	   also requires find_matching_permutation to return same values.
	   also: is unordered possible?
 - seperate match and replace into multiple functions
 - write data structure one abstraction layer above RewriteRule (RuleSet) to group multiple pattens
 - restructure everything to use modules (basically needed to constexprfy all the things)
 - allow to restrict a variables possibility space e.g. natural, integer, real, complex (split Literal::variable in own enum?)

nice to have:
 - helper type for SumEnum to allow more course switch case: group whole SumEnums to single helper value -> switch ofer helper values
 - use templates to allow multiple stores
 - avoid copying pattern::MultiPn entries (needs to know original storage location of each entry, to steal it from there)
 - noexceptify everything

idea status:
 - add type system: check only when pattern /usual term instanciates, not needed when copied
 - introduce "intrinsic" type and use index in intrinsic typed_idx to specify witch (have all stored at central function pointer array)
 - add a pattern variable type catching functions with other pattern variables beeing parameters of these functions (perhaps two types, one parameters?)
 - add concept of "easy pattern" with every pattern variable occuring only once, match these in guaranteed linear time
 - const term_slc iterator dereferences to Ref, not typed_idx
 - ctor of RewriteRule: regroup sums / products if nessecairy to allow value match variables to better catch values
 - let each RewriteRule have a name in form of a constant c-string (not what pattern is constructed from, but name of rule, e.g. "differentiation: product rule" or something)
 - prettify pattern::Multivar sytax to require "..." after variable name 
 - always keep -1 at some known index in store and never allocate new -1 in build_negated and build_inverted (problem: identify bevore copy)
*/

struct A :SingleSumEnumEntry {};
struct B :SingleSumEnumEntry {};
using AB = SumEnum<A, B>;
struct C :SingleSumEnumEntry {};
enum class Num { one, two, three, COUNT };
using Enum = SumEnum<Num, AB, C>;

/*
	vorgehen:
	1. sortiere Liste von EnumSwitch nach reihenfolge in enum_detail::ListEnums 
		-> position in sortierter Liste ist Value von Typ
	2. baue liste von Paaren mit Typ und assoziiertem wert (nach wie vor sortiert nach Typ)
	2. halbiere Paar-Liste bei COUNT / 2 und teste mit if wert größer / kleinergleich mitte in decide -> rekursionsaufruf
*/

void f(Enum e) {
	using Switch = EnumSwitch<Enum, meta::List<Num, AB, C>>;

	switch (Switch::decide(e)) {
	case Switch::as<Num>:
		std::cout << "Num\n";
		break;
	case Switch::as<AB>:
		std::cout << "AB\n";
		break;
	case Switch::as<C>:
		std::cout << "C\n";
		break;
	}
}

int main()
{
	f(Num::one);
	f(Num::two);
	f(Num::three);
	f(A{});
	f(B{});
	f(C{});
	//debug::enumerate_type();
	//debug::test_rechner();
	//test::combine_exact();
	//test::pattern_term();
	//test::arithmetic_term();
	//test::copy();
	//test::bit_set();
	//test::match();
	//test::combine();
	//test::alloc_n();
	//test::term_array();
}








