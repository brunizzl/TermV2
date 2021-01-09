
#include "stupidTests.hpp"
using namespace bmath::intern;

/*
TODO:

important:
 - change permutation_equals (returning bool) to permutation_compare returning one of {match, smaller, larger, unordered} -> binary search for math types?
       if compares unordered, it should clean up after itself, not requiring caller calling reset_own_matches
	   also requires find_matching_permutation to return same values.
	   also: is unordered possible?
 - seperate match and replace into multiple functions
 - add concept of "easy pattern" with every pattern variable occuring only once, match these in guaranteed linear time
 - use templates to allow multiple stores
 - write data structure one abstraction layer above RewriteRule to group multiple pattens
 - restructure everything to use modules (basically needed to constexprfy all the things)
 - allow to restrict a variables possibility space e.g. natural, integer, real, complex (split Literal::variable in own enum?)

nice to have:
 - make pattern extra type (again...) and add extra info to function types:
     index in MatchData, for each param "rematchable" and "reset required" info, perhaps for each param if dependent on others?
 - find a way to give every variadic in pattern an index (without skipping any indices), then store index table in match_data and acess variadic_datum by index,
     then get rid of MultimatchData and let the index of MultiPn refer to the owning variadic_datum (thus the variadic_datum also needs to know the variadic it matches)
 - introduce "intrinsic" type and use index in intrinsic typed_idx to specify witch (have all stored at central function pointer array)
 - add a pattern variable type catching functions with other pattern variables beeing parameters of these functions (perhaps two types, one parameters?)
 - match::permutation_equals: abort earlier (use fact that both terms are (assumed to be) sorted, not only pattern)
 - avoid copying pattern::MultiPn entries (needs to know original storage location of each entry, to steal it from there)
 - seperate utility in different files, create utility folder
 - noexceptify everything

idea status:
 - const term_slc iterator dereferences to Ref, not typed_idx
 - ctor of RewriteRule: regroup sums / products if nessecairy to allow value match variables to better catch values
 - TermSLC: only allow null_value at end -> SLC_Iterator becomes  simpler (meaning some functions in current form may produce invalid results)
 - let each RewriteRule have a name in form of a constant c-string (not what pattern is constructed from, but name of rule, e.g. "differentiation: product rule" or something)
 - prettify pattern::Multivar sytax to require (and allow) "..." after variable name 
 - always keep -1 at some known index in store and never allocate new -1 in build_negated and build_inverted (problem: identify bevore copy)
*/

struct Mercedes {};
struct BMW {};
struct VW {};
struct Audi {};

enum class Test { eins, zwei, drei };

using Auto = SumEnum<Mercedes, BMW, VW, Audi>;

void f(const Auto a)
{
	switch (a) {
		case Auto::as<Mercedes>:
		std::cout << "mercedes\n";
		break;
	case Auto::as<BMW>:
		std::cout << "bmw\n";
		break;
	case Auto::as<VW>:
		std::cout << "vw\n";
		break;
	case Auto::as<Audi>:
		std::cout << "audi\n";
		break;			
	}
}

static_assert(enum_detail::InstanceOf<Auto, SumEnum>);

int main()
{
	f(Mercedes{});
	f(BMW{});
	f(VW{});
	f(Audi{});
	debug::enumerate_type();
	debug::test_rechner();
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








