
#include "stupidTests.hpp"
using namespace bmath::intern;

class Int 
{ 
protected: 
	int i; 
public: 
	constexpr Int(int i_) :i(i_) {} 

	friend constexpr bool operator==(const Int&, const Int&) noexcept = default;
};

struct Integer : Int
{
	constexpr Integer(int i_) :Int(i_) {}
	friend constexpr bool operator==(const Integer&, const Integer&) noexcept = default;
};

static_assert(Integer{ 4 } != Integer{ 3 });


int main()
{
	//test::combine_exact();
	//test::arithmetic_term();
	//test::pattern_term();
	//test::copy();
	//test::bit_set();
	//debug::enumerate_pn_type();
	//test::match();
	debug::test_rechner();
}











