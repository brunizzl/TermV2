
#include "stupidTests.hpp"
using namespace bmath::intern;

enum class AutoMarke
{
	mercedes, bmw, audi, COUNT
};

enum class MotorradMarke
{
	honda, ducati, COUNT
};

using FahrzeugMarke = SumEnum<AutoMarke, MotorradMarke>;

void f(FahrzeugMarke m)
{
	switch (m) {
	case FahrzeugMarke(AutoMarke::bmw):
		std::cout << "sweeter schlitten!\n";
		break;
	case FahrzeugMarke(MotorradMarke::ducati):
		std::cout << "huebsches motorrad!\n";
		break;
	default:
		if (m.is<AutoMarke>()) {
			std::cout << "meh, bmw ist besser.\n";
		}
		else {
			assert(m.is<MotorradMarke>());
			std::cout << "meh, ducati ist besser.\n";
		}
		break;
	}
}

enum class Handy
{
	samsung, apple, COUNT
};

using Marke = SumEnum<Handy, FahrzeugMarke>;

void g(Marke m)
{
	if (m.is<Handy>());
	if (m.is<AutoMarke>());
}



int main()
{

	//f(AutoMarke::bmw);
	//f(MotorradMarke::honda);
	//f(MotorradMarke::ducati);
	//test::combine_exact();
	//test::arithmetic_term();
	test::pattern_term();
	//test::copy();
}


