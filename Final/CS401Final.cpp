#include "Function.h"

// Windows: Add to VS Project
// Mac/Linux: clang++/g++ -std=c++11 CS401Final.cpp -o CS401Final (make sure Function.h is in the same folder)

// Instructions: Edit the input of f to match the desired single varaible polynomial function
// i.e. x^2 + 4x + 4 = {1, 4, 4}
// Change variables of integrate to represent the lower and upper bounds and the tolerance
// i.e. f.integrate(2, 5, 0.01) integrates f from 2 to 5 with a tolerance of 0.01.

int main()
{
	Function<double> f({ 1,4,4 });
	std::cout << f.integrate(2, 5, 0.01) << std::endl;

#ifdef _WIN32 || WIN32
	system("pause");
#endif
	return 0;
}
