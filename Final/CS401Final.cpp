#include "Function.h"

// Windows: Add to VS Project
// Mac: clang++ -std=c++11 CS401Final.cpp -o CS401Final (make sure Function.h is in the same folder)
// Linux: g++ -std=c++11 CS401Final.cpp -o CS401Final (make sure Function.h is in the same folder)

// Instructions: Edit the input of f to match the desired single varaible polynomial function
// i.e. x^2 + 4x + 4 = {1, 4, 4}
// Change variables of integrate to represent the lower and upper bounds and the tolerance
// i.e. f.integrate(2, 5, 0.01) integrates f from 2 to 5 with a tolerance of 0.01.
// Note: make sure to increase the tolerance with large function. Failing to do so will result in quickly reaching the thread limit.

int main()
{
    std::cout << Function<double>({1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}).integrate(2, 5, 100) << std::endl;
    
#ifdef _WIN32 || WIN32
    system("pause");
#endif
    return 0;
}
