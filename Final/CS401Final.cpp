#include "Function.h"

// Windows: Add to VS Project
// Mac/Linux: Use the Makefile

// Instructions: Create a const std::function<Number(const Number)>
// Change variables of integrate to represent the lower and upper bounds and the tolerance

int main()
{
    const std::function<double(const double)> func = [](const double input) {
        double output = 0;
        for (int i = 11; i >= 0; --i) output += i*pow(input,i);
        return output;
    };
    
    std::cout << integrate<double>(func, 2, 5, 100) << std::endl;
    return 0;
}