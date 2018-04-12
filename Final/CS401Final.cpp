#include "Function.h"

// Windows: Add to VS Project
// Mac/Linux: clang++ -std=c++11 CS401Final.cpp -o CS401Final (make sure Function.h is in the same folder)
// Linux: g++ -std=c++11 CS401Final.cpp -o CS401Final (make sure Function.h is in the same folder)

// Instructions: Create a const std::function<Number(const Number)>
// Change variables of integrate to represent the lower and upper bounds and the tolerance

int main()
{
    const std::function<double(const double)> func = [](const double input) { return pow(input, 2) + 2*input + 1;};
    
    std::cout << integrate<double>(func, 2, 5, 0.01) << std::endl;
    
#ifdef _WIN32 || WIN32
    system("pause");
#endif
    return 0;
}
