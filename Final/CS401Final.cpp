// CS401Final.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "Function.h"


int main()
{
	Function<double> f({ 1,4,4 });
	std::cout << f.integrate(2, 5, 0.01) << std::endl;

	system("pause");
	return 0;
}

