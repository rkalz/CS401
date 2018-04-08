// CS401Final.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "Function.h"


int main()
{
	std::vector<double> params = { 6, 5, 1 };
	Function<double> f(params);

	auto startMulti = std::chrono::high_resolution_clock::now();
	auto multi = f.integrate(2, 5, 0.01);
	auto endMulti = std::chrono::high_resolution_clock::now();
	auto diffMulti = std::chrono::duration_cast<std::chrono::milliseconds>(endMulti - startMulti).count();

	auto startSingle = std::chrono::high_resolution_clock::now();
	auto single = f.integrateSingle(2, 5, 0.01);
	auto endSingle = std::chrono::high_resolution_clock::now();
	auto diffSingle = std::chrono::duration_cast<std::chrono::milliseconds>(endSingle - startSingle).count();

	std::cout << "Single Threaded: " << single << " (";
	if (!diffSingle) std::cout << "<1 ms)" << std::endl;
	else std::cout << single << " ms" << std::endl;
	std::cout << "Multi Threaded: " << multi.first << " (" << diffMulti << " ms/" << multi.second << " threads (" << diffMulti / multi.second << "ms/thread))" << std::endl;

	system("pause");
	return 0;
}

