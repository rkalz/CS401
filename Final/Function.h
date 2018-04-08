#pragma once

#include "stdafx.h"

template <typename Number>
class Function {
	static_assert(std::is_integral<Number>::value || std::is_floating_point<Number>::value, "Argument must be numeric.");
private:
	std::vector<Number> args;

	Number operator()(Number input) {
		Number output = 0;

		for (int i = 0; i < args.size(); ++i) {
			output += args[i] * pow(input, i);
		}

		return output;
	}

	void compute(const Number a, const Number b, const Number tol, Number& I, std::mutex& lock, unsigned int& count) {
		Number f_a = (*this)(a);
		Number f_b = (*this)(b);

		Number I1 = ((b - a) / 2)*(f_a + f_b);

		Number m = (a + b) / 2;
		Number f_m = (*this)(m);

		Number I2 = ((b - a) / 4)*(f_a + 2 * f_m + f_b);

		if (abs(I1 - I2) < 3 * (b - a)*tol) {
			lock.lock();
			I += I2;
			lock.unlock();
		}
		else {
			lock.lock();
			count += 2;
			lock.unlock();

			std::thread left(&Function::compute, this, std::cref(a), std::cref(m), std::cref(tol), std::ref(I), std::ref(lock), 
				std::ref(count));
			std::thread right(&Function::compute, this, std::cref(m), std::cref(b), std::cref(tol), std::ref(I), std::ref(lock), 
				std::ref(count));

			left.join();
			right.join();
		}
	}
public:
	Function(const std::vector<Number>& arguments) {
		args = arguments;
	}

	Number integrateSingle(const Number lower, const Number upper, const Number tolerance) {
		Function f = *this;
		
		std::stack<std::pair<Number, Number>> stack;
		stack.push(std::pair<Number, Number>(lower, upper));
		Number I = 0;

		while (!stack.empty()) {
			auto pair = stack.top();
			stack.pop();
			Number a = pair.first;
			Number f_a = f(a);
			Number b = pair.second;
			Number f_b = f(b);

			Number I1 = ((b - a) / 2)*(f_a + f_b);
			
			Number m = (a + b) / 2;
			Number f_m = f(m);

			Number I2 = ((b - a) / 4)*(f_a + 2 * f_m + f_b);

			if (abs(I1 - I2) < 3 * (b - a)*tolerance) {
				I += I2;
			}
			else {
				stack.push(std::pair<Number, Number>(a, m));
				stack.push(std::pair<Number, Number>(m, b));
			}
		}

		return I;
	}

	std::pair<Number, unsigned int> integrate(const Number lower, const Number upper, const Number tolerance) {
		Number I = 0;
		std::mutex lock;

		unsigned int count = 1;
		std::thread start(&Function::compute, this, std::cref(lower), std::cref(upper), std::cref(tolerance), std::ref(I), std::ref(lock), std::ref(count));
		start.join();

		return std::pair<Number, unsigned int>(I, count);
	}

};

