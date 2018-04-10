#pragma once

#include <iostream>
#include <thread>
#include <type_traits>
#include <math.h>
#include <mutex>
#include <vector>
#include <stack>
#include <utility>

template <typename Number>
class Function {
    static_assert(std::is_floating_point<Number>::value, "Function only supports floating points.");
private:
    std::vector<Number> args;
    std::mutex lock;
    
    Number operator()(Number input) {
        Number output = 0;
        
        int exp = args.size() - 1;
        for (Number arg : args) {
            output += arg * pow(input, exp--);
        }
        
        return output;
    }
    
    void compute(const Number a, const Number b, const Number tol, Number& I) {
        Number f_a = (*this)(a);
        Number f_b = (*this)(b);
        
        Number I1 = ((b - a) / 2)*(f_a + f_b);
        
        Number m = (a + b) / 2;
        Number f_m = (*this)(m);
        
        Number I2 = ((b - a) / 4)*(f_a + 2 * f_m + f_b);
        
        if (abs(I1 - I2) < 3 * (b - a)*tol) {
            std::lock_guard<std::mutex> guard(lock);
            I += I2;
        }
        else {
            std::thread left(&Function::compute, this, std::cref(a), std::cref(m), std::cref(tol), std::ref(I));
            std::thread right(&Function::compute, this, std::cref(m), std::cref(b), std::cref(tol), std::ref(I));
            
            left.join();
            right.join();
        }
    }
public:
    Function(const std::initializer_list<Number> arguments) {
        args = std::vector<Number>(arguments);
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
    
    Number integrate(const Number lower, const Number upper, const Number tolerance) {
        Number I = 0;

        std::thread(&Function::compute, this, std::cref(lower), std::cref(upper), std::cref(tolerance), std::ref(I)).join();
        
        return I;
    }
    
};
