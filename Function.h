#pragma once

#include <iostream>
#include <thread>
#include <type_traits>
#include <math.h>
#include <mutex>
#include <vector>
#include <chrono>

template <typename Number>
class Function {
    static_assert(std::is_floating_point<Number>::value, "Function only supports floating points.");
private:
    std::vector<Number> coefs;
    
    std::mutex output_lock;
    std::mutex args_lock;
    
    std::atomic_int live_threads;
    
    Number operator()(Number input){
        Number output = 0;
        
        int exp = coefs.size() - 1;
        for (Number arg : coefs) {
            output += arg * pow(input, exp--);
        }
        
        return output;
    }
    
    void compute(const Number a, const Number b, Number tol, Number& I) {
        Number m = (a + b) / 2;
        Number f_a, f_b, f_m;
        
        {
            std::lock_guard<std::mutex> guard(args_lock);
            f_a = (*this)(a);
            f_b = (*this)(b);
            f_m = (*this)(m);
        }

        Number I1 = ((b - a) / 2)*(f_a + f_b);
        Number I2 = ((b - a) / 4)*(f_a + 2 * f_m + f_b);
        
        if (abs(I1 - I2) < 3 * (b - a) * tol) {
            {
                std::lock_guard<std::mutex> guard(output_lock);
                I += I2;
            }
            --live_threads;
        }
        else {
            std::thread(&Function::compute, this, a, m, tol, std::ref(I)).detach();
            ++live_threads;
            compute(m, b, tol, I);
        }
    }
    
public:
    Function(const std::initializer_list<Number> arguments) : coefs(arguments) {}

    Number integrate(const Number lower, const Number upper, const Number tolerance) {
        Number I = 0;
        live_threads = 0;
        
        std::thread(&Function::compute, this, lower, upper, tolerance, std::ref(I)).detach();
        ++live_threads;

        while (live_threads) {
            std::this_thread::sleep_for(std::chrono::milliseconds(live_threads));
        }

        return I;
    }
    
};
