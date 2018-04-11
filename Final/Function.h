#pragma once

#include <iostream>
#include <thread>
#include <type_traits>
#include <math.h>
#include <mutex>
#include <vector>
#include <utility>
#include <chrono>

template <typename Number>
class Function {
    static_assert(std::is_floating_point<Number>::value, "Function only supports floating points.");
private:
    std::vector<Number> args;
    std::vector<std::thread> threads;
    
    std::mutex output_lock;
    std::mutex thread_lock;
    std::mutex args_lock;
    
    std::atomic_int live_threads;
    std::atomic_bool started;
    
    Number operator()(Number input) {
        Number output = 0;
        
        int exp = args.size() - 1;
        for (Number arg : args) {
            output += arg * pow(input, exp--);
        }
        
        return output;
    }
    
    void compute(const Number a, const Number b, const Number tol, Number& I) {
        ++live_threads;
        if (!started) started = true;
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
        
        if (abs(I1 - I2) < 3 * (b - a)*tol) {
            std::lock_guard<std::mutex> guard(output_lock);
            I += I2;
        }
        else {
            std::thread left(&Function::compute, this, a, m, tol, std::ref(I));
            std::thread right(&Function::compute, this, m, b, tol, std::ref(I));
            
            std::lock_guard<std::mutex> guard(thread_lock);
            threads.push_back(std::move(left));
            threads.push_back(std::move(right));
        }
        --live_threads;
    }
    
public:
    Function(const std::initializer_list<Number> arguments) : args(arguments) {}

    Number integrate(const Number lower, const Number upper, const Number tolerance) {
        Number I = 0;
        live_threads = 0;
        started = false;
        
        std::thread start(&Function::compute, this, lower, upper, tolerance, std::ref(I));
        threads.push_back(std::move(start));

        while (live_threads || !started) {
            std::this_thread::sleep_for(std::chrono::milliseconds(live_threads));
        }
        
        for (auto& t : threads) t.join();

        return I;
    }
    
};
