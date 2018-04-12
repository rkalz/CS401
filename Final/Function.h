#pragma once

#include <iostream>
#include <thread>
#include <type_traits>
#include <math.h>
#include <mutex>
#include <chrono>

template <typename Number>
static void compute(const std::function<Number(const Number)> func, const double a, const double b, const double tol, double& I, std::atomic_int& live_threads, std::mutex& output_lock) {
    double m = (a+b)/2;
    double f_a = func(a);
    double f_b = func(b);
    double f_m = func(m);
    
    double I1 = ((b - a) / 2)*(f_a + f_b);
    double I2 = ((b - a) / 4)*(f_a + 2 * f_m + f_b);
    
    if (abs(I1 - I2) < 3 * (b - a) * tol) {
        {
            std::lock_guard<std::mutex> guard(output_lock);
            I += I2;
        }
        --live_threads;
    }
    else {
        std::thread(compute<Number>, func, a, m, tol, std::ref(I), std::ref(live_threads), std::ref(output_lock)).detach();
        ++live_threads;
        compute(func, m, b, tol, I, live_threads, output_lock);
    }
}

template <typename Number>
static Number integrate(const std::function<Number(const Number)> func, const Number lower, const Number upper, const Number tol) {
    static_assert(std::is_integral<Number>::value || std::is_floating_point<Number>::value, "Type must be numeric\n");
    
    double I = 0;
    std::atomic_int live_threads;
    live_threads = 0;
    
    std::mutex output_lock;
    
    std::thread(compute<Number>, func, lower, upper, tol, std::ref(I), std::ref(live_threads), std::ref(output_lock)).detach();
    ++live_threads;
    
    while (live_threads) {
        std::this_thread::sleep_for(std::chrono::milliseconds(live_threads));
    }
    
    return I;
}
