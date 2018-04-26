#pragma once

#include <atomic>
#include <chrono>
#include <iostream>
#include <math.h>
#include <mutex>
#include <thread>
#include <type_traits>

template <typename Number>
static void compute(const std::function<Number(const Number)> func, const double a, const double b, const double tol, double& I, std::atomic_int& live_threads, std::mutex& lock, std::atomic_bool& has_failed) {
    double m = (a+b)/2;
    double f_a = func(a);
    double f_b = func(b);
    double f_m = func(m);
    
    double I1 = ((b - a) / 2)*(f_a + f_b);
    double I2 = ((b - a) / 4)*(f_a + 2 * f_m + f_b);
    
    if (abs(I1 - I2) < 3 * (b - a) * tol) {
        {
            try {
                std::lock_guard<std::mutex> guard(lock);
            } catch(...) {
                --live_threads;
                return;
            }
            I += I2;
        }
        --live_threads;
    }
    else {
        try {
        std::thread(compute<Number>, func, a, m, tol, std::ref(I), std::ref(live_threads), std::ref(lock), std::ref(has_failed)).detach();
        } catch(...) {
            --live_threads;
            has_failed = true;
            return;
        }
        ++live_threads;
        compute(func, m, b, tol, I, live_threads, lock, has_failed);
    }
}

template <typename Number>
static Number integrate(const std::function<Number(const Number)> func, const Number lower, const Number upper, const Number tol) {
    static_assert(std::is_integral<Number>::value || std::is_floating_point<Number>::value, "Type must be a number");
    
    double I = 0;
    std::atomic_int live_threads;
    std::atomic_bool has_failed;
    live_threads = 0;
    has_failed = false;
    
    std::mutex lock;
    
    std::thread(compute<Number>, func, lower, upper, tol, std::ref(I), std::ref(live_threads), std::ref(lock), std::ref(has_failed)).detach();
    ++live_threads;
    
    while (live_threads) {
        if (has_failed) {
            std::cout << "Thread limit reached: Try increasing the tolerance." << std::endl;
            return 0;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(live_threads));
    }
    
    return I;
}