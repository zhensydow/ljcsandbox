// -*- compile-command: "g++ -std=c++17 -O3 -o problem problem012.cpp euler.cpp" -*-
#include <cstdio>
#include <cmath>
#include "euler.h"

int main(){

    size_t i = 1;
    size_t triangle = 1;
    size_t divs = euler::num_divisors(triangle);

    while( divs < 500 ){
        ++i;
        triangle += i;
        divs = euler::num_divisors( triangle );
    }

    printf( "%ld %ld\n", triangle, divs );

    return 0;
}
