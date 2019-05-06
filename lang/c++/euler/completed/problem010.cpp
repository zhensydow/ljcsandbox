// -*- compile-command: "g++ -std=c++17 -O3 -o problem problem010.cpp euler.cpp" -*-
#include <cstdio>
#include "euler.h"

int main(){\
    size_t sum = 2;
    size_t n = 3;
    while( n < 2000000 ){
        if( euler::is_prime( n ) ){
            sum += n;
        }
        n += 2;
    }

    printf( "%ld %ld\n", n, sum );

    return 0;
}
