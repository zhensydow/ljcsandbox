// -*- compile-command: "g++ -std=c++17 -O3 -o problem problem007.cpp euler.cpp" -*-
#include <cstdio>
#include "euler.h"

int main(){
    int i = 2;
    size_t p = 3;
    size_t n = p+2;
    while( i < 10001 ){
        if( euler::is_prime( n ) ){
            ++i;
            p = n;
        }
        n += 2;
    }

    printf( "%d %ld\n", i, p );

    return 0;
}
