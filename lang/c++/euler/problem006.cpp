// -*- compile-command: "g++ -std=c++17 -O3 -o problem problem006.cpp euler.cpp" -*-
#include <cstdio>
#include <cmath>
#include "euler.h"

int main(){
    size_t sum_sq = 0;
    size_t sum = 0;
    for( int i = 1 ; i <= 100 ; ++i ){
        sum_sq += i*i;
        sum += i;
    }

    printf( "%ld %ld %ld = %ld", sum_sq, sum, sum*sum, sum*sum - sum_sq );
    return 0;
}
