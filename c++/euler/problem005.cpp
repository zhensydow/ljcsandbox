// -*- compile-command: "g++ -std=c++17 -O3 -o problem problem005.cpp euler.cpp" -*-
#include <cstdio>
#include <cmath>
#include <map>
#include "euler.h"

int main(){
    size_t max = 0;
    std::map< int, int > sum;

    for( int i = 2 ; i <= 20 ; ++i ){
        const auto res = euler::prime_decomp( i );
        for( const auto & p: res ){
            sum[ p.first ] = std::max( sum[ p.first ], p.second );
        }
    }

    size_t mul = 1;
    for( const auto & p: sum ){
        mul = mul * pow( p.first, p.second );
    }

    printf( "%ld\n", mul );

    return 0;
}
