// -*- compile-command: "g++ -std=c++17 -O3 -o problem problem014.cpp euler.cpp" -*-
#include <cstdio>
#include <cmath>
#include "euler.h"

std::vector<int> cache( 1000000, -1 );

int collatz_size( size_t n ){
    if( n < (cache.size() + 1) and (cache[ n - 1 ] != -1) ){
        return cache[ n - 1 ];
    }

    int n0;

    if( n % 2 == 0 ){
        n0 = collatz_size( n / 2 );
    } else {
        n0 = collatz_size( 3*n + 1);
    }

    if( n < (cache.size() + 1) ){
        cache[ n - 1 ] = 1 + n0;
    }
    return 1 + n0;
}

int main(){
    cache[ 0 ] = 1;

    int max = 0;
    int max_i = 0;
    for( int i = 1 ; i < 1000000 ; ++i ){
        const auto l = collatz_size( i );
        if( l > max ){
            max = l;
            max_i = i;
        }
    }

    printf( "max %d %d\n", max, max_i );

    return 0;
}
