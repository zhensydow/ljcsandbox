// -*- compile-command: "g++ -std=c++17 -O3 -o problem problem008.cpp euler.cpp" -*-
#include <cstdio>
#include <cmath>
#include "euler.h"

int main(){

    int count = 0;
    const auto limit = (1000/3) + 1;
    for( int a = 1 ; a < 1000 ; ++a ){
        for( int b = a + 1; b < 1000 ; ++b ){
            int c = 1000 - a - b;

            ++count;
            if( (a*a + b*b) == c*c ){
                printf( "%d %d %d = %d\n", a, b, c, a*b*c );
                return 0;
            }
        }
    }
    return 0;
}
