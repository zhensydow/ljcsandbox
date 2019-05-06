// -*- compile-command: "g++ -std=c++17 -O3 -o problem problem004.cpp euler.cpp" -*-
#include <cstdio>
#include "euler.h"

int main(){

    size_t max = 0;

    for( size_t i = 999 ; i >= 100 ; --i ){
        for( size_t j = 999 ; j >= 100 ; --j ){
            const auto str = std::to_string( i*j );
            if( euler::is_palindrome( str ) ){
                if( i*j > max ){
                    max = i*j;
                    printf( "%s\n", str.data() );
                }
            }
        }
    }

    return 0;
}
