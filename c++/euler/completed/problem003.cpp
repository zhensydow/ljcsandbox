// -*- compile-command: "g++ -std=c++17 -O3 -o problem problem003.cpp" -*-
#include <cstdio>
#include <vector>
#include <cmath>

bool is_prime( size_t n ){
    const auto limit = sqrt( n );

    for( int i = 2 ; i < limit ; ++i ){
        if( n % i == 0 ){
            return false;
        }
    }

    return true;
}

std::vector<size_t> prime_factors( size_t n ){
    std::vector<size_t> factors;

    const auto limit = sqrt( n );

    for( int i = 2 ; i < limit ; ++i ){
        if( n % i == 0 and is_prime(i) ){
            factors.push_back( i );
        }
    }

    return factors;
}

int main(){
    const auto factors = prime_factors( 600851475143 );

    for( const auto f: factors ){
        printf( "%ld, ", f );
    }

    printf( "%ld\n", factors.back() );

    return 0;
}
