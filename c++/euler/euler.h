#pragma once

#include <memory>
#include <cstdint>
#include <string_view>
#include <vector>

namespace euler {

    bool is_prime( size_t n );

    std::vector< std::pair<int, int> > prime_decomp( size_t n );

    int num_divisors( const int n );

    bool is_palindrome( const std::string_view str );

}//euler
