#include "euler.h"
#include <cmath>
#include <map>

namespace euler{

    bool is_prime( size_t n ) {
        if ( n == 2 or n == 3 ){
            return true;
        }

        const auto limit = sqrt( n );

        for( int i = 3 ; i <= limit ; i+=2 ){
            if( n % i == 0 ){
                return false;
            }
        }

        return true;
    }

    struct PrimeCandidateState{
        size_t k = 0;
        size_t r_idx = 0;
    };

    size_t next_prime( PrimeCandidateState & state ){
        static const size_t r_table[] = { 1, 5 };
        for(;;){
            int p = state.k * 6 + r_table[ state.r_idx ];
            state.r_idx = not state.r_idx;
            if( state.r_idx == 0 ){
                ++state.k;
            }

            if( euler::is_prime(p) ){
                return p;
            }
        }
    }


    std::vector< std::pair<int, int> > prime_decomp( size_t n ){
        std::map< int, int > decomp;

        while( n % 2 == 0 ){
            n = n / 2;
            decomp[ 2 ] = decomp[ 2 ] + 1;
        }

        while( n % 3 == 0 ){
            n = n / 3;
            decomp[ 3 ] = decomp[ 3 ] + 1;
        }

        if( n > 1 ){
            PrimeCandidateState state = { 0, 1 };

            size_t p = next_prime( state );
            do{
                if( n % p == 0 ){
                    n = n / p;
                    decomp[ p ] = decomp[ p ] + 1;
                } else {
                    p = next_prime( state );
                }
            }while( n > 1 );
        }

        std::vector< std::pair<int, int> > result;
        result.reserve( decomp.size() );

        for( const auto & v: decomp ){
            result.emplace_back( v.first, v.second );
        }

        return result;
    }


    /*
      En general sigma_x(n) es definida como la suma de la x-esima potencia de
      los divisores positivos de n

      sigma0 es el numero de los divisores de n === d(n)
     */
    int sigma0( const int n ) {
        const auto divisors = prime_decomp( n );

        int mul = 1;
        for( const auto & d: divisors ){
            mul *= d.second + 1;
        }

        return mul;
    }

    int num_divisors( const int n ) {
        return sigma0( n );
    }

    bool is_palindrome( const std::string_view str ){
        const auto len = str.size();
        const auto limit = (len / 2) + 1;
        for( int i = 0 ; i < limit ; ++i ){
            if( str[i] != str[len - 1 - i] ){
                return false;
            }
        }

        return true;
    }

}//euler
