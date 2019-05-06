#include <vector>
#include <iostream>
#include <random>
#include <chrono>

template< typename G, typename T >
void shuffle_vector1( G gen, std::vector<T> & v ){
    for( unsigned i = v.size() ; i > 0 ; --i ){
        std::uniform_int_distribution<unsigned> dis(0, i-1);
        auto j = dis(gen);
        if( j != i-1 ){
            std::swap( v[i-1], v[j] );
        }
    }
}

template< typename G, typename T >
void shuffle_vector2( G gen, std::vector<T> & v ){
    for( unsigned i = 0, n = v.size() ; i < n - 1 ; ++i ){
        std::uniform_int_distribution<unsigned> dis(0, n - 1 - i);
        auto j = i + dis(gen);
        if( j != i ){
            std::swap( v[i], v[j] );
        }
    }
}

template< typename T >
void print_vector( const std::vector<T> & v ){
    for( auto val: v ){
        std::cout << val << " ";
    }
    std::cout << "\n";
}

int main(){
    std::random_device rd;
    std::mt19937 gen(rd());

    std::vector<int> v(5000);

    for( unsigned i = 0, n = v.size() ; i < n ; ++i ){
        v[i] = i;
    }

    auto start = std::chrono::high_resolution_clock::now();
    for( auto i = 0 ; i < 5000 ; ++i ){
        shuffle_vector1( gen, v );
    }
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> diff = end - start;

    std::cout << "time 1 : " << diff.count() << "\n";

    start = std::chrono::high_resolution_clock::now();
    for( auto i = 0 ; i < 5000 ; ++i ){
        shuffle_vector2( gen, v );
    }
    end = std::chrono::high_resolution_clock::now();
    diff = end - start;

    std::cout << "time 2 : " << diff.count() << "\n";

    std::cout << "\n";

}
