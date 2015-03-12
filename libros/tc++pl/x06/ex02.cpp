#include <iostream>
#include <chrono>

//------------------------------------------------------------------------------
long int calcValue( int n ){
    long int a = 0;
    long int b = 1;

    for( auto i = 0 ; i < n ; ++i ){
        auto tmp = a + b;
        a = b;
        b = tmp;
    }

    return a;
}

//------------------------------------------------------------------------------
int main(){
    auto t0 = std::chrono::high_resolution_clock::now();
    auto a = calcValue( 50 );
    auto t1 = std::chrono::high_resolution_clock::now();

    std::cout << a << std::endl;

    auto delta = std::chrono::duration_cast<std::chrono::nanoseconds>( t1 - t0 ).count();
    std::cout << delta << " nanoseconds passed\n";

}

//------------------------------------------------------------------------------
