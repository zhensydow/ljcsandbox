#include <vector>
#include <iostream>
#include <algorithm>

template<class T>
void printvector( const std::vector<T> & v ){
    for( auto i: v ){
        std::cout << i << " ";
    }
    std::cout << std::endl;
}

int main(){
    std::vector< int > ints {5,9,-1,200,0};

    printvector( ints );

    std::sort( ints.begin(), ints.end() );

    printvector( ints );

    std::vector< std::string > names {"Kant","Plato","Aristotle","Kierkegard","Hume"};

    printvector( names );

    std::sort( names.begin(), names.end() );

    printvector( names );

}
