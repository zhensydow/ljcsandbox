//------------------------------------------------------------------------------
#include <iostream>
#include <functional>

//------------------------------------------------------------------------------
template<typename A, typename B>
struct Either{
    Either( const A & a, const B & b, bool flag )
        : m_a{a}, m_b{b}, m_first{flag} {};

    A m_a;
    B m_b;

    bool m_first = false;
};

template <typename A, typename B>
using Cell = std::function<Either<A,B> (A,B)>;

template<typename A, typename B>
Either<A,B> car( std::function< Either<A,B>(Cell<A,B>) > m ){
    return m( [](A a, B b){ return Either<A,B>( a, b, true ); } );
}

template<typename A, typename B>
Either<A,B> cdr( std::function< Either<A,B>(Cell<A,B>) > m ){
    return m( [](A a, B b){ return Either<A,B>( a, b, false ); } );
}

template<typename A, typename B>
std::function< Either<A,B>(Cell<A,B>) >
cons( const A a, const B b){
    return [=](Cell<A,B> l){ return l(a,b); };
}

template<typename A, typename B>
std::ostream& operator<<( std::ostream& stream, const Either<A,B>& val ){
    // if( val.m_first ){
    stream << val.m_a;
    // }else{
    //     stream << val.m_b;
    // }
}

//------------------------------------------------------------------------------
int main(){
    auto l = cons( 1, cons( 2, 3 ) );  // no matching function
    //auto l = cons( 1, 2 );

    std::cout << car( l ) << ", " << car( cdr( l ) ) << std::endl;

    return 0;
}

//------------------------------------------------------------------------------
