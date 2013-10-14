/*******************************************************************************
compile:  g++ -Wall -std=c++11 -o iota iota.cpp
 */
//------------------------------------------------------------------------------
#include <list>
#include <iostream>
#include <algorithm>
#include <random>

//------------------------------------------------------------------------------
class Filler{
public:
    Filler( int a, int b );
    operator int();
    Filler & operator++();

private:
    std::mt19937 m_gen;
    std::uniform_int_distribution<> m_dis;
};

//------------------------------------------------------------------------------
Filler::Filler( int a, int b ) : m_gen(std::random_device()()), m_dis(a,b) {
    // empty
}

Filler:: operator int(){
    return m_dis(m_gen);
}

Filler & Filler::operator++(){
    return *this;
}

//------------------------------------------------------------------------------
int main(){
    std::list<int> l(10);
    std::iota(l.begin(), l.end(), Filler(1,10));

    std::cout << "Contents of the list: ";
    for(auto n: l) {
        std::cout << n << ' ';
    }
    std::cout << std::endl;
 

    return 0;
}

//------------------------------------------------------------------------------
