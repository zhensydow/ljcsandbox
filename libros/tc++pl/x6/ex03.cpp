//------------------------------------------------------------------------------
#include <vector>
#include <iostream>
#include <random>

class Rand_int{
public:
    Rand_int( int low, int high ) 
        : m_dist{ (high-low)/2.0f, 4 }
    {
        
    }

    int operator()(){
        return std::round(m_dist(m_engine));
    }

private:
    std::normal_distribution<> m_dist;
    std::default_random_engine m_engine;
};

//------------------------------------------------------------------------------
int main(){
    Rand_int rnd {0,30};
    
    std::vector<int> histogram( 30 );

    for( auto i = 0 ; i < 300 ; ++i ){
        auto val = rnd();
        if( val >= 0 and val < int(histogram.size()) ){
            ++histogram[ val ];
        }
    }
    
    for( auto i = 0u ; i < histogram.size() ; ++i ){
        std::cout << i << ": ";

        for( auto j = 0 ; j < histogram[i] ; ++j ){
            std::cout << "*";
        }

        std::cout << std::endl;
    }
}

//------------------------------------------------------------------------------
