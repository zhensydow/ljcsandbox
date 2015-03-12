// g++ version >= 4.9.x
#include <regex>
#include <iostream>
#include <fstream>

//------------------------------------------------------------------------------
int main(){
    std::regex decimal_pat( "[0-9]+" );
    
    std::ifstream f( "ex03.cpp" );

    std::smatch matches;
    for( std::string line ; std::getline( f, line ) ; ){
        std::regex_search( line, matches, decimal_pat );
        std::cout << "line: " << line << std::endl;
        for( auto i = 0u ; i < matches.size() ; ++i ){
            std::cout << matches[i] << std::endl;
        }
    }
}

//------------------------------------------------------------------------------
