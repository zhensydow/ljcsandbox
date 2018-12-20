// -*- compile-command: "g++ -Wall -Wextra -std=c++17 -O3 -o colemak colemak.cpp" -*-
#include <cstdlib>
#include <cstdio>
#include <map>
#include <set>
#include <sstream>
#include <fstream>
#include <string>
#include <string_view>
#include <algorithm>

using Dict = std::set<std::string>;
using KeyboardFun = std::map<char, char>;
using Change = std::pair< bool, std::string >;

std::string str_tolower( std::string s ){
    std::transform( s.begin(), s.end(), s.begin(),
                    []( unsigned char c ){
                        return std::tolower(c);
                    });
    return s;
}

Change change_word( const KeyboardFun & kf, std::string w ){
    for( auto & c: w ){
        const auto it = kf.find( c );
        if( it != kf.end() ){
            c = it->second;
        }else{
            return { false, "" };
        }
    }

    return { true, w };
}

bool valid_word( const Dict & dict, const Change & res ){
    return res.first and (dict.count(res.second) > 0);
}

KeyboardFun load_keyboard( std::string filename ){
    KeyboardFun keyf;

    std::ifstream infile;
    infile.open( filename );

    std::string line;
    while( std::getline(infile, line) ){
        std::istringstream iss(line);
        char a, b;
        if( not (iss >> a >> b) ){
            break;
        }

        keyf.emplace( a, b );
    }

    return keyf;
}

Dict load_dictionary( std::string filename ){
    Dict dict;

    std::ifstream infile;
    infile.open( filename );

    std::string line;
    while( std::getline(infile, line) ){
        if( line.size() > 1 ){
            dict.insert( str_tolower(line) );
        }
    }

    return dict;
}

int main(){
    const auto keyf = load_keyboard( "colemak_qwerty.txt" );
    const auto dict = load_dictionary( "english.txt" );

    for( const auto & w : dict ){
        const auto res = change_word( keyf, w );
        if( valid_word( dict, res ) ){
            printf( "'%s' -> '%s'\n", w.data(), res.second.data() );
        }
    }

    return EXIT_SUCCESS;
}
