// g++ -std=c++11 -Wall -Wextra -O2 ex02.cpp
#include <iostream>
#include <string>

int main(){
    bool b {true};
    char c {'e'};
    int i {42};
    double d {4.2};
    std::string s {"test"};

    std::cout << b << " " << c << " " << i << " " << d << " " << s << std::endl;

    std::cout << "write the new data:\n";
    std::cin >> b >> c >> i >> d >> s;

    std::cout << b << " " << c << " " << i << " " << d << " " << s << std::endl;
}
