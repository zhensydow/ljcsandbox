#include <iostream>
#include <fstream>

int main(){
    
    std::ofstream f1("file.txt");

    for( int i = 0 ; i < 200 ; ++i ){
        f1 << i << std::endl;
    }

    f1.close();

    std::ifstream f2("file.txt");
    
    for( int i = 0 ; f2 >> i ; ){
        std::cout << i << std::endl;
    }
}
