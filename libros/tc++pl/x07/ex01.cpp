#include <iostream>

int main( ){
    char c = 255;
    int i = c;

    if( i == 255 ){
        std::cout << "signed\n";
    }else{
        std::cout << "unsigned\n";
    }    
}
