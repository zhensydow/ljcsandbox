#include <iostream>

int main(){
    int val;
    int sum = 0;
    while( std::cin >> val ){
        sum += val;
    }
    std::cout << "sum = " << sum << std::endl;
    return 0;
}
