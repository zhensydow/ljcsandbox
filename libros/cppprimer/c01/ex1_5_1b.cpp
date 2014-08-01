#include "Sales_item.h"
#include <iostream>

int main(){
    Sales_item item1, item2;

    std::cin >> item1 >> item2;
    if( item1.isbn() != item2.isbn() ){
        std::cerr << " different isbn " << std::endl;
        return -1;
    }

    std::cout << item1 + item2  << std::endl;

    return 0;
}
