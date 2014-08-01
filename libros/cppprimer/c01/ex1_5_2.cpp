#include "Sales_item.h"
#include <iostream>

int main(){
    Sales_item currVal;
    Sales_item val;

    if( std::cin >> currVal ){
        int cnt = 1;
        while( std::cin >> val ){
            if( val.isbn() == currVal.isbn() ){
                ++cnt;
            }else{
                std::cout << currVal.isbn() << " occurs "
                          << cnt << " times" << std::endl;
                currVal = val;
                cnt = 1;
            }
        }
        std::cout << currVal.isbn() << " occurs "
                  << cnt << " times" << std::endl;
    }

    return 0;
}
