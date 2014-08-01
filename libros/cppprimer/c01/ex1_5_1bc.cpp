#include "Sales_item.h"
#include <iostream>

int main(){
    Sales_item item;

    if( std::cin >> item ){
        Sales_item total = item;
        auto isbn = item.isbn();
        while( std::cin >> item ){
            if( item.isbn() == isbn ){
                total = total + item;
            }
        }

        std::cout << " total " << total << std::endl;
    }

    return 0;
}
