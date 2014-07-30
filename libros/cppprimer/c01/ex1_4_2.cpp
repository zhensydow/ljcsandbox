#include <iostream>

void test1(){
    int sum = 0;
    for( int i = -100 ; i <= 100 ; ++i ){
        sum += i;
    }
    std::cout << "sum = " << sum << std::endl;
}

void test2(){
    int sum = 0;
    for( int i = 50 ; i <= 100 ; ++i ){
        sum += i;
    }
    std::cout << "sum = " << sum << std::endl;

    for( int i = 10 ; i >= 0; --i ){
        std::cout << "count = " << i << std::endl;
    }
}

void test3(){
    // // syntax error
    // std::cout << "syntax error " << std;

    // // type error
    // int a = 0;
    // a = a + "2";
    // std::cout << "sum = " << a << std::endl;

    // // declaration error
    // std::cout << "sum = " << sum << std::endl;
}

int main(){
    test1();
    test2();
    test3();
}
