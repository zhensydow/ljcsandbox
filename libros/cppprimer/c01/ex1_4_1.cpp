#include <iostream>

void test1(){
    int sum = 0;
    int val = 50;
    while( val <= 100 ){
        sum += val;
        ++val;
    }

    std::cout << "sum = " << sum << std::endl;
}

void test2(){
    int val = 10;
    while( val >= 0 ){
        std::cout << "count = " << val << std::endl;
        --val;
    }
}

void test3(){
    std::cout << "Write two numbers of a range:" << std::endl;
    int a, b;
    std::cin >> a >> b;
    int val = a;
    int inc = a > b ? -1 : 1;
    int end = b + inc;
    while( val != end ){
        std::cout << "count = " << val << std::endl;
        val += inc;
    }
    
}

int main(){
    test1();
    test2();
    test3();
}
