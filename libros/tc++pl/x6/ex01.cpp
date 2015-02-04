// g++ -std=c++11 -Wall -Wextra -pthread -O2 ex01.cpp
#include <iostream>
#include <thread>
#include <chrono>

void hello(){
    while( true ){
        std::cout << "hello\n";
        std::this_thread::sleep_for( std::chrono::seconds(1) );
    }
}

void world(){
    while( true ){
        std::cout << "world!\n";
        std::this_thread::sleep_for( std::chrono::seconds(1) );
    }
}

int main(){
    std::thread t1 {hello};
    std::thread t2 {world};

    t1.join();
    t2.join();
}
