// -*- compile-command: "g++ -std=c++17 -O3 -o problem problem002.cpp" -*-
#include <cstdio>

int main(){
    int sum = 0;

    int a = 1;
    int b = 2;
    while( a <= 4000000 ){
        printf( "%d\n", a );

        if( a % 2 == 0 ){
            sum += a;
        }

        int tmp = a + b;
        a = b;
        b = tmp;

    }

    printf( "%d\n",  sum );

    return 0;
}
