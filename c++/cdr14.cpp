#include <iostream>

auto car = [](auto func) {
    return func( [](auto a, auto b) { return a; } );
};

auto cdr = [](auto func) {
    return func( [](auto a, auto b) { return b; } );
};

auto cons = [](auto a, auto b) {
    return [=](auto func) { return func(a, b); };
};

int main() {
    auto p = cons( cons("a", 2), 3 );

    //std::cout << car(car(p)) << ", " << cdr(car(p)) << ", ";
    //std::cout << cdr(p) << std::endl;

    return 0;
}
