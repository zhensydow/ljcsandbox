#include <iostream>

int main(){
    std::cout << "write a name\n";

    std::string name;
    std::cin >> name;

    std::cout << "write an age\n";
    int age;
    std::cin >> age;

    std::cout << name << " has " << age << " years\n";
}
