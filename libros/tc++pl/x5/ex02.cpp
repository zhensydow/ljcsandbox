#include <iostream>

//------------------------------------------------------------------------------
class Person{
public:
    std::string name;
    int age;
};

//------------------------------------------------------------------------------
std::istream& operator>>( std::istream & is, Person & p ){
    std::cout << "write a name\n";

    std::string name;
    std::cin >> name;

    std::cout << "write an age\n";
    int age;
    std::cin >> age;
    
    p = {name,age};

    return is;
}

//------------------------------------------------------------------------------
std::ostream& operator<<( std::ostream & os, const Person & p ){
    std::cout << p.name << " has " << p.age << " years\n";
    
    return os;
}

//------------------------------------------------------------------------------
int main(){
    for( Person p ; std::cin >> p; ){
        std::cout << p;        
    }

}

//------------------------------------------------------------------------------
