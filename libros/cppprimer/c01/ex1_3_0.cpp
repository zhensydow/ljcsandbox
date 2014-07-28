#include <iostream>

/* nested comment /*

 */

int main(){
    std::cout << " /* ";
    std::cout << " */ ";
    //std::cout << /* "*/" */;
    std::cout << /* "*/" /* "/*" */;

    return 0;
}
