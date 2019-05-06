#include <limits>
#include <cmath>
#include <cstdlib>
#include <cstdio>

template<typename T>
bool almost_equal( T x, T y, int ulp ){
    const T diff = std::abs(x - y);
    return diff < std::numeric_limits<T>::epsilon() * ulp;
}


int main(){
    printf( "min:        %.200f, \ndenorm min: %.200f, \nepsilon: %.200f, \nlowest: %f\n",
            std::numeric_limits<float>::min(),
            std::numeric_limits<float>::denorm_min(),
            std::numeric_limits<float>::epsilon(),
            std::numeric_limits<float>::lowest());

    printf( "min:        %.400f, \ndenorm min: %.400f, \nepsilon: %.400f, \nlowest: %f\n",
            std::numeric_limits<double>::min(),
            std::numeric_limits<double>::denorm_min(),
            std::numeric_limits<double>::epsilon(),
            std::numeric_limits<double>::lowest());

    float x = 1.10002f, y = 1.10001f;
    printf( "equal: %.7f == %.7f, --- %d\n", x, y, almost_equal( x, y, 10 ) );

    x = 1.10000002f, y = 1.10000001f;
    printf( "equal: %.9f == %.9f, --- %d\n", x, y, almost_equal( x, y, 10 ) );

    return EXIT_SUCCESS;
}

