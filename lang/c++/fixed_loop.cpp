#include <iostream>
#include <chrono>
double getCurrentSecond(){
    using namespace std::chrono;
    auto t0 = high_resolution_clock::now();

    auto d = t0.time_since_epoch();
    return d.count();
}

int main(){
    auto running = true;
    double t = 0.0;
    constexpr double dt = 1.0 / 10.0;
    constexpr double MAX_FRAME_TIME = 0.25;

    double currentTime = getCurrentSecond();
    double accum = 0.0;

    while( running ){
        double newTime = getCurrentSecond();
        double frameTime = newTime - currentTime;
        if( frameTime > MAX_FRAME_TIME ){
            frameTime = MAX_FRAME_TIME;
        }
        currentTime = newTime;

        accum += frameTime;

        while( accum >= dt ){
            std::cout << "update " << t << std::endl;
            t += dt;
            accum -= dt;
        }

        //std::cout << "draw " << currentTime  << std::endl;
    }
}
