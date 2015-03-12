/**
   Compile with GNU:

   g++ -O2 -Wall -Wextra -std=c++11 -pthread main.cpp -o jobqueue

 */
//------------------------------------------------------------------------------
#include <cstdlib>
#include <thread>
#include <iostream>
#include <memory>
#include <vector>
#include <cmath>

#include "WorkQueue.h"

//------------------------------------------------------------------------------
using MyQueue = WorkQueue< std::vector<double> >;
using MyQueuePtr = std::shared_ptr<MyQueue>;

//------------------------------------------------------------------------------
void producer( MyQueuePtr jobq, double input ){
    std::clog << "> Producer started\n";

    for( unsigned k = 0 ; k < 10 ; ++k ){
        auto vec = MyQueue::Work { new std::vector<double>() };

        vec->reserve( 50 );
        for( unsigned i = 0 ; i < 50 ; ++i ){
            vec->push_back( sqrt(i * (k+1)) + input );
        }

        jobq->pushWork( std::move(vec) );
    }

    std::clog << "> Producer ended\n";
}

//------------------------------------------------------------------------------
void consumer( MyQueuePtr jobq ){
    std::clog << "> Consumer started\n";

    while( true ){
        auto work = jobq->getWork();

        if( not work ){
            std::clog << ">> no more work\n";
            break;
        }

        double sum = 0;
        for( auto & v: *work ){
            sum += v;
        }
        double mean = sum / double(work->size());
        std::cout << ">> work mean : " << mean << std::endl;
    }

    std::clog << "> Consumer ended\n";
}

//------------------------------------------------------------------------------
int main(){

    std::clog << "Program started\n";

    // create the work queue to comunicate threads
    auto jobq = std::make_shared<MyQueue>( 10 );

    // create the working threads
    std::thread tprod1 {producer, jobq, 1.0 };
    std::thread tprod2 {producer, jobq, 100.0 };

    std::thread tcons {consumer, jobq};

    // wait for producers end generate work
    tprod1.join();
    tprod2.join();

    // push the end of work
    jobq->pushEnd();

    // wait for consumer end
    tcons.join();

    std::clog << "Program ended\n";

    return EXIT_SUCCESS;
}

//------------------------------------------------------------------------------
