/*
  g++ -std=c++11 -o singleton singleton.cpp -pthread -Wl,--no-as-needed
 */
#include <iostream>
#include <mutex>
#include <memory>
#include <thread>
#include <vector>

class Singleton{
public:
    static Singleton *getInstance(){
        std::call_once( m_onceFlag,
                        [] {
                            m_instance.reset( new Singleton );
                        });
        return m_instance.get();
    }

    virtual ~Singleton(){
    }

    Singleton( Singleton const& ) = delete;
    Singleton& operator=( Singleton const& ) = delete;

protected:
    Singleton(){
        std::cout << "Creating singleton" << std::endl;
    }

private:
    static std::unique_ptr<Singleton> m_instance;
    static std::once_flag m_onceFlag;

    int x;
};

std::unique_ptr<Singleton> Singleton::m_instance;
std::once_flag Singleton::m_onceFlag;

int main(){
    std::vector<std::thread> threads;

    std::cout << "* Start " << std::endl;
    for( int i = 0 ; i < 10 ; ++i ){
        auto t = std::thread([=](){
                auto s = Singleton::getInstance();
                std::cout << "Thread " << i << std::endl;
            });

        threads.push_back( std::move(t) );
    }

    std::cout << "* Wait .. " << std::endl;
    for( auto & t: threads ){
        t.join();
    }

    std::cout << "* End" << std::endl;

    return 0;
}
