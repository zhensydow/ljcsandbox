//------------------------------------------------------------------------------
#ifndef WORKQUEUE_H_
#define WORKQUEUE_H_

//------------------------------------------------------------------------------
#include <memory>
#include <mutex>
#include <queue>
#include <condition_variable>
#include <cassert>

//------------------------------------------------------------------------------
/**
   A concurrent queue to pass work items between threads.
   @param T type of the Work Items
 */
template< typename T >
class WorkQueue{
public:
    using Work = std::unique_ptr<T>;

    WorkQueue( unsigned maxItems );

    Work getWork();
    void pushWork( Work w );
    void pushEnd();

private:
    std::mutex m_mutex;
    std::condition_variable m_cond;
    std::queue< Work > m_queue;
    unsigned m_maxItems = 2;
};

//------------------------------------------------------------------------------
/**
   @params maxItems number maximum of work items
 */
template< typename T >
WorkQueue<T>::WorkQueue( unsigned maxItems ) 
: m_maxItems{maxItems} 
{
    assert( m_maxItems > 0 && "Invalid maximum limit" );
}

//------------------------------------------------------------------------------
/**
   Get a Work Item from the Queue. Wait until there is work available.
   @returns a Work Item
 */
template< typename T>
typename WorkQueue<T>::Work
WorkQueue<T>::getWork(){
    // wait until there is work in the queue
    std::unique_lock<std::mutex> lck{ m_mutex };
    m_cond.wait( lck, [this]{ return not m_queue.empty(); });

    // get the first work item
    auto w = std::move( m_queue.front() );
    m_queue.pop();
    
    // notify work is got.
    m_cond.notify_one();
    lck.unlock();

    // return the work item
    return w;
}

//------------------------------------------------------------------------------
/**
   Insert a Work Item in the queue.
   @param w the Work Item to queue.
 */
template< typename T>
void
WorkQueue<T>::pushWork( Work w ){
    // wait until there is space in the queue
    std::unique_lock<std::mutex> lck{ m_mutex };
    m_cond.wait( lck, [this]{ return m_queue.size() < m_maxItems; });

    // push a work item in the queue
    m_queue.push( std::move(w) );

    // notify there is work available
    m_cond.notify_one();
}

//------------------------------------------------------------------------------
/**
   Insert the end of work in the queue.
 */
template< typename T >
inline
void
WorkQueue<T>::pushEnd(){
    // push a null pointer item to mark the end
    this->pushWork( Work {nullptr} );
}

//------------------------------------------------------------------------------
#endif//WORKQUEUE_H_

//------------------------------------------------------------------------------
