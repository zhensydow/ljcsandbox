//------------------------------------------------------------------------------
#include <cstdlib>
#include <iostream>
#include <vector>
#include <string>

//------------------------------------------------------------------------------
class node{
public:
    int data;
    node* next;
};

//------------------------------------------------------------------------------
int Length( const node * head );
node * BuildOneTwoThree();
void Push( node ** headRef, int newData );

//------------------------------------------------------------------------------
int Length( const node * head ){
    int count = 0;

    while( head != nullptr ){
        ++count;
        head = head->next;
    }

    return count;
}

//------------------------------------------------------------------------------
std::string Print( const node * head ){
    std::string msg = "{ ";

    while( head != nullptr ){
        msg += std::to_string( head->data );
        if( head->next != nullptr ){
            msg += ", ";
        }
        head = head->next;
    }

    msg += " }";

    return msg;
}

//------------------------------------------------------------------------------
void Push( node ** headRef, int newData ){
    node * p = new node;
    p->data = newData;
    p->next = *headRef;
    *headRef = p;
}

//------------------------------------------------------------------------------
int Pop( node ** headRef ){
    if( *headRef == nullptr ){
        return -1;
    }

    node * p = *headRef;
    *headRef = p->next;
    int value = p->data;
    delete p;
    return value;
}

//------------------------------------------------------------------------------
int Count( node * head, int searchFor ){
    int count = 0;

    while( head != nullptr ){
        if( head->data == searchFor ){
            ++count;
        }
        head = head->next;
    }

    return count;
}

//------------------------------------------------------------------------------
int GetNth( const node * head, int index ){
    while( head != nullptr and index > 0 ){
        --index;
        head = head->next;
    }

    if( head != nullptr ){
        return head->data;
    }

    return -1;
}

//------------------------------------------------------------------------------
void InsertNth( node ** headRef, int index, int value ){
    node * p = new node;
    p->data = value;

    if( *headRef == nullptr or index == 0 ){
        p->next = *headRef;
        *headRef = p;
        return;
    }

    node * q = *headRef;

    while( q->next != nullptr and index > 1 ){
        --index;
        q = q->next;
    }

    p->next = q->next;
    q->next = p;
}

//------------------------------------------------------------------------------
void SortedInsert( node ** headRef, node * newnode ){
    if( *headRef == nullptr or (*headRef)->data >= newnode->data ){
        newnode->next = *headRef;
        *headRef = newnode;
        return;
    }

    node * q = *headRef;

    while( q->next != nullptr and q->next->data < newnode->data ){
        q = q->next;
    }

    newnode->next = q->next;
    q->next = newnode;
}

//------------------------------------------------------------------------------
void DeleteList( node ** headRef ){
    while( *headRef != nullptr ){
        node * p = *headRef;
        *headRef = p->next;
        delete p;
    }

}

//------------------------------------------------------------------------------
void InsertSort( node ** headRef ){
    node * sorted = nullptr;

    while( *headRef != nullptr ){
        node * p = *headRef;
        *headRef = p->next;
        SortedInsert( &sorted, p );
    }

    *headRef = sorted;
}

//------------------------------------------------------------------------------
void Append( node ** aRef, node ** bRef ){
    if( *aRef == nullptr ){
        *aRef = *bRef;
        *bRef = nullptr;
        return;
    }

    node * p = *aRef;

    while( p->next != nullptr ){
        p = p->next;
    }

    p->next = *bRef;
    *bRef = nullptr;
}

//------------------------------------------------------------------------------
void FrontBackSplit( node * source, node ** frontRef, node ** backRef ){
    auto len = Length( source );

    *frontRef = source;
    *backRef = nullptr;

    if( len < 2){
        return;
    }

    int mid = (len+1) >> 1;

    node ** q = &source;
    while( (*q) != nullptr and mid > 0 ){
        --mid;
        q = &((*q)->next);
    }

    *backRef = *q;
    *q = nullptr;
}

//------------------------------------------------------------------------------
void RemoveDuplicates( node * head ){
    node * p = nullptr;

    while( head != nullptr ){
        if( head->next != nullptr and head->data == head->next->data ){
            p = head->next;
            head->next = p->next;
            delete p;
        }else{
            head = head->next;
        }
    }
}

//------------------------------------------------------------------------------
void MoveNode( node ** aRef, node ** bRef ){
    if( *bRef == nullptr ){
        return;
    }

    node * p = *bRef;

    *bRef = p->next;

    p->next = *aRef;

    *aRef = p;
}

//------------------------------------------------------------------------------
void AlternatingSplit( node * source, node ** aRef, node ** bRef ){
    node * p = nullptr;
    node * q = nullptr;

    while( source != nullptr ){
        MoveNode( &p, &source );
        MoveNode( &q, &source );
    }

    *aRef = p;
    *bRef = q;
}

//------------------------------------------------------------------------------
node * ShuffleMerge( node * a, node * b ){
    if( a == nullptr ){
        return b;
    }

    a->next = ShuffleMerge( b, a->next );;

    return a;
}

//------------------------------------------------------------------------------
node * SortedMerge( node * a, node * b ){
    if( a == nullptr ){
        return b;
    }

    if( b == nullptr ){
        return a;
    }

    if( a->data < b->data ){
        a->next = SortedMerge( a->next, b );
        return a;
    }else{
        b->next = SortedMerge( a, b->next );
        return b;
    }
}

//------------------------------------------------------------------------------
void MergeSort( node ** headRef ){
    if( *headRef == nullptr or (*headRef)->next == nullptr ){
        return;
    }

    node * a;
    node * b;

    FrontBackSplit( *headRef, &a, &b );

    MergeSort( &a );
    MergeSort( &b );

    *headRef = SortedMerge( a, b );
}

//------------------------------------------------------------------------------
node * SortedIntersect( node * a, node * b){
    node * q = nullptr;

    while( a != nullptr and b != nullptr ){
        if( a->data == b->data ){
            Push( &q, a->data );
            a = a->next;
            b = b->next;
        }else if( a->data < b->data ){
            a = a->next;
        }else{
            b = b->next;
        }
    }

    MergeSort( &q );

    return q;
}

//------------------------------------------------------------------------------
void Reverse( node ** headRef ){
    if( *headRef == nullptr or (*headRef)->next == nullptr ){
        return;
    }

    node * rev = nullptr;

    while( (*headRef)->next != nullptr ){
        node * p = *headRef;
        *headRef = (*headRef)->next;

        p->next = rev;
        rev = p;
    }

    (*headRef)->next = rev;
}

//------------------------------------------------------------------------------
void RecursiveReverse( node ** headRef ){
    if( *headRef == nullptr or (*headRef)->next == nullptr ){
        return;
    }

    node * first = (*headRef)->next;

    node * reversed = first;

    RecursiveReverse( &reversed );

    (*headRef)->next = nullptr;

    first->next = *headRef;

    *headRef = reversed;
}

//------------------------------------------------------------------------------
node * BuildOneTwoThree(){
    node * head = nullptr;

    Push( &head, 3 );
    Push( &head, 2 );
    Push( &head, 1 );

    return head;
};

//------------------------------------------------------------------------------
node * AddAtHead(){
    node * head = nullptr;

    for( auto i = 1 ; i < 6 ; ++i ){
        Push( &head, i );
    }

    return head;
}

//------------------------------------------------------------------------------
void BasicsCaller(){
    node * head;
    int len;

    head = BuildOneTwoThree();

    Push( &head, 13 );
    std::cout << Print( head ) << std::endl;

    Push( &(head->next), 42 );
    std::cout << Print( head ) << std::endl;

    len = Length( head );

    std::cout << "len = " << len << std::endl;
}

//------------------------------------------------------------------------------
bool Problem1Test(){
    node * myList = BuildOneTwoThree();

    int count = Count( myList, 2 );

    return (count == 1);
}

//------------------------------------------------------------------------------
bool Problem2Test(){
    node * myList = BuildOneTwoThree();

    int lastNode = GetNth( myList, 2 );

    return (lastNode == 3);
}

//------------------------------------------------------------------------------
bool Problem3Test(){
    node * myList = BuildOneTwoThree();

    DeleteList( &myList );

    return (myList==nullptr);
}

//------------------------------------------------------------------------------
bool Problem4Test(){
    node * myList = BuildOneTwoThree();

    int a = Pop( &myList );
    int b = Pop( &myList );
    int c = Pop( &myList );

    auto len = Length( myList );

    return (len==0 and a == 1 and b == 2 and c == 3);
}

//------------------------------------------------------------------------------
bool Problem5Test(){
    node * myList = nullptr;

    InsertNth( &myList, 0, 13 );
    InsertNth( &myList, 1, 42 );
    InsertNth( &myList, 1, 5 );

    bool check = Length( myList ) == 3 and GetNth( myList, 1 ) == 5;

    DeleteList( &myList );

    return check;
}

//------------------------------------------------------------------------------
bool Problem6Test(){
    node * myList = nullptr;

    node * p;

    std::vector<int> xs = { 5, 3, 1, 6, 4, 2, 12, 11, 10, 9, 8, 7 };

    for( auto i: xs ){
        p = new node;
        p->data = i;
        p->next = nullptr;

        SortedInsert( &myList, p );
    }

    bool check = Length( myList ) == int(xs.size())
        and Print( myList ) == "{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 }" ;

    DeleteList( &myList );

    return check;
}

//------------------------------------------------------------------------------
bool Problem7Test(){
    node * myList = nullptr;

    std::vector<int> xs = { 5, 3, 1, 6, 4, 2, 12, 11, 10, 9, 8, 7 };

    for( auto i: xs ){
        Push( &myList, i );
    }

    InsertSort( &myList );

    bool check = Length( myList ) == int(xs.size())
        and Print( myList ) == "{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 }" ;

    DeleteList( &myList );

    return check;
}

//------------------------------------------------------------------------------
bool Problem8Test(){
    node * a = BuildOneTwoThree();
    node * b = BuildOneTwoThree();

    Append( &a, &b );

    bool check = Length( a ) == 6 and b == nullptr
        and Print( a ) == "{ 1, 2, 3, 1, 2, 3 }";

    DeleteList( &a );

    return check;
}

//------------------------------------------------------------------------------
bool Problem9Test(){

    node * a = nullptr;

    node * b = nullptr;
    Push( &b, 2 );
    Push( &b, 1 );

    node * c = BuildOneTwoThree();

    node * d = nullptr;

    for( auto i: {6,5,4,3,2,1} ){
        Push( &d, i );
    }

    node * p1;
    node * p2;

    bool check = true;

    for( auto myList: {a,b,c,d} ){
        auto len = Length(myList);
        FrontBackSplit( myList, &p1, &p2 );

        check = check and len == (Length(p1) + Length(p2));
    }

    return check;
}

//------------------------------------------------------------------------------
bool Problem10Test(){
    node * myList = nullptr;

    for( auto i: {1,1,2,3,4,4,4,5,6,6} ){
        Push( &myList, i );
    }

    InsertSort( &myList );

    RemoveDuplicates( myList );

    return (Print(myList) == "{ 1, 2, 3, 4, 5, 6 }");
}

//------------------------------------------------------------------------------
bool Problem11Test(){
    node * a = BuildOneTwoThree();
    node * b = BuildOneTwoThree();

    MoveNode( &a, &b );

    return (Print( a ) == "{ 1, 1, 2, 3 }") and (Print( b ) == "{ 2, 3 }");
}

//------------------------------------------------------------------------------
bool Problem12Test(){
    node * myList = nullptr;

    for( auto i: {8,7,6,5,4,3,2,1} ){
        Push( &myList, i );
    }

    node * a = nullptr;
    node * b = nullptr;

    auto len = Length( myList );

    AlternatingSplit( myList, &a, &b );

    return len == (Length(a) + Length(b));
}

//------------------------------------------------------------------------------
bool Problem13Test(){
    node * a = nullptr;

    for( auto i: {3,2,1} ){
        Push( &a, i );
    }

    node * b = nullptr;

    for( auto i: {1,13,7} ){
        Push( &b, i );
    }

    node * myList = ShuffleMerge( a, b );

    return Print( myList ) == "{ 1, 7, 2, 13, 3, 1 }";
}

//------------------------------------------------------------------------------
bool Problem14Test(){
    node * a = nullptr;

    for( auto i: {4,3,1} ){
        Push( &a, i );
    }

    node * b = nullptr;

    for( auto i: {6,5,2} ){
        Push( &b, i );
    }

    node * myList = SortedMerge( a, b );

    return Print( myList ) == "{ 1, 2, 3, 4, 5, 6 }";
}

//------------------------------------------------------------------------------
bool Problem15Test(){
    node * myList = nullptr;

    std::vector<int> xs = { 5, 3, 1, 6, 4, 2, 12, 11, 10, 9, 8, 7 };

    for( auto i: xs ){
        Push( &myList, i );
    }

    MergeSort( &myList );

    bool check = Print( myList ) == "{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 }" ;

    DeleteList( &myList );

    return check;
}

//------------------------------------------------------------------------------
bool Problem16Test(){
    node * a = nullptr;

    for( auto i: {1,2,3,4,5,6} ){
        Push( &a, i );
    }

    MergeSort( &a );

    node * b = nullptr;

    for( auto i: {2,4,5,7} ){
        Push( &b, i );
    }

    MergeSort( &b );

    node * myList = SortedIntersect( a, b );

    return Print( myList ) == "{ 2, 4, 5 }";
}

//------------------------------------------------------------------------------
bool Problem17Test(){
    node * myList = BuildOneTwoThree();

    Reverse( &myList );

    bool check = Print( myList ) == "{ 3, 2, 1 }";

    DeleteList( &myList );

    return check;
}

//------------------------------------------------------------------------------
bool Problem18Test(){
    node * myList = nullptr;

    for( auto i: {5,4,3,2,1} ){
        Push( &myList, i );
    }

    std::cout << Print( myList ) << std::endl;

    RecursiveReverse( &myList );

    std::cout << Print( myList ) << std::endl;

    bool check = Print( myList ) == "{ 5, 4, 3, 2, 1 }";

    DeleteList( &myList );

    return check;
}

//------------------------------------------------------------------------------
void TestProblems(){
    auto tests = { Problem1Test, Problem2Test, Problem3Test, Problem4Test,
                   Problem5Test, Problem6Test, Problem7Test, Problem8Test,
                   Problem9Test, Problem10Test, Problem11Test, Problem12Test,
                   Problem13Test, Problem14Test, Problem15Test, Problem16Test,
                   Problem17Test, Problem18Test };
    int i = 1;

    for( auto & t: tests ){
        auto result = t();
        std::cout << "Test " << i++;
        if( result ){
            std::cout << " ok\n";
        }else{
            std::cout << " failed\n";
        }
    }
}

//------------------------------------------------------------------------------
int main(){

    BasicsCaller();

    auto head = BuildOneTwoThree();

    std::cout << Print( head ) << std::endl;

    auto head2 = AddAtHead();

    std::cout << Print( head2 ) << std::endl;

    TestProblems();

    return EXIT_SUCCESS;
}

//------------------------------------------------------------------------------
