//------------------------------------------------------------------------------
#include <cstdlib>
#include <iostream>
#include <vector>
#include <random>

//------------------------------------------------------------------------------
class node{
public:
    int data;
    node * left;
    node * right;
};

//------------------------------------------------------------------------------
void ListPushBack( node ** headRef, int value ){
    if( *headRef == nullptr ){
        *headRef = new node;
        (*headRef)->data = value;
        (*headRef)->left = *headRef;
        (*headRef)->right = *headRef;
    }else{
        node * p = new node;
        p->data = value;
        p->right = *headRef;
        p->left = (*headRef)->left;
        (*headRef)->left->right = p;
        (*headRef)->left = p;
    }
}

//------------------------------------------------------------------------------
void TreeInsert( node ** rootRef, int value ){
    if( *rootRef == nullptr ){
        *rootRef = new node;
        (*rootRef)->data = value;
        (*rootRef)->left = nullptr;
        (*rootRef)->right = nullptr;
    }else{
        if( value < (*rootRef)->data ){
            TreeInsert( &((*rootRef)->left), value );
        }else{
            TreeInsert( &((*rootRef)->right), value );
        }
    }
}

//------------------------------------------------------------------------------
node * Append( node * a, node * b ){
    if( a == nullptr ){
        return b;
    }

    if( b == nullptr ){
        return a;
    }

    auto last_a = a->left;
    auto last_b = b->left;

    a->left->right = b;
    a->left = last_b;

    b->left->right = a;
    b->left = last_a;

    return a;
}

//------------------------------------------------------------------------------
template<class T>
node * CreateList( T xs ){
    node * head = nullptr;

    for( auto x: xs ){
        ListPushBack( &head, x );
    }

    return head;
}

//------------------------------------------------------------------------------
template<class T>
node * CreateTree( T xs ){
    node * root = nullptr;

    for( auto x: xs ){
        TreeInsert( &root, x );
    }

    return root;
}

//------------------------------------------------------------------------------
node * TreeToList( node * root ){
    if( root == nullptr ){
        return nullptr;
    }

    auto llist = TreeToList( root->left );
    auto rlist = TreeToList( root->right );

    root->left = root;
    root->right = root;

    return Append( Append( llist, root ), rlist );
}

//------------------------------------------------------------------------------
std::string PrintList( node * head ){
    if( head == nullptr ){
        return "{ }";
    }

    std::string msg = "{ ";

    node * p = head;
    do{
        msg += std::to_string( p->data );
        if( p->right != head ){
            msg += ", ";
        }
        p = p->right;
    }while( p != head );

    msg += " }";

    return msg;
}

//------------------------------------------------------------------------------
std::string PrintTree( node * root ){
    if( root == nullptr ){
        return ".";
    }

    std::string msg = "{ ";

    msg += PrintTree( root->left );
    msg += " " + std::to_string( root->data ) + " ";
    msg += PrintTree( root->right );
    msg += " }";

    return msg;
}

//------------------------------------------------------------------------------
std::string PrintBackList( node * head ){
    if( head == nullptr ){
        return "{ }";
    }

    std::string msg = "{ ";

    node * p = head->left;
    while( p != head ){
        msg += std::to_string( p->data );
        msg += ", ";
        p = p->left;
    };

    msg += std::to_string( head->data );

    msg += " }";

    return msg;
}

//------------------------------------------------------------------------------
void TestRandomTree(){
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(1, 100);

    std::vector<int> values;

    for( auto i = 0 ; i < 100 ; ++i ){
        values.emplace_back( dis(gen) );
    }

    auto tree = CreateTree( values );

    std::cout << PrintTree( tree ) << std::endl;

    auto treelist = TreeToList( tree );

    std::cout << PrintList( treelist ) << std::endl;
}

//------------------------------------------------------------------------------
int main(){
    auto list = CreateList( std::vector<int>( {1,2,3,4,5} ) );

    std::cout << PrintList( list ) << std::endl;
    std::cout << PrintBackList( list ) << std::endl;

    auto listA = CreateList( std::vector<int>( {1,2,3,} ) );
    auto listB = CreateList( std::vector<int>( {4,5} ) );

    auto ab = Append( listA, listB );

    std::cout << PrintList( ab ) << std::endl;

    auto tree = CreateTree( std::vector<int>( {4,2,1,3,5} ) );

    std::cout << PrintTree( tree ) << std::endl;

    auto treelist = TreeToList( tree );

    std::cout << PrintList( treelist ) << std::endl;

    TestRandomTree();

    return EXIT_SUCCESS;
}

//------------------------------------------------------------------------------
