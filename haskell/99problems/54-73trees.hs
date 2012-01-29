import Data.Tuple( swap )

{-| Binary trees

A binary tree is either empty or it is composed of a root element and two
successors, which are binary trees themselves.

In Haskell, we can characterize binary trees with a datatype definition:
-}
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
{-|
This says that a Tree of type a consists of either an Empty node, or a Branch
containing one value of type a with exactly two subtrees of type a.

Given this definition, the tree in the diagram above would be represented as:
-}
tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))
{-|
Since a "leaf" node is a branch with two empty subtrees, it can be useful to
define a shorthand function:
-}
leaf x = Branch x Empty Empty
{-|
Then the tree diagram above could be expressed more simply as:
-}
tree1' = Branch 'a' (Branch 'b' (leaf 'd') (leaf 'e'))
                    (Branch 'c' Empty
                                (Branch 'f' (leaf 'g')
                                            Empty))
{-|
Other examples of binary trees:
-}
-- A binary tree consisting of a root node only
tree2 = Branch 'a' Empty Empty
 
-- An empty binary tree
tree3 = Empty
 
-- A tree of integers
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

{-| Problem 54A
(*) Check whether a given term represents a binary tree

In Prolog or Lisp, one writes a predicate to do this.

Example in Lisp:

* (istree (a (b nil nil) nil))
T
* (istree (a (b nil nil)))
NIL
Non-solution:

Haskell's type system ensures that all terms of type Tree a are binary trees: it
is just not possible to construct an invalid tree with this type. Hence, it is
redundant to introduce a predicate to check this property: it would always
return True.
-}

{-| Problem 55

(**) Construct completely balanced binary trees

In a completely balanced binary tree, the following property holds for every
node: The number of nodes in its left subtree and the number of nodes in its
right subtree are almost equal, which means their difference is not greater than
one.

Write a function cbal-tree to construct completely balanced binary trees for a
given number of nodes. The predicate should generate all solutions via
backtracking. Put the letter 'x' as information into all nodes of the tree.

Example:

* cbal-tree(4,T).
T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
etc......No

Example in Haskell, whitespace and "comment diagrams" added for clarity and
exposition:

*Main> cbalTree 4
[
-- permutation 1
--     x
--    / \
--   x   x
--        \
--         x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)),
 
-- permutation 2
--     x
--    / \
--   x   x
--      /
--     x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty),
 
-- permutation 3
--     x
--    / \
--   x   x
--    \
--     x
Branch 'x' (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)) 
           (Branch 'x' Empty Empty),
 
-- permutation 4
--     x
--    / \
--   x   x
--  /
-- x
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty) 
           (Branch 'x' Empty Empty)
]
-}
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree 2 = [Branch 'x' (Branch 'x' Empty Empty) Empty,
              Branch 'x' Empty (Branch 'x' Empty Empty)]
cbalTree n 
  | odd n = map (\(x,y) -> Branch 'x' x y) [(x,y) | x <- xs, y <- xs]
  | otherwise = map (\(x,y) -> Branch 'x' x y) zzs
    where 
      xs = cbalTree ((n-1) `div` 2)
      ys = [(x,y) | x <- xsEven, y <- ysEven]
      xsEven = cbalTree ((n-1) `div` 2)
      ysEven = cbalTree (((n-1) `div` 2)+1)
      zzs = ys ++ map swap ys
      
{-| Problem 56
(**) Symmetric binary trees

Let us call a binary tree symmetric if you can draw a vertical line through the
root node and then the right subtree is the mirror image of the left
subtree. Write a predicate symmetric/1 to check whether a given binary tree is
symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is
the mirror image of another. We are only interested in the structure, not in the
contents of the nodes.

Example in Haskell:

*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
False
*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
True
-}
symmetric Empty = True
symmetric (Branch _ x y) = symmetric' x y

symmetric' Empty Empty = True
symmetric' Empty _ = False
symmetric' _ Empty = False
symmetric' (Branch _ x1 y1) (Branch _ x2 y2) = (symmetric' x1 y2) 
                                               && (symmetric' y1 x2)
  
{-| Problem 57
(**) Binary search trees (dictionaries)

Use the predicate add/3, developed in chapter 4 of the course, to write a
predicate to construct a binary search tree from a list of integer numbers.

Example:

* construct([3,2,5,7,1],T).
T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
Then use this predicate to test the solution of the problem P56.

Example:

* test-symmetric([5,3,18,1,4,12,21]).
Yes
* test-symmetric([3,2,5,7,4]).
No
Example in Haskell:

*Main> construct [3, 2, 5, 7, 1]
Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
*Main> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
True
*Main> symmetric . construct $ [3, 2, 5, 7, 1]
True
-}
construct xs = construct' True xs
construct' _ [] = Empty
construct' True (x:xs) = Branch x (construct' True ls) (construct' False rs)
  where
    (rs,ls) = splitAt ((length xs) `div` 2) xs
construct' False (x:xs) = Branch x (construct' True rs) (construct' False ls)
  where
    (rs,ls) = splitAt ((length xs) `div` 2) xs

{-| Problem 58
(**) Generate-and-test paradigm

Apply the generate-and-test paradigm to construct all symmetric, completely
balanced binary trees with a given number of nodes.

Example:

* sym-cbal-trees(5,Ts).
Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))] 
Example in Haskell:

*Main> symCbalTrees 5
[Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]
-}
symCbalTrees = filter symmetric . cbalTree

{-| Problem 59
(**) Construct height-balanced binary trees

In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.

Example:

?- hbal_tree(3,T).
T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
etc......No
Example in Haskell:

*Main> take 4 $ hbalTree 'x' 3
[Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]
-}

{-| Problem 60
(**) Construct height-balanced binary trees with a given number of nodes

Consider a height-balanced binary tree of height H. What is the maximum number
of nodes it can contain?

Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This
question is more difficult. Try to find a recursive statement and turn it into a
function minNodes that returns the minimum number of nodes in a height-balanced
binary tree of height H. On the other hand, we might ask: what is the maximum
height H a height-balanced binary tree with N nodes can have? Write a function
maxHeight that computes this.  Now, we can attack the main problem: construct
all the height-balanced binary trees with a given number of nodes. Find out how
many height-balanced trees exist for N = 15.

Example in Prolog:

?- count_hbal_trees(15,C).
C = 1553
Example in Haskell:

*Main> length $ hbalTreeNodes 'x' 15
1553
*Main> map (hbalTreeNodes 'x') [0..3]
[[Empty],
 [Branch 'x' Empty Empty],
 [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
 [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]
Solutions

Binary trees
As defined in problem 54A.

An example tree:

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)
2 Problem 61
Count the leaves of a binary tree

A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.

Example:

% count_leaves(T,N) :- the binary tree T has N leaves
Example in Haskell:

> countLeaves tree4
2
Solutions

3 Problem 61A
Collect the leaves of a binary tree in a list

A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.

Example:

% leaves(T,S) :- S is the list of all leaves of the binary tree T
Example in Haskell:

> leaves tree4
[4,2]
Solutions

4 Problem 62
Collect the internal nodes of a binary tree in a list

An internal node of a binary tree has either one or two non-empty successors. Write a predicate internals/2 to collect them in a list.

Example:

% internals(T,S) :- S is the list of internal nodes of the binary tree T.
Example in Haskell:

Prelude>internals tree4
Prelude>[1,2]
Solutions


5 Problem 62B
Collect the nodes at a given level in a list

A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list.

Example:

% atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L
Example in Haskell:

Prelude>atLevel tree4 2
Prelude>[2,2]
Solutions

6 Problem 63
Construct a complete binary tree

A complete binary tree with height H is defined as follows:

The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order, starting at the root with number 1. For every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist. This fact can be used to elegantly construct a complete binary tree structure.

Write a predicate complete_binary_tree/2.

Example:

% complete_binary_tree(N,T) :- T is a complete binary tree with N nodes.
Example in Haskell:

Main> completeBinaryTree 4
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
 
Main> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
True
Solutions

7 Problem 64
Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid. Several layout methods are conceivable, one of them is shown in the illustration below:



In this layout strategy, the position of a node v is obtained by the following two rules:

x(v) is equal to the position of the node v in the inorder sequence
y(v) is equal to the depth of the node v in the tree
Write a function to annotate each node of the tree with a position, where (1,1) in the top left corner or the rectangle bounding the drawn tree.

Here is the example tree from the above illustration:

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )
Example in Haskell:

> layout tree64
Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...
Solutions


8 Problem 65
An alternative layout method is depicted in the illustration below:



Find out the rules and write the corresponding function. Hint: On a given level, the horizontal distance between neighboring nodes is constant.

Use the same conventions as in problem P64 and test your function in an appropriate way.

Here is the example tree from the above illustration:

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )
Example in Haskell:

> layout tree65
Branch ('n',(15,1)) (Branch ('k',(7,2)) (Branch ('c',(3,3)) ...
Solutions


9 Problem 66
Yet another layout strategy is shown in the illustration below:



The method yields a very compact layout while maintaining a certain symmetry in every node. Find out the rules and write the corresponding Prolog predicate. Hint: Consider the horizontal distance between a node and its successor nodes. How tight can you pack together two subtrees to construct the combined binary tree?

Use the same conventions as in problem P64 and P65 and test your predicate in an appropriate way. Note: This is a difficult problem. Don't give up too early!

Which layout do you like most?

Example in Haskell:

> layout tree65
Branch ('n',(5,1)) (Branch ('k',(3,2)) (Branch ('c',(2,3)) ...
Solutions


10 Problem 67A
A string representation of binary trees

Somebody represents binary trees as strings of the following type:

a(b(d,e),c(,f(g,)))
a) Write a Prolog predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term). Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, combine the two predicates in a single predicate tree_string/2 which can be used in both directions.

Example in Prolog

?- tree_to_string(t(x,t(y,nil,nil),t(a,nil,t(b,nil,nil))),S).
S = 'x(y,a(,b))'
?- string_to_tree('x(y,a(,b))',T).
T = t(x, t(y, nil, nil), t(a, nil, t(b, nil, nil)))
Example in Haskell:

Main> stringToTree "x(y,a(,b))" >>= print
Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
Main> let t = cbtFromList ['a'..'z'] in stringToTree (treeToString t) >>= print . (== t)
True
Solutions


11 Problem 68
Preorder and inorder sequences of binary trees. We consider binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67.

a) Write predicates preorder/2 and inorder/2 that construct the preorder and inorder sequence of a given binary tree, respectively. The results should be atoms, e.g. 'abdecfg' for the preorder sequence of the example in problem P67.

b) Can you use preorder/2 from problem part a) in the reverse direction; i.e. given a preorder sequence, construct a corresponding tree? If not, make the necessary arrangements.

c) If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given, then the tree is determined unambiguously. Write a predicate pre_in_tree/3 that does the job.

Example in Haskell:

Main> let { Just t = stringToTree "a(b(d,e),c(,f(g,)))" ;
            po = treeToPreorder t ;
            io = treeToInorder t } in preInTree po io >>= print
Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))
Solutions


12 Problem 69
Dotstring representation of binary trees.

We consider again binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67. Such a tree can be represented by the preorder sequence of its nodes in which dots (.) are inserted where an empty subtree (nil) is encountered during the tree traversal. For example, the tree shown in problem P67 is represented as 'abd..e..c.fg...'. First, try to establish a syntax (BNF or syntax diagrams) and then write a predicate tree_dotstring/2 which does the conversion in both directions. Use difference lists.

Example in Haskell:

> fst (ds2tree example)
Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))
 
> tree2ds (Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty))
"xy..z0..."
Solutions

Multiway Trees
A multiway tree is composed of a root element and a (possibly empty) set of successors which are multiway trees themselves. A multiway tree is never empty. The set of successor trees is sometimes called a forest.



2 Problem 70B
(*) Check whether a given term represents a multiway tree.

In Prolog or Lisp, one writes a predicate to check this.

Example in Prolog:

?- istree(t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])).
Yes
In Haskell, we define multiway trees as a datatype, as in the module Data.Tree:

data Tree a = Node a [Tree a]
        deriving (Eq, Show)
Some example trees:

tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]
The last is the tree illustrated above.

As in problem 54A, all members of this type are multiway trees; there is no use for a predicate to test them.

3 Problem 70C
(*) Count the nodes of a multiway tree.

Example in Haskell:

Tree> nnodes tree2
2
Solutions

4 Problem 70
(**) Tree construction from a node string.

We suppose that the nodes of a multiway tree contain single characters. In the depth-first order sequence of its nodes, a special character ^ has been inserted whenever, during the tree traversal, the move is a backtrack to the previous level.

By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^^



Define the syntax of the string and write a predicate tree(String,Tree) to construct the Tree when the String is given. Make your predicate work in both directions.

Solutions


5 Problem 71
(*) Determine the internal path length of a tree.

We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree. By this definition, tree5 has an internal path length of 9.

Example in Haskell:

Tree> ipl tree5
9
Tree> ipl tree4
2
Solutions


6 Problem 72
(*) Construct the bottom-up order sequence of the tree nodes.

Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence of the nodes of the multiway tree Tree.

Example in Haskell:

Tree> bottom_up tree5
"gfcdeba"
Solutions


7 Problem 73
(**) Lisp-like tree representation.

There is a particular notation for multiway trees in Lisp. Lisp is a prominent functional programming language, which is used primarily for artificial intelligence problems. As such it is one of the main competitors of Prolog. In Lisp almost everything is a list, just as in Prolog everything is a term.

The following pictures show how multiway tree structures are represented in Lisp.



Note that in the "lispy" notation a node with successors (children) in the tree is always the first element in a list, followed by its children. The "lispy" representation of a multiway tree is a sequence of atoms and parentheses '(' and ')', which we shall collectively call "tokens". We can represent this sequence of tokens as a Prolog list; e.g. the lispy expression (a (b c)) could be represented as the Prolog list ['(', a, '(', b, c, ')', ')']. Write a predicate tree_ltl(T,LTL) which constructs the "lispy token list" LTL if the tree is given as term T in the usual Prolog notation.

(The Prolog example given is incorrect.)

Example in Haskell:

Tree> display lisp tree1
"a"
Tree> display lisp tree2
"(a b)"
Tree> display lisp tree3
"(a (b c))"
Tree> display lisp tree4
"(b d e)"
Tree> display lisp tree5
"(a (f g) c (b d e))"
As a second, even more interesting exercise try to rewrite tree_ltl/2 in a way that the inverse conversion is also possible.

Solutions

-}