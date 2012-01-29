import Control.Arrow( (&&&) )
import Data.List( foldl', delete, (\\), sortBy, group, sort )
import Data.Ord( comparing )
import Data.Maybe( fromJust )
import System.Random( randomRIO )

{-| Problem 1
(*) Find the last element of a list.

Example in Haskell:

@
Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
@
-}

myLast [x] = x
myLast (x:xs) = myLast xs

{-| Problem 2
(*) Find the last but one element of a list.

Example in Haskell:

@
Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'
@
-}
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

{-| Problem 3
(*) Find the K'th element of a list. The first element in the list is number 1.

Example in Haskell:

@
Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'
@
-}
elementAt (x:xs) n
  | n == 1 = x
  | otherwise = elementAt xs (n-1)
                
{-| Problem 4
(*) Find the number of elements of a list.

Example in Haskell:

@
Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13
@
-}
myLength = foldl (\a b -> a + 1) 0

{-| Problem 5
(*) Reverse a list.

Example in Haskell:

@
Prelude> reverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> reverse [1,2,3,4]
[4,3,2,1]
@
-}
myReverse = foldr (\a b -> b ++ [a]) []
 
{-| Problem 6
(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

Example in Haskell:

@
*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True
@
-}
isPalindrome xs = xs == myReverse xs

{-| Problem 7
(**) Flatten a nested list structure.

Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example:

* (my-flatten '(a (b (c d) e)))
(A B C D E)
Example in Haskell:

*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[]
-}
data MyList a = Elem a | List [MyList a] deriving( Show )

flatten :: MyList a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = concatMap flatten xs

{-| Problem 8
(**) Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be replaced with a single copy
of the element. The order of the elements should not be changed.

Example:

* (compress '(a a a a b c c a a d e e e e))
(A B C A D E)
Example in Haskell:

> compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
["a","b","c","a","d","e"]
-}
compress [] = []
compress [x] = [x]
compress (x:y:xs) 
  | x == y = compress (x:xs)
  | otherwise = x : compress (y:xs)

{-| Problem 9
(**) Pack consecutive duplicates of list elements into sublists. If a list
contains repeated elements they should be placed in separate sublists.

Example:

* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))
Example in Haskell:

*Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
-}
pack :: Eq a => [a] -> [[a]]
pack = pack' []

pack' :: Eq a => [a] -> [a] -> [[a]]
pack' xs [] = [xs]
pack' [] (y:ys) = pack' [y] ys
pack' (x:xs) (y:ys) 
  | x == y = pack' ((x:xs) ++ [y]) ys
  | otherwise = [(x:xs)] ++ pack' [y] ys

{-| Problem 10
(*) Run-length encoding of a list. Use the result of problem P09 to implement
the so-called run-length encoding data compression method. Consecutive
duplicates of elements are encoded as lists (N E) where N is the number of
duplicates of the element E.

Example:

* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
Example in Haskell:

encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}
encode :: Eq a => [a] -> [(Integer, a)]
encode = map (\xs -> (myLength xs, head xs)) . pack

{-| Problem 11
(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no
duplicates it is simply copied into the result list. Only elements with
duplicates are transferred as (N E) lists.

Example:

* (encode-modified '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))
Example in Haskell:
@
P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
@
-}
data RLE a = Multiple Integer a | Single a 
           deriving( Show )
encodeModified :: Eq a => [a] -> [RLE a]
encodeModified = map rlef . pack
  where
    rlef xs = let ll = myLength xs
                  la = head xs
              in if ll == 1 
                 then Single la
                 else Multiple ll la
{-| Problem 12
(**) Decode a run-length encoded list.

Given a run-length code list generated as specified in problem 11. Construct its
uncompressed version.

Example in Haskell:
@
P12> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
@
-}
decodeModified :: Eq a => [RLE a] -> [a]
decodeModified = concatMap rlef
  where
    rlef (Multiple ll la) = replicate (fromIntegral ll) la
    rlef (Single la) = [la]
    
{-| Problem 13
(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method
directly. I.e. don't explicitly create the sublists containing the duplicates,
as in problem 9, but only count them. As in problem P11, simplify the result
list by replacing the singleton lists (1 X) by X.

Example:

* (encode-direct '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))
Example in Haskell:
@
P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
@
-}
rlee x n
  | n == 1 = Single x
  | otherwise = Multiple n x
                
encodeDirect :: Eq a => [a] -> [RLE a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' x 1 xs

encodeDirect' :: Eq a => a -> Integer -> [a] -> [RLE a]
encodeDirect' x n [] = [rlee x n]
encodeDirect' x n (y:ys) 
  | x == y = encodeDirect' x (n+1) ys
  | otherwise = rlee x n : encodeDirect' y 1 ys

{-| Problem 14
(*) Duplicate the elements of a list.

Example:

* (dupli '(a b c c d))
(A A B B C C C C D D)
Example in Haskell:
@
> dupli [1, 2, 3]
[1,1,2,2,3,3]
@
-}
dupli = concatMap (\x -> [x,x])
    
{-| Problem 15
(**) Replicate the elements of a list a given number of times.

Example:

* (repli '(a b c) 3)
(A A A B B B C C C)
Example in Haskell:

> repli "abc" 3
"aaabbbccc"
-}
repli xs n = concatMap (replicate n) xs

{-| Problem 16
(**) Drop every N'th element from a list.

Example:

* (drop '(a b c d e f g h i k) 3)
(A B D E G H K)
Example in Haskell:

*Main> dropEvery "abcdefghik" 3
"abdeghk"
-}
dropEvery xs n = map snd . filter ((/=n).fst) . zip (cycle [1..n]) $ xs

{-| Problem 17
(*) Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates.

Example:

* (split '(a b c d e f g h i k) 3)
( (A B C) (D E F G H I K))
Example in Haskell:

*Main> split "abcdefghik" 3
("abc", "defghik")
-}
split xs 0 = ([], xs)
split (x:xs) n = let (ys,zs) = split xs (n-1)
                 in (x:ys,zs)

{-| Problem 18
(**) Extract a slice from a list.

Given two indices, i and k, the slice is the list containing the elements
between the i'th and k'th element of the original list (both limits
included). Start counting the elements with 1.

Example:

* (slice '(a b c d e f g h i k) 3 7)
(C D E F G)
Example in Haskell:

*Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"
-}
slice (x:_) 1 1 = [x]
slice (x:xs) 1 n = x : slice xs 1 (n-1)
slice (x:xs) n k = slice xs (n-1) (k-1)

{-| Problem 19
(**) Rotate a list N places to the left.

Hint: Use the predefined functions length and (++).

Examples:

* (rotate '(a b c d e f g h) 3)
(D E F G H A B C)

* (rotate '(a b c d e f g h) -2)
(G H A B C D E F)
Examples in Haskell:

*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"
 
*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"
-}
rotate xs 0 = xs
rotate (x:xs) n
  | n > 0 = rotate (xs++[x]) (n-1)
  | otherwise = rotate (x:xs) ((myLength (x:xs)) + n)
    
{-| Problem 20
(*) Remove the K'th element from a list.

Example in Prolog:

?- remove_at(X,[a,b,c,d],2,R).
X = b
R = [a,c,d]
Example in Lisp:

* (remove-at '(a b c d) 2)
(A C D)
(Note that this only returns the residue list, while the Prolog version also
returns the deleted element.)

Example in Haskell:

*Main> removeAt 1 "abcd"
('b',"acd")
-}
removeAt 0 (x:xs) = (x,xs)
removeAt n (x:xs) = let (y,ys) = removeAt (n-1) xs
                    in (y, x:ys)

{-| Problem 21
Insert an element at a given position into a list.

Example:

* (insert-at 'alfa '(a b c d) 2)
(A ALFA B C D)
Example in Haskell:

P21> insertAt 'X' "abcd" 2
"aXbcd"
-}
insertAt y xs 1 = y:xs
insertAt y (x:xs) n = x : insertAt y xs (n-1)
  
{-| Problem 22
Create a list containing all integers within a given range.

Example:

* (range 4 9)
(4 5 6 7 8 9)
Example in Haskell:

Prelude> range 4 9
[4,5,6,7,8,9]
-}
range i j = [i..j]

{-| Problem 23
Extract a given number of randomly selected elements from a list.

Example:

* (rnd-select '(a b c d e f g h) 3)
(E D A)
Example in Haskell:

Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
eda
-}

rnd_select _ 0 = return []
rnd_select xs n = do
  pos <- randomRIO (0,(length xs) - 1)
  let (x,ys) = removeAt pos xs
  zs <- rnd_select ys (n-1)
  return $ x : zs

{-| Problem 24
Lotto: Draw N different random numbers from the set 1..M.

Example:

* (rnd-select 6 49)
(23 1 17 33 21 37)
Example in Haskell:

Prelude System.Random>diff_select 6 49
Prelude System.Random>[23,1,17,33,21,37]
-}
diff_select n k = rnd_select [1..k] n

{-| Problem 25
Generate a random permutation of the elements of a list.

Example:

* (rnd-permu '(a b c d e f))
(B A D C E F)
Example in Haskell:

Prelude>rnd_permu "abcdef"
Prelude>"badcef"
-}
rnd_permu xs = rnd_select xs (length xs)

{-| Problem 26
(**) Generate the combinations of K distinct objects chosen from the N elements of a list

In how many ways can a committee of 3 be chosen from a group of 12 people? We
all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
well-known binomial coefficients). For pure mathematicians, this result may be
great. But we want to really generate all the possibilities in a list.

Example:

* (combinations 3 '(a b c d e f))
((A B C) (A B D) (A B E) ... )
Example in Haskell:

> combinations 3 "abcdef"
["abc","abd","abe",...]
-}
combinations :: Eq a => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

{-| Problem 27
Group the elements of a set into disjoint subsets.

a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3
and 4 persons? Write a function that generates all the possibilities and returns
them in a list.

Example:

* (group3 '(aldo beat carla david evi flip gary hugo ida))
( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
... )
b) Generalize the above predicate in a way that we can specify a list of group
sizes and the predicate will return a list of groups.

Example:

* (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
... )
Note that we do not want permutations of the group members; i.e. ((ALDO BEAT)
...) is the same solution as ((BEAT ALDO) ...). However, we make a difference
between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

You may find more about this combinatorial problem in a good book on discrete
mathematics under the term "multinomial coefficients".

Example in Haskell:

P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
(altogether 1260 solutions)
 
27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
(altogether 756 solutions)
-}
mygroup :: Eq a => [Int] -> [a] -> [[[a]]]
mygroup [] _ = [[]]
mygroup (n:ns) xs = concatMap (\zs -> map (zs:) $ mygroup ns (xs \\ zs)) $ combinations n xs

{-| Problem 28
Sorting a list of lists according to length of sublists

a) We suppose that a list contains elements that are lists themselves. The
objective is to sort the elements of this list according to their
length. E.g. short lists first, longer lists later, or vice versa.

Example:

* (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
Example in Haskell:

Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
Prelude>["o","de","de","mn","abc","fgh","ijkl"]

b) Again, we suppose that a list contains elements that are lists
themselves. But this time the objective is to sort the elements of this list
according to their length frequency; i.e., in the default, where sorting is done
ascendingly, lists with rare lengths are placed first, others with a more
frequent length come later.

Example:

* (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
Example in Haskell:

lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
["ijkl","o","abc","fgh","de","de","mn"]
-}
lsort = sortBy (comparing length)
lfsort :: [[a]] -> [[a]]
lfsort xs = sortBy (comparing ff) xs
  where
    ff = fromJust . (flip lookup $ dict) . length
    dict = map (head &&& length) . group . sort . map length $ xs
