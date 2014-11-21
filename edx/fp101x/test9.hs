{-# LANGUAGE NPlusKPatterns #-}
import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
    | Succ Nat
      deriving Show

--natToInteger Zero = 1
--natToInteger (Succ n) = 1 + natToInteger n
--natToInteger = head . m
--    where m Zero = [0]
--          m (Succ n) = [sum [x | x <- (1: m n)]]

natToInteger :: Nat -> Integer
natToInteger = \n -> genericLength [c | c <- show n, c == 'S']

integerToNat (n+1) = let m = integerToNat n in Succ m
integerToNat 0 = Zero

--integerToNat n = product [(unsafeCoerce c) :: Integer | c <- show n]

-- integerToNat = head . m
--     where {
--           ; m 0 = [0]
--           ; m (n + 1) = [sum [x | x <- (1:m n )]]
--           }
--integerToNat :: Integer -> Nat
--integerToNat = \n -> genericLength [c | c <- show n, isDigit c]

add n (Succ m) = Succ (add m n)
add n Zero = n

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

data Tree1 = Leaf1 Integer
          | Node1 Tree1 Integer Tree1
            deriving Show

occurs :: Integer -> Tree1 -> Bool
occurs m (Leaf1 n) = m == n
occurs m (Node1 l n r) = case compare m n of
                           LT -> occurs m l
                           EQ -> True
                           GT -> occurs m r

tree1a = Leaf1 10
tree1b = Node1 (Node1 (Leaf1 1) 2 (Leaf1 3)) 5 (Leaf1 7)

data Tree2 = Leaf2 Integer
           | Node2 Tree2 Tree2
             deriving Show

balanced :: Tree2 -> Bool
balanced (Leaf2 _) = True
balanced (Node2 l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

leaves (Leaf2 _) = 1
leaves (Node2 l r) = leaves l + leaves r

tree2a =  Node2 (Node2 (Leaf2 1) (Leaf2 3)) (Leaf2 7)
tree2b =  Node2 (Node2 (Leaf2 1) (Node2 (Leaf2 3) (Leaf2 4))) (Leaf2 7)

halve xs = splitAt (length xs `div` 2) xs

balance :: [Integer] -> Tree2
balance [x] = Leaf2 x
balance xs = Node2 (balance ys) (balance zs)
    where (ys,zs) = halve xs

