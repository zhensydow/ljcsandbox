\begin{code}
import Data.List
\end{code}

\begin{code}
data Nat = Zero | Succ Nat
\end{code}

\begin{code}
instance Show Nat where
    show = show.nat2int
\end{code}

\begin{code}
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat (n+1) = Succ (int2nat n)
int2nat _ = error ""
\end{code}

\begin{code}
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n
\end{code}

\begin{code}
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)
\end{code}

\begin{code}
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult m (Succ Zero) = m
mult (Succ Zero) m = m
mult (Succ n) m = add m (mult n m)
\end{code}

\begin{code}
data BTree = BLeaf Int | BNode BTree Int BTree
\end{code}

\begin{code}
t :: BTree
t = BNode (BNode (BLeaf 1) 3 (BLeaf 4)) 5 (BNode (BLeaf 6) 7 (BLeaf 9)) 
\end{code}

\begin{code}
occurs :: Int -> BTree -> Bool
occurs m (BLeaf n) = m==n
occurs m (BNode l n r) = select (compare m n)
    where
      select EQ = True
      select LT = occurs m l
      select GT = occurs m r
\end{code}

\begin{code}
data Tree = Leaf Int | Node Tree Tree
            deriving( Show )
\end{code}

\begin{code}
leaves :: Tree -> Int
leaves (Leaf _) = 1
leaves (Node l r) = (leaves l) + (leaves r)
\end{code}

\begin{code}
balanced :: Tree -> Bool
balanced (Leaf _) = True 
balanced (Node l r) = (balanced l) && (balanced r) && (diff <= 1)
    where
      diff = abs ((leaves l) - (leaves r))
\end{code}

\begin{code}
split :: [a] -> ([a],[a])
split [] = error "can't split empty list"
split [_] = error "can't split"
split xs = (take mid xs, drop mid xs)
    where
      mid = div (length xs) 2
\end{code}

\begin{code}
balance :: [Int] -> Tree
balance [x] = Leaf x
balance xs = Node (balance a) (balance b)
    where
      (a,b) = split xs
\end{code}

Tautology Checker

\begin{code}
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop

instance Show Prop where
    show (Const True) = "T"
    show (Const False) = "F"
    show (Var n) = [n]
    show (Not p) = "¬ " ++ (show p)
    show (And p q) = (show p) ++ " ^ " ++ (show q)
    show (Or p q) = (show p) ++ " v " ++ (show q)
    show (Imply p q) = (show p) ++ " -> " ++ (show q)
\end{code}

\begin{code}
type Assoc k v = [(k,v)]
\end{code}

\begin{code}
findAssoc :: Eq k => k -> Assoc k v -> v
findAssoc k t = head [v | (k',v) <- t, k == k']
\end{code}

\begin{code}
type Subst = Assoc Char Bool
\end{code}

\begin{code}
p1, p2, p3, p4 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
\end{code}

\begin{code}
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = findAssoc x s
eval s (Not p) = not (eval s p)
eval s (And p q) = (eval s p) && (eval s q)
eval s (Or p q) = (eval s p) || (eval s q)
eval s (Imply p q) = (eval s p) <= (eval s q)
\end{code}

\begin{code}
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
\end{code}

\begin{code}
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools (n+1) = map (False:) bss ++ map (True:) bss
    where
      bss = bools n
\end{code}

\begin{code}
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/=x) xs)
\end{code}

\begin{code}
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools $ length vs)
    where
      vs = rmdups $ vars p
\end{code}

\begin{code}
isTaut :: Prop -> Bool
isTaut p = and [eval s p| s <- substs p]
\end{code}

Abstract Machine

\begin{code}
data Expr = Val Int | Add Expr Expr | Mul Expr Expr
instance Show Expr where
    show (Val n) = show n
    show (Add p q) = "(" ++ (show p) ++ "+" ++ (show q) ++ ")"
    show (Mul p q) = "(" ++ (show p) ++ "*" ++ (show q) ++ ")"
\end{code}

\begin{code}
value (Val n) = n
value (Add p q) = (value p) + (value q)
value (Mul p q) = (value p) * (value q)
\end{code}

\begin{code}
type Cont = [Op]
data Op = EVALA Expr | EVALM Expr | ADD Int | MUL Int
\end{code}

\begin{code}
evale :: Expr -> Cont -> Int
evale (Val n) xs = exec xs n
evale (Add x y) xs = evale x (EVALA y:xs)
evale (Mul x y) xs = evale x (EVALM y:xs)
\end{code}

\begin{code}
exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALA x:xs) n = evale x (ADD n:xs)
exec (EVALM x:xs) n = evale x (MUL n:xs)
exec (ADD x:xs) n = exec xs (x + n)
exec (MUL x:xs) n = exec xs (x * n)
\end{code}

\begin{code}
valuee e = evale e []
\end{code}

\begin{code}
exp1 = Add (Add (Val 2) (Val 3)) (Val 4)
exp2 = Add (Mul (Val 2) (Val 3)) (Val 4)
\end{code}

\begin{code}
class MiMonad m where
    devuelve :: a -> m a
    bind :: m a -> (a -> m b)  -> m b
\end{code}

\begin{code}
instance MiMonad Maybe where
    devuelve x = Just x
    bind Nothing _ = Nothing
    bind (Just x) f = f x
\end{code}

\begin{code}
instance MiMonad [] where
    devuelve x = [x]
    bind xs f = concatMap f xs
\end{code}
