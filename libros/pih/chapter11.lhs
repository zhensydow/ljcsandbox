\begin{code}
import Data.List
\end{code}

\begin{code}
data Op = Add | Sub | Mul | Div
          deriving( Show )
\end{code}

\begin{code}
valid :: Op -> Int -> Int -> Bool
--valid Add x y = x <= y
valid Add _ _ = True
valid Sub x y = x > y
valid Sub _ _ = True
--valid Mul x y = x != 1 && y != 1 && 
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
\end{code}

\begin{code}
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
\end{code}

\begin{code}
data Expr = Val Int | App Op Expr Expr
            deriving( Show )
\end{code}

\begin{code}
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r
\end{code}

\begin{code}
eval :: Expr -> [Int]
eval (Val n) = [ n | n > 0 ]
eval (App o l r) = [apply o x y | 
                    x <- eval l,
                    y <- eval r,
                    valid o x y]
\end{code}

\begin{code}
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
    where yss = subs xs
\end{code}

\begin{code}
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys):(map (y:) (interleave x ys))
\end{code}

\begin{code}
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))
\end{code}

\begin{code}
choices :: [a] -> [[a]]
--choices xs = concat (map perms (subs xs))
choices xs =[y| x <- subs xs, y <- perms x]
\end{code}

\begin{code}
isChoice :: Ord a => [a] -> [a] -> Bool
isChoice xs ys = isChoice' (sort xs) (sort ys)

isChoice' :: Ord a => [a] -> [a] -> Bool
isChoice' [] _ = True
isChoice' (_:_) [] = False
isChoice' (x:xs) (y:ys)
    | x < y = False
    | x > y = isChoice' (x:xs) ys
    | otherwise = isChoice' xs ys
\end{code}

\begin{code}
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs):[(x:ls,rs)|(ls,rs)<-split xs]
\end{code}

\begin{code}
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [ e | 
             (ls,rs) <- split ns,
             l <- exprs ls,
             r <- exprs rs,
             e <- combine l r]
\end{code}

\begin{code}
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]
ops :: [Op]
ops = [Add,Sub,Mul,Div]
\end{code}

\begin{code}
all4_0 = concat $ map exprs $ choices [1,3,7,10,25,50]
test4_1 = length all4_0
test4_2 = length $ concat $ map eval all4_0
\end{code}
