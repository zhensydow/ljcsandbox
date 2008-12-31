\begin{code}
import Prelude(drop,take,zip,tail,head,dropWhile,div
              ,($),(+),(<),(.),(++)
              ,Integer,Int,Show(..))
\end{code}

\begin{code}
fibs :: [Integer]
fibs = 0:1:[x+y|(x,y)<-zip fibs (tail fibs)]
\end{code}

\begin{code}
fib :: Int -> Integer
fib n = head $ drop n $ take (n+1) fibs
\end{code}

\begin{code}
fibGreatThan :: Integer -> Integer
fibGreatThan n = head $ dropWhile (<n) fibs
\end{code}

\begin{code}
data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Show a => Show (Tree a) where
    show Leaf = "[X]"
    show (Node l x r) = "[" ++ (show l) ++ "," 
                        ++ (show x) 
                        ++ "," ++ (show r) ++ "]"
\end{code}

\begin{code}
repeat :: a -> Tree a
repeat x = Node (repeat x) x (repeat x)
\end{code}

\begin{code}
taket :: Int -> Tree a -> Tree a
taket 0 _ = Leaf
taket _ Leaf = Leaf
taket (n+1) (Node l x r) = Node (taket midl l) x (taket midr r)
    where 
      midl = (n+1) `div` 2
      midr = n `div` 2
\end{code}

\begin{code}
replicate :: Int -> a -> Tree a
replicate n = taket n . repeat
\end{code}
