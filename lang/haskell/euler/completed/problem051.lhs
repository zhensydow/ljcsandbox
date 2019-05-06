\begin{code}
import Euler( isPrime, primes, digits, toint )
import Data.List( subsequences )
import Control.Arrow( (&&&) )
\end{code}

\begin{code}
myprimes = dropWhile (<56003) primes
\end{code}

\begin{code}
sustitutions :: Int -> [[Int]]
sustitutions = init . tail . subsequences . enumFromTo 1
\end{code}

\begin{code}
applySust' :: [Int] -> t -> Int -> [t] -> [t]
applySust' [] _ _ ys = ys
applySust' _ _ _ [] = []
applySust' (x:xs) d p (y:ys)
  | x==p = d : applySust' xs d (p+1) ys
  | otherwise = y : applySust' (x:xs) d (p+1) ys
                
applySust :: (Integral a) => [Int] -> [a] -> a -> a
applySust xs v d = toint $ applySust' xs d 1 v
\end{code}

\begin{code}
check n = map (length &&& head) 
          $ filter (not.null) 
          $ map (filter ((==ll).length.digits) . checkSust ds) (sustitutions ll)
  where
    ds = digits n
    ll = length ds
  
checkSust ds xs = filter isPrime $ map (applySust xs ds) [0..9]
\end{code}

\begin{code}
solution = head . dropWhile ((<8).fst) . concatMap check $ myprimes
\end{code}

\begin{code}
main = print solution
\end{code}
