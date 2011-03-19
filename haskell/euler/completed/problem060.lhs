%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes 
and concatenating them in any order the result will always be prime. For 
example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four 
primes, 792, represents the lowest sum for a set of four primes with this 
property.

Find the lowest sum for a set of five primes for which any two primes 
concatenate to produce another prime.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Euler( primes, toint, isPrime, digits', combinations )
import Data.Maybe( isJust, isNothing )
import Data.List( find, sort, nub )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
checkjoin xs = isPrime xs1 && isPrime xs2
    where
      xs1 = toint . concat $ strings
      xs2 = toint . concat . reverse $ strings
      strings = map (digits'.show) xs
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
myprimes = take 200 primes
\end{code}

\begin{code}
psetpairs = filter checkjoin $ combinations 2 myprimes
\end{code}

\begin{code}
checkmerge (x:xs) = all checkjoin [[x,y] | y <- xs]
\end{code}

\begin{code}
upgrade pset = nub . map sort . filter checkmerge $ 
               [(x:xs)| x <- myprimes, xs <- pset, isNothing $ find (==x) xs]
\end{code}

\begin{code}
fullcheck :: [Integer] -> Bool
fullcheck = all checkjoin . combinations 2
\end{code}

\begin{code}
initialpsets n = filter fullcheck $ combinations n myprimes
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
myprimes3 = take 100 primes
myprimes4 = take 200 primes
myprimes5 = take 2000 primes
\end{code}

\begin{code}
checkjoin2 a b = checkjoin [a,b]
\end{code}

\begin{code}
posibles3 = [ [a,b,c] | 
              a <- myprimes3
            , b <- filter (checkjoin2 a) $ dropWhile (<=a) myprimes3
            , c <- filter (checkjoin2 b) $ filter (checkjoin2 a) $ dropWhile (<=b) myprimes3 ]
\end{code}

\begin{code}
posibles4 = [ [a,b,c,d] | 
              a <- myprimes4
            , let r1 = filter (checkjoin2 a) $ dropWhile (<=a) myprimes4
            , b <- r1
            , let r2 = filter (checkjoin2 b) $ dropWhile (<=b) r1
            , c <- r2
            , d <- filter (checkjoin2 c) $ dropWhile (<=c) r2 ]
\end{code}

\begin{code}
posibles5 = [ [a,b,c,d,e] | 
              a <- myprimes5
            , let r1 = filter (checkjoin2 a) $ dropWhile (<=a) myprimes5
            , b <- r1
            , let r2 = filter (checkjoin2 b) $ dropWhile (<=b) r1
            , c <- r2
            , let r3 = filter (checkjoin2 c) $ dropWhile (<=c) r2 
            , d <- r3
            , e <- filter (checkjoin2 d) $ dropWhile (<=d) r3 ]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
solution = head $ posibles5
\end{code}

\begin{code}
main = print solution
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%