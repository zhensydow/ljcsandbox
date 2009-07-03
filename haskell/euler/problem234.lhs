\begin{code}
divides n p = n `mod` p == 0
\end{code}

\begin{code}
primes :: [Integer]
primes = 2:3:primes'
  where
    1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
    primes' = p : filter isPrime' candidates
    isPrime' n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'
\end{code}

\begin{code}
isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes
\end{code}

\begin{code}
lps n = head $ dropWhile (not.isPrime) [sn,(sn-1)..]
    where sn = floor $ sqrt $ fromInteger n
\end{code}

\begin{code}
ups n = head $ dropWhile (not.isPrime) [sn..]
    where sn = ceiling $ sqrt $ fromInteger n
\end{code}

\begin{code}
lups n = (head $ dropWhile (not.isPrime) [ln,(ln-1)..]
         ,head $ dropWhile (not.isPrime) [un..])
    where ln = floor sn
          un = ceiling sn
          sn = sqrt $ fromInteger n
\end{code}

\begin{code}
iters = (4,2,2) : next' 5 nm nlp nup
    where (nlp,nup) = lups 5
          sn = ceiling $ sqrt $ fromInteger 5
          nm = sn^2 - 1
next' n m lp up
    | n == m = (n,lp,up) : (n+1,lp1,lp1) : next' (n+2) nm nlp nup
    | otherwise = (n, lp, up) : next' (n+1) m lp up
    where 
      lp1 = lps (n+1)
      (nlp,nup) = lups (n+2)
      sn = ceiling $ sqrt $ fromInteger (n+2)
      nm = sn^2 - 1
\end{code}

\begin{code}
semidivs (n,lp,up) = (divides n lp) /= (divides n up)
\end{code}

\begin{code}
sumsemidivs n = sum $ map (\(a,_,_) -> a) $ filter semidivs $ takeWhile (\(a,_,_)-> a <= n) iters
\end{code}

\begin{code}
solution = sumsemidivs 1000000
--solution = sumsemidivs 999966663333
\end{code}

\begin{code}
main = print solution
\end{code}
