Let pn be the nth prime: 2, 3, 5, 7, 11, ..., and let r be the remainder when (pn1)n + (pn+1)n is divided by pn2.

For example, when n = 3, p3 = 5, and 43 + 63 = 280  5 mod 25.

The least value of n for which the remainder first exceeds 109 is 7037.

Find the least value of n for which the remainder first exceeds 1010.

\begin{code}
import Euler( primes )
import Control.Arrow( (&&&) )
\end{code}

\begin{code}
calc_1 :: Integer -> Integer -> Integer
calc_1 0 _ = 0
calc_1 p n = (a^n + b^n) `mod` (p^2)
    where
      a = p - 1
      b = p + 1
\end{code}

\begin{code}
calc_2 :: Integer -> Integer -> Integer
calc_2 0 _ = 0
calc_2 p n = (a^n + b^n) `mod` (p^2)
    where
      a = p - 1
      b = p + 1
\end{code}

\begin{code}
pnes = zip [1..] primes
\end{code}

\begin{code}
solution = head $ dropWhile ((<10^10).snd) $ map (fst &&& (\(a,b)-> calc_1 b a)) $ pnes
\end{code}

\begin{code}
main = print solution
\end{code}
