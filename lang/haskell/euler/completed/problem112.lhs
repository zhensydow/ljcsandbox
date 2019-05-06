Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number; for example, 134468.

Similarly if no digit is exceeded by the digit to its right it is called a decreasing number; for example, 66420.

We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number; for example, 155349.

Clearly there cannot be any bouncy numbers below one-hundred, but just over half of the numbers below one-thousand (525) are bouncy. In fact, the least number for which the proportion of bouncy numbers first reaches 50% is 538.

Surprisingly, bouncy numbers become more and more common and by the time we reach 21780 the proportion of bouncy numbers is equal to 90%.

Find the least number for which the proportion of bouncy numbers is exactly 99%.

\begin{code}
import Euler( digits )
\end{code}

\begin{code}
bouncing :: Integer -> Bool
bouncing n = not (all (==1) signs || all (==(-1)) signs)
    where 
      xs = digits n
      differences = zipWith (-) (tail xs) xs
      signs = map signum . filter (/= 0) $ differences
\end{code}

\begin{code}
iterateBouncing :: Integer -> Double -> Double -> Integer
iterateBouncing n b l
    | b == 0 = iterateBouncing (n+1) nextb l
    | (b / fromInteger n) >= l = n
    | otherwise = iterateBouncing (n+1) nextb l
    where
      nextb = if bouncing (n+1) then b+1 else b
\end{code}

\begin{code}
solution :: Integer
solution = iterateBouncing 100 0.0 0.99
\end{code}

\begin{code}
main :: IO ()
main = print solution
\end{code}
