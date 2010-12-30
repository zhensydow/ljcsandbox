\begin{code}
import Euler( fact )
import Control.Arrow( (&&&) )
\end{code}

\begin{code}
binom :: Int -> Int -> Int
binom n k = round $ binom' (fromIntegral n) (fromIntegral k)
binom' n 0 = 1
binom' 0 k = 0
binom' n k = (binom' (n-1) (k-1)) * (n / k)
\end{code}

\begin{code}
kmax :: Int -> Int -> Int -> Int
kmax s n p = floor $ (fromIntegral (p - n)) / (fromIntegral s)
\end{code}

\begin{code}
c s n p = sum [(sign k) * (binom n k) * (binom (p - s*k - 1) (n - 1)) | k <- [0 .. kmax s n p]]
    where
      sign :: Int -> Int
      sign k = round $ (-1) ** (fromIntegral k)
\end{code}

\begin{code}
numWith9x4 = c 4 9
numWith6x6 = c 6 6
\end{code}

\begin{code}
num9x4great x = sum $ map numWith9x4 [x+1 .. 36]
\end{code}

\begin{code}
wins :: Integer
wins = sum . (uncurry $ zipWith (*)) . (map (toInteger . numWith6x6) &&& map (toInteger.num9x4great)) $ [6..35]
\end{code}

\begin{code}
total :: Integer
total = 6^6 * 4^9
\end{code}

\begin{code}
solution :: Double
solution = (fromInteger wins) / (fromInteger total)
\end{code}

\begin{code}
main = print $ (fromIntegral (round $ solution * 10000000)) / 10000000
\end{code}
