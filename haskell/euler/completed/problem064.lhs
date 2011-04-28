All square roots are periodic when written as continued fractions and can be written in the form:

[...]

It can be seen that the sequence is repeating. For conciseness, we use the notation 23 = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats indefinitely.

The first ten continued fraction representations of (irrational) square roots are:

2=[1;(2)], period=1
3=[1;(1,2)], period=2
5=[2;(4)], period=1
6=[2;(2,4)], period=2
7=[2;(1,1,1,4)], period=4
8=[2;(1,4)], period=2
10=[3;(6)], period=1
11=[3;(3,6)], period=2
12= [3;(2,6)], period=2
13=[3;(1,1,1,1,6)], period=5

Exactly four continued fractions, for N  13, have an odd period.

How many continued fractions for N  10000 have an odd period?

\begin{code}
import Data.Ratio(Ratio )
import Data.List( (\\) )
\end{code}

\begin{code}
approximation :: (Int,[Int]) -> Int -> Ratio Integer
approximation (b,[]) _ = (fromIntegral b)
approximation (b,_) 0 = (fromIntegral b)
approximation (b,x:xs) n = (fromIntegral b) + ((fromIntegral 1) / (approximation (x,xs) (n-1)))
\end{code}

\begin{code}
continuedFraction :: Int -> Int -> Int -> [Int]
continuedFraction r n0 d0 = m : continuedFraction r n1 d1
    where
      m = (truncate (sqrt (fromIntegral r)) + n0) `div` d0
      a = n0 - d0 * m
      n1 = (-a)
      d1 = (r - a ^ 2) `div` d0
\end{code}

We get last one based in property it must be double the first number
\begin{code}
checkConti :: Int -> (Int, [Int])
checkConti s = (x,xs)
    where
      (x:xs') = continuedFraction s 0 1
      lll = length $ takeWhile (/=(2*x)) xs'
      xs = take (lll+1) xs'
\end{code}

\begin{code}
solution :: Int
solution = length . filter odd . map (length . snd . checkConti) $ irrationals
\end{code}

\begin{code}
rationals, irrationals :: [Int]
rationals = takeWhile (<=10000) [x*x | x <- [1..]]
irrationals = [1..10000] \\ rationals
\end{code}

\begin{code}
main :: IO ()
main = print solution
\end{code}
