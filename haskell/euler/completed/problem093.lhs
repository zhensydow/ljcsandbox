By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and making use of the four arithmetic operations (+, , *, /) and brackets/parentheses, it is possible to form different positive integer targets.

For example,

8 = (4 * (1 + 3)) / 2
14 = 4 * (3 + 1 / 2)
19 = 4 * (2 + 3)  1
36 = 3 * 4 * (2 + 1)

Note that concatenations of the digits, like 12 + 34, are not allowed.

Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target numbers of which 36 is the maximum, and each of the numbers 1 to 28 can be obtained before encountering the first non-expressible number.

Find the set of four distinct digits, a  b < c  d, for which the longest set of consecutive positive integers, 1 to n, can be obtained, giving your answer as a string: abcd.

\begin{code}
import Data.Ratio( Ratio, (%) )
import Data.Maybe( mapMaybe, isJust )
import Data.List( find, sortBy )
import Data.Ord( comparing )
import Euler( combinations )
import Control.Arrow( (&&&) )
\end{code}

\begin{code}
data ArithmOp = ADD
              | SUB
              | MUL
              | DIV
                deriving( Show )
data ArithmElem = Digit Int
                | Op ArithmOp
                  deriving( Show )
\end{code}

\begin{code}
type ArithExpresion = [ArithmElem]
\end{code}

\begin{code}
isOperator (Op _) = True
isOperator _ = False
\end{code}

\begin{code}
numOps :: ArithExpresion -> Int
numOps = length .  filter isOperator
\end{code}

\begin{code}
evaluate :: ArithExpresion -> Maybe (Ratio Int)
evaluate = evaluate' []
\end{code}

\begin{code}
evaluate' [] [] = error "empty stack at end"
evaluate' (x:_) [] = x
evaluate' xs ((Digit n):ys) = evaluate' ((Just $ fromIntegral n) : xs) ys
evaluate' [] ((Op _):_) = error "empty stack 1"
evaluate' (x:[]) ((Op _):_) = error "empty stack 2"
evaluate' (x:y:xs) ((Op o):ys) = do
  valx <- x
  valy <- y
  evaluate' (operate o valx valy : xs) ys
\end{code}

\begin{code}
operate :: ArithmOp -> Ratio Int -> Ratio Int -> Maybe (Ratio Int)
operate ADD x y = Just (x + y)
operate MUL x y = Just (x * y)
operate SUB x y = Just (x - y)
operate DIV x y 
    | y == 0 = Nothing
    | otherwise = Just (x / y)
\end{code}

\begin{code}
generate xs = [ [a,b,c,d,e,f,g] |
             a <- dgs, b <- dgs,
             differentDigit a b,

             c <- elems,
             c `notDigitIn` [a,b],

             d <- if isOperator c then dgs else elems,
             d `notDigitIn` [a,b,c],

             e <- elems,
             e `notDigitIn` [a,b,c,d],

             f <- if numOps [c,d,e] == 2 then dgs else elems,
             f `notDigitIn` [a,b,c,d,e],

             g <- ops
           ]
    where
      dgs = fmap Digit xs
      ops = fmap Op [ADD,SUB,MUL,DIV]
      elems = dgs ++ ops
\end{code}

\begin{code}
differentDigit (Digit a) (Digit b) = a /= b
differentDigit _ _ = True
\end{code}

\begin{code}
notDigitIn d = all id . map (differentDigit d)
\end{code}

\begin{code}
findResult xs n = find (maybe False (==n). evaluate) $ generate xs
\end{code}

\begin{code}
countN xs = length $ takeWhile isJust $ map (findResult xs) [1..]
\end{code}

\begin{code}
solution = last . sortBy (comparing snd) $ map (id &&& countN) (combinations 4 [1..9])
\end{code}

\begin{code}
main = print solution
\end{code}
