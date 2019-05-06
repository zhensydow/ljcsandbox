\begin{code}
import Test.HUnit
\end{code}

\begin{code}
tests f = TestList 
          [ TestCase $ 
            assertEqual "Empty Array" (-1) (f 3 [])
          , TestCase $
            assertEqual "1 Element" (-1) (f 3 [1])
          , TestCase $
            assertEqual "1 Element" (0) (f 1 [1])
          , TestCase $
            assertEqual "3 Element" (0) (f 1 [1,3,5])
          , TestCase $
            assertEqual "3 Element" (1) (f 3 [1,3,5])
          , TestCase $
            assertEqual "3 Element" (2) (f 5 [1,3,5])
          , TestCase $
            assertEqual "3 Element" (-1) (f 0 [1,3,5])
          , TestCase $
            assertEqual "3 Element" (-1) (f 2 [1,3,5])
          , TestCase $
            assertEqual "3 Element" (-1) (f 4 [1,3,5])
          , TestCase $
            assertEqual "3 Element" (-1) (f 6 [1,3,5])
          , TestCase $
            assertEqual "4 Element" (0) (f 1 [1,3,5,7])
          , TestCase $
            assertEqual "4 Element" (1) (f 3 [1,3,5,7])
          , TestCase $
            assertEqual "4 Element" (2) (f 5 [1,3,5,7])
          , TestCase $
            assertEqual "4 Element" (3) (f 7 [1,3,5,7])
          , TestCase $
            assertEqual "4 Element" (-1) (f 0 [1,3,5,7])
          , TestCase $
            assertEqual "4 Element" (-1) (f 2 [1,3,5,7])
          , TestCase $
            assertEqual "4 Element" (-1) (f 4 [1,3,5,7])
          , TestCase $
            assertEqual "4 Element" (-1) (f 6 [1,3,5,7])
          , TestCase $
            assertEqual "4 Element" (-1) (f 8 [1,3,5,7])
          ]
\end{code}

\begin{code}
chop0 _ _ = -1
\end{code}

\begin{code}
chop1 _ [] = -1
chop1 i xs = chop1' i 0 (length xs) xs
chop1' i l r xs
    | mid >= length xs = -1
    | xs!!mid == i = mid
    | l >= r = -1
    | xs!!mid > i = chop1' i l mid xs
    | xs!!mid < i = chop1' i (mid+1) r xs
    | otherwise = -1
    where mid = (floor $ (fromIntegral $ r - l) / 2) + l
\end{code}
