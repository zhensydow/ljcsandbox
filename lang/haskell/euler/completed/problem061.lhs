\begin{code}
import Euler( digits )
import Data.List(delete)
import Control.Arrow( (&&&) )
\end{code}

\begin{code}
permute [] = [[]]
permute xs = concatMap (\x -> map (x:) $ permute $ delete x xs) xs
\end{code}

\begin{code}
numbers = [1000 ..9999]
\end{code}

\begin{code}
polygonal :: Int -> Int -> Int
polygonal s n = round $ ((fs/2)-1)*(fn**2) - ((fs/2)-2)*fn
    where 
      fs = fromIntegral s
      fn = fromIntegral n
\end{code}

\begin{code}
candidates s = map (const s &&& id) . takeWhile (<10000) . dropWhile (<1000) . map (polygonal s) $ [1..]
\end{code}

\begin{code}
joinpair p1@(s1,n1) p2@(s2,n2)
    | s1 == s2 = []
    | otherwise = joinpair1 ++ joinpair2
    where
      joinpair1 
          | ln1 == hn2 = [[(s1,n1), (s2,n2)]]
          | otherwise = []
      joinpair2
          | hn1 == ln2 = [[(s2,n2), (s1,n1)]]
          | otherwise = []
      hn1 = take 2 $ digits n1
      hn2 = take 2 $ digits n2
      ln1 = drop 2 $ digits n1
      ln2 = drop 2 $ digits n2
\end{code}

\begin{code}
allCandidates = concatMap candidates [3..8]
\end{code}

\begin{code}
ispaired p1@(s1,n1) p2@(s2,n2)
    | s1 == s2 = False
    | otherwise = ln1 == hn2
    where
      ln1 = drop 2 $ digits n1
      hn2 = take 2 $ digits n2
\end{code}

\begin{code}
iscycled xs = ispaired (last xs) (head xs)
\end{code}

\begin{code}
isalready _ [] = False
isalready s1 ((s2,_):xs) 
    | s1 == s2 = True
    | otherwise = isalready s1 xs
\end{code}

\begin{code}
upgrade xss = [ y : xs | xs <- xss, y <- allCandidates, not $ isalready (fst y) xs, ispaired y (head xs)]
\end{code}

\begin{code}
fase2 = concat [joinpair x y | x <- allCandidates, y <- allCandidates ]
fase3 = upgrade fase2
fase4 = upgrade fase3
fase5 = upgrade fase4
fase6 = upgrade fase5
\end{code}

\begin{code}
solution = sum . map snd . head . filter iscycled $ filter ((==6).length) fase6
\end{code}

\begin{code}
main = print solution
\end{code}
