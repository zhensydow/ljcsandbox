%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import qualified Data.Set as DS( Set(..), filter, size, fromList )
import Euler( digits, permutations, isPermutation )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
findPermutations :: Eq a => [a] -> [([a],b)] -> [[a]]
findPermutations x xs = map fst $ filter ((isPermutation x) . fst) xs
\end{code}

\begin{code}
countPerms :: Eq a => [a] -> [([a],Int)] -> Int
countPerms _ [] = 0
countPerms xs ((ys,n):zs)
    | isPermutation xs ys = n
    | otherwise = countPerms xs zs
\end{code}

\begin{code}
generateCubes :: Integer -> [([Int],Int)] -> [([Int],Int)]
generateCubes n xs = x : generateCubes (n+1) (x:xs)
    where 
      x = (ndigits, count + 1)
      ndigits = digits $ n ^ 3
      count = countPerms ndigits xs
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
numperms = 5
solution = head $ findPermutations greatCube minorcubes
greatCube = fst . head . dropWhile ((<numperms).snd) $ allcubes
allcubes = generateCubes 0 []
minorcubes = takeWhile ((<numperms).snd) allcubes
\end{code}

\begin{code}
main = print solution
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%