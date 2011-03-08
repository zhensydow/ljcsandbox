%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Data.Array( Array(..), array, listArray, assocs, elems, (!) )
import Data.List( nub, (\\), isPrefixOf, sort, group )
import Euler( digits' )
import Control.Arrow( (&&&) )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
type SudokuIdx = (Int,Int)
\end{code}

\begin{code}
type SudokuGrid = Array SudokuIdx [Int]
\end{code}

\begin{code}
emptySudokuGrid :: SudokuGrid
emptySudokuGrid = listArray ((0,0),(8,8)) $ repeat [1..9]
\end{code}

\begin{code}
convertToElement :: Int -> [Int]
convertToElement 0 = [1..9]
convertToElement n
    | (n >= 1) || (n <= 9) = [n]
    | otherwise = error $ "element invalid : " ++ show n
\end{code}

\begin{code}
stringToSudoku :: String -> SudokuGrid
stringToSudoku = mkSudokuGrid . digits'
\end{code}

\begin{code}
mkSudokuGrid :: [Int] -> SudokuGrid
mkSudokuGrid xs = listArray ((0,0),(8,8)) $ map convertToElement xs
\end{code}

\begin{code}
ex01 = mkSudokuGrid [
 0, 0, 3, 0, 2, 0, 6, 0, 0,
 9, 0, 0, 3, 0, 5, 0, 0, 1,
 0, 0, 1, 8, 0, 6, 4, 0, 0,
 0, 0, 8, 1, 0, 2, 9, 0, 0,
 7, 0, 0, 0, 0, 0, 0, 0, 8,
 0, 0, 6, 7, 0, 8, 2, 0, 0,
 0, 0, 2, 6, 0, 9, 5, 0, 0,
 8, 0, 0, 2, 0, 3, 0, 0, 9,
 0, 0, 5, 0, 1, 0, 3, 0, 0 ]
\end{code}

\begin{code}
ex02 = stringToSudoku "200080300060070084030500209000105408000000000402706000301007040720040060004010003"
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
extractUniques :: SudokuGrid -> [SudokuIdx] -> [Int]
extractUniques sdk = concat . filter ((==1).length) . map (sdk!)
\end{code}

\begin{code}
extractPairs :: SudokuGrid -> [SudokuIdx] -> [[Int]]
extractPairs sdk = filter ((==2).length) . map (sdk!)
\end{code}

\begin{code}
colIdxs :: SudokuIdx -> [SudokuIdx]
colIdxs (row,col) = [(row,x) | x <- [0..8], x /= col]
\end{code}

\begin{code}
rowIdxs :: SudokuIdx -> [SudokuIdx]
rowIdxs (row,col) = [(x,col) | x <- [0..8], x /=row ]
\end{code}

\begin{code}
squareIdxs :: SudokuIdx -> [SudokuIdx]
squareIdxs i@(row,col) = [(x,y)| x<-[minr..maxr], y<-[minc..maxc], (x,y)/=i]
    where
      minr = baser
      maxr = baser + 2
      minc = basec 
      maxc = basec + 2
      baser = (row `div` 3) * 3
      basec = (col `div` 3) * 3
\end{code}

\begin{code}
rowElems :: SudokuGrid -> SudokuIdx -> [Int]
rowElems sdk = extractUniques sdk . rowIdxs
\end{code}

\begin{code}
colElems :: SudokuGrid -> SudokuIdx -> [Int]
colElems sdk = extractUniques sdk . colIdxs
\end{code}

\begin{code}
squareElems :: SudokuGrid -> SudokuIdx -> [Int]
squareElems sdk = extractUniques sdk . squareIdxs
\end{code}

\begin{code}
coincidentPairs :: [[Int]] -> [Int]
coincidentPairs = nub . concatMap snd . filter ((==2).fst) . map (length &&& head) . group . sort
\end{code}

\begin{code}
rowPairElems :: SudokuGrid -> SudokuIdx -> [Int]
rowPairElems sdk = coincidentPairs . extractPairs sdk . rowIdxs
\end{code}

\begin{code}
forbidenElems :: SudokuGrid -> SudokuIdx -> [Int]
forbidenElems sdk i = nub $ extractUniques sdk indices
    where
      indices = (rowIdxs i) ++ (colIdxs i) ++ (squareIdxs i)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
removeFunction :: SudokuGrid -> (SudokuIdx,[Int]) -> (SudokuIdx,[Int])
removeFunction sdk = \(i,e) -> (i, e \\ forbidenElems sdk i)
\end{code}

\begin{code}
removeForbidden :: SudokuGrid -> SudokuGrid
removeForbidden sdk = array ((0,0),(8,8)) newassocs
    where
      newassocs = map (removeFunction sdk) $ assocs sdk
\end{code}

\begin{code}
iterateRemove :: SudokuGrid -> SudokuGrid
iterateRemove sdk
    | sdk /= newsdk = iterateRemove newsdk
    | otherwise = sdk
    where 
      newsdk = removeForbidden sdk
\end{code}

\begin{code}
solved :: SudokuGrid -> Bool
solved = all ((==1).length) . elems
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
joinN _ [] = []
joinN n xs = take n xs : joinN n (drop n xs)
\end{code}

\begin{code}
readSudokus :: String -> IO [SudokuGrid]
readSudokus = fmap (map stringToSudoku . sudokuStrings) . readFile
    where
      sudokuStrings = map concat . joinN 9 . filter noGridLine . lines
      noGridLine = not . ("Grid" `isPrefixOf`)
\end{code}

\begin{code}
solution = do
  suds <- readSudokus "problem096.txt"
  return $ map (solved . iterateRemove) suds
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
main = do
  sol <- solution
  print sol
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%