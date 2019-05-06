%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Data.Array( Array, array, listArray, assocs, elems, (!), (//) )
import Data.List( nub, (\\), isPrefixOf )
import Euler( digits', combinations, toint )
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
mkSudokuGrid = listArray ((0,0),(8,8)) . map convertToElement
\end{code}

\begin{code}
noAssigned :: SudokuGrid -> [(SudokuIdx,[Int])]
noAssigned = filter ((>1).length.snd) . assocs
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
ex01, ex02, ex03, ex04, ex05 :: SudokuGrid
ex01 = stringToSudoku "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
ex02 = stringToSudoku "200080300060070084030500209000105408000000000402706000301007040720040060004010003"
ex03 = iterateRemove ex02
ex04 = stringToSudoku "000000907000420180000705026100904000050000040000507009920108000034059000507000000"
ex05 = stringToSudoku "100920000524010000000000070050008102000000000402700090060000000000030945000071006"
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
extractUniques :: SudokuGrid -> [SudokuIdx] -> [Int]
extractUniques sdk = concat . filter ((==1).length) . map (sdk!)
\end{code}

\begin{code}
extractNplets :: Int -> SudokuGrid -> [SudokuIdx] -> [[Int]]
extractNplets n sdk = filter ((>1).length) . filter ((<=n).length) . map (sdk!)
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
coincidentNplets :: Int -> [[Int]] -> [Int]
coincidentNplets n = nub . concat . filter ((==n).length). map (nub.concat) . combinations n
\end{code}

\begin{code}
rowNpletElems :: Int -> SudokuGrid -> SudokuIdx -> [Int]
rowNpletElems n sdk = coincidentNplets n . extractNplets n sdk . rowIdxs
\end{code}

\begin{code}
colNpletElems :: Int -> SudokuGrid -> SudokuIdx -> [Int]
colNpletElems n sdk = coincidentNplets n . extractNplets n sdk . colIdxs
\end{code}

\begin{code}
squareNpletElems :: Int -> SudokuGrid -> SudokuIdx -> [Int]
squareNpletElems n sdk = coincidentNplets n . extractNplets n sdk . squareIdxs
\end{code}

\begin{code}
forbidenElems :: SudokuGrid -> SudokuIdx -> [Int]
forbidenElems sdk i = nub $ extractUniques sdk indices
    where
      indices = rowIdxs i ++ colIdxs i ++ squareIdxs i
\end{code}

\begin{code}
forbidenNpletElems :: Int -> SudokuGrid -> SudokuIdx -> [Int]
forbidenNpletElems n sdk i = nub $ rowts ++ colts ++ squarets
    where
      rowts = rowNpletElems n sdk i
      colts = colNpletElems n sdk i
      squarets = squareNpletElems n sdk i
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
removeFunction :: (SudokuIdx -> [Int]) -> (SudokuIdx,[Int]) -> (SudokuIdx,[Int])
removeFunction f (i,e) = (i, e \\ f i)
\end{code}

\begin{code}
removeForbidden :: SudokuGrid -> SudokuGrid
removeForbidden sdk = array ((0,0),(8,8)) newassocs
    where
      newassocs = map (removeFunction $ forbidenElems sdk) $ assocs sdk
\end{code}

\begin{code}
removeForbiddenNplets :: Int -> SudokuGrid -> SudokuGrid
removeForbiddenNplets n sdk = array ((0,0),(8,8)) newassocs
    where
      newassocs = map (removeFunction $ forbidenNpletElems n sdk) $ assocs sdk
\end{code}

\begin{code}
iterateRemove :: SudokuGrid -> SudokuGrid
iterateRemove sdk
    | sdk /= newsdk01 = iterateRemove newsdk01
    | sdk /= newsdk02 = iterateRemove newsdk02
    | sdk /= newsdk03 = iterateRemove newsdk03
    | sdk /= newsdk04 = iterateRemove newsdk04
    | sdk /= newsdk05 = iterateRemove newsdk05
    | sdk /= newsdk06 = iterateRemove newsdk06
    | sdk /= newsdk07 = iterateRemove newsdk07
    | sdk /= newsdk08 = iterateRemove newsdk08
    | otherwise = sdk
    where 
      newsdk01 = removeForbidden sdk
      newsdk02 = removeForbiddenNplets 2 newsdk01
      newsdk03 = removeForbiddenNplets 3 newsdk02
      newsdk04 = removeForbiddenNplets 4 newsdk03
      newsdk05 = removeForbiddenNplets 5 newsdk04
      newsdk06 = removeForbiddenNplets 6 newsdk05
      newsdk07 = removeForbiddenNplets 7 newsdk06
      newsdk08 = removeForbiddenNplets 8 newsdk07
\end{code}

\begin{code}
solved :: SudokuGrid -> Bool
solved = all ((==1).length) . elems
\end{code}

\begin{code}
invalid :: SudokuGrid -> Bool
invalid = any ((==0).length) . elems
\end{code}

\begin{code}
backtrackingSolve :: SudokuGrid -> [SudokuGrid]
backtrackingSolve sdk
    | solved sdk = [sdk]
    | invalid sdk = []
    | otherwise = concatMap (backtrackingSolve . iterateRemove) . map apply $ changes
    where
      apply x = sdk // [x]
      changes = [(a,[b]) | (a,r) <- noAssigned sdk, b <- r]
\end{code}

\begin{code}
resolveSudoku :: SudokuGrid -> SudokuGrid
resolveSudoku = head . backtrackingSolve . iterateRemove
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
topLeftCorner :: SudokuGrid -> Integer
topLeftCorner sdk = toint . concatMap (sdk!) $ tlIdx
    where
      tlIdx = [(0,0),(0,1),(0,2)]
\end{code}

\begin{code}
joinN :: Int -> [a] -> [[a]]
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
solution :: IO Integer
solution = do
  suds <- readSudokus "problem096.txt"
  return . sum $ map (topLeftCorner . resolveSudoku) suds
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
main :: IO ()
main = solution >>= print
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
