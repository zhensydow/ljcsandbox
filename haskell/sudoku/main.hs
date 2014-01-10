{-# LANGUAGE TupleSections #-}
-- -----------------------------------------------------------------------------
import Control.Arrow( second )
import Control.Monad( void )
import Data.Array( Array, listArray, assocs, elems, indices, (//), (!) )
import Data.Char( digitToInt )
import Data.List( nub, sort, group, foldl', union )
import qualified Data.Map.Strict as M

-- -----------------------------------------------------------------------------
type SudokuCell = [Int]
type Sudoku = Array (Int,Int) SudokuCell

-- -----------------------------------------------------------------------------
mkEmptyCell :: SudokuCell
mkEmptyCell = []

mkFullCell :: SudokuCell
mkFullCell = [1..9]

{-| Make an Sudoku cell from a fixed value or zero.

>>> mkSudokuCell 1
[1]

>>> mkSudokuCell 0
[1,2,3,4,5,6,7,8,9]
-}
mkSudokuCell :: Integral a => a -> SudokuCell
mkSudokuCell x
  | x == 0 = mkFullCell
  | x >= 1 && x <= 9 = [fromIntegral x]
  | otherwise = mkEmptyCell

mkEmptySudoku :: Sudoku
mkEmptySudoku = listArray ((1,1),(9,9)) $ repeat mkEmptyCell

mkFullSudoku :: Sudoku
mkFullSudoku = listArray ((1,1),(9,9)) $ repeat mkFullCell

-- -----------------------------------------------------------------------------
initial01 :: Array (Int, Int) Int
initial01 = listArray ((1,1),(9,9)) [ 3,0,0, 0,0,8, 0,0,0,
                                      0,9,0, 0,6,2, 7,8,0,
                                      5,0,0, 0,0,0, 6,0,0,

                                      0,0,0, 0,7,0, 3,5,0,
                                      0,0,0, 6,0,9, 0,0,0,
                                      0,1,7, 0,5,0, 0,0,0,

                                      0,0,2, 0,0,0, 0,0,4,
                                      0,6,4, 7,9,0, 0,2,0,
                                      0,0,0, 4,0,0, 0,0,6 ]

ex01, ex01' :: Sudoku
ex01 = sudokuFromInitial initial01
ex01' = fullIterate ex01

ex03:: Sudoku
ex03 = mkFullSudoku // [ ((2,2),[3,4]), ((2,3),[3,4]), ((1,2),[6,4]) ]

-- -----------------------------------------------------------------------------
sudokuFromInitial :: Integral a => Array (Int,Int) a -> Sudoku
sudokuFromInitial = (mkEmptySudoku //) . changes
  where changes = fmap (second mkSudokuCell) . assocs

-- -----------------------------------------------------------------------------
sudokuFromString :: String -> Sudoku
sudokuFromString = sudokuFromInitial . listArray ((1,1),(9,9))
                   . take (9*9) . (++ repeat 0) . map digitToInt

-- -----------------------------------------------------------------------------
showLine :: String
showLine = replicate (4 * 9 + 1) '-'

showSudokuCell :: SudokuCell -> String
showSudokuCell [] = "   "
showSudokuCell [x] = " " ++ show x ++ " "
showSudokuCell [x,y] = show x ++ " " ++ show y
showSudokuCell [x,y,z] = show x ++ show y ++ show z
showSudokuCell _ = "..."

showSudokuLine :: [SudokuCell] -> String
showSudokuLine = ("|" ++) . concat . fmap ((++"|") . showSudokuCell)

showSudoku :: Sudoku -> [String]
showSudoku = (showLine :) . (++ [showLine])
             . fmap showSudokuLine . split9 . elems

split9 :: [a] -> [[a]]
split9 [] = []
split9 xs = y : split9 ys
  where (y,ys) = splitAt 9 xs

printSudoku :: Sudoku -> IO ()
printSudoku = mapM_ putStrLn . showSudoku

-- -----------------------------------------------------------------------------
boxIdx :: Int -> Int
boxIdx n = ((n - 1) `div` 3) + 1

lowerIdx :: Int -> Int
lowerIdx n = boxIdx n * 3 - 2

horizontalIndices :: (Int, Int) -> [(Int, Int)]
horizontalIndices (y,x) = [(y,i) | i <- [1..9], i /= x ]
verticalIndices :: (Int, Int) -> [(Int, Int)]
verticalIndices (y,x) = [(j,x) | j <- [1..9], j /= y ]
boxIndices :: (Int, Int) -> [(Int, Int)]
boxIndices (y,x) = [(j,i) | j <- [ly .. ly + 2], i <- [lx .. lx + 2]
                          , (j,i) /= (y,x) ]
  where
    ly = lowerIdx y
    lx = lowerIdx x

-- -----------------------------------------------------------------------------
uniques :: Ord a => [a] -> [a]
uniques = nub . sort

filterLen :: Int -> [[a]] -> [[a]]
filterLen n = filter ((==n) . length)

groupCells :: [SudokuCell] -> [[SudokuCell]]
groupCells = group . sort . fmap sort

getSingletons :: Sudoku -> [(Int, Int)] -> [Int]
getSingletons sudoku = uniques . concat . filterLen 1 . fmap (sudoku!)

getPairs :: Sudoku -> [(Int, Int)] -> [Int]
getPairs sudoku = uniques . concat . fmap head
                  . filterLen 2 . groupCells
                  . filterLen 2 . fmap (sudoku!)

appendUnion :: [Int] -> [Int] -> [[Int]] -> [[Int]]
appendUnion new key old = if key `union` new == key then new:old else old

getTriples :: Sudoku -> [(Int, Int)] -> [Int]
getTriples sudoku xs = uniques . concat . concat
                       . filterLen 3 . M.elems $ fillupBuckets
  where
    cells = fmap (sudoku!) $ xs
    initialBuckets = M.fromList . fmap ((,[]) . head) . groupCells . filterLen 3
                     $ cells
    bucketElems = fmap sort . filter ((\a -> a>=2 && a <=3) . length) $ cells
    fillupBuckets = foldl' (\buckets x -> M.mapWithKey (appendUnion x) buckets)
                    initialBuckets $ bucketElems

getUniques :: Sudoku -> [(Int, Int)] -> [Int]
getUniques sudoku xs = uniques $ singletons ++ pairs ++ triples
  where
    singletons = getSingletons sudoku xs
    pairs = getPairs sudoku xs
    triples = getTriples sudoku xs

getUniquesCell :: Sudoku -> (Int, Int) -> [Int]
getUniquesCell sudoku idx = uniques $ hUniques ++ vUniques ++ bUniques
  where
    hUniques = getUniques sudoku (horizontalIndices idx)
    vUniques = getUniques sudoku (verticalIndices idx)
    bUniques = getUniques sudoku (boxIndices idx)

removeUniquesCell :: Sudoku -> (Int, Int) -> Sudoku
removeUniquesCell sudoku idx = sudoku // [(idx, filter filterFun $ sudoku ! idx)]
  where filterFun = (`notElem` getUniquesCell sudoku idx)

-- -----------------------------------------------------------------------------
getLikely :: Sudoku -> [(Int, Int)] -> SudokuCell
getLikely sudoku = uniques . concat . fmap (sudoku!)

getOwnCell :: Sudoku -> (Int, Int) -> SudokuCell
getOwnCell sudoku idx = take 1 $ gethOwn ++ getvOwn ++ getbOwn
  where
    owns = sudoku ! idx
    gethOwn = filter (`notElem` getLikely sudoku (horizontalIndices idx)) owns
    getvOwn = filter (`notElem` getLikely sudoku (verticalIndices idx)) owns
    getbOwn = filter (`notElem` getLikely sudoku (boxIndices idx)) owns

setOwnCell :: Sudoku -> (Int, Int) -> Sudoku
setOwnCell sudoku idx = sudoku // fmap (\x -> (idx,[x])) own
  where
    own = getOwnCell sudoku idx

-- -----------------------------------------------------------------------------
iterateRemove :: Sudoku -> Sudoku
iterateRemove sudoku = foldl' removeUniquesCell sudoku (indices sudoku)

iterateLikely :: Sudoku -> Sudoku
iterateLikely sudoku = foldl' setOwnCell sudoku (indices sudoku)

fullIterate :: Sudoku -> Sudoku
fullIterate sudoku
  | newSudoku == sudoku = newSudoku
  | otherwise = fullIterate newSudoku
  where newSudoku = iterateLikely . iterateRemove $ sudoku

-- -----------------------------------------------------------------------------
showFullIterate :: Sudoku -> IO Sudoku
showFullIterate sudoku = do
  printSudoku sudoku
  if newSudoku /= sudoku then showFullIterate newSudoku
    else return newSudoku

    where newSudoku = iterateLikely . iterateRemove $ sudoku

-- -----------------------------------------------------------------------------
main :: IO ()
main = void . showFullIterate $ ex01

-- -----------------------------------------------------------------------------
