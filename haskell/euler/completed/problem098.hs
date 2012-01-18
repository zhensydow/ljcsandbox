{- By replacing each of the letters in the word CARE with 1, 2, 9, and 6
respectively, we form a square number: 1296 = 362. What is remarkable is that,
by using the same digital substitutions, the anagram, RACE, also forms a square
number: 9216 = 962. We shall call CARE (and RACE) a square anagram word pair and
specify further that leading zeroes are not permitted, neither may a different
letter have the same digital value as another letter.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
containing nearly two-thousand common English words, find all the square anagram
word pairs (a palindromic word is NOT considered to be an anagram of itself).

What is the largest square number formed by any member of such a pair?

NOTE: All anagrams formed must be contained in the given text file.
-}
import Control.Arrow( (&&&) )
import Data.List( sort, permutations )
import Data.List.Split( splitOn )
import qualified Data.Map as M( Map, fromListWithKey, filter, size, assocs )
import Data.Maybe( fromJust )
import Euler( toint, combinations, isSquare )

readData :: IO [String]
readData = do
  val <- readFile "problem098.txt"
  return . filter ((>3).length) . splitOn "," . filter (/='"') $ val

anagram :: Ord a => [a] -> [a] -> Bool
anagram xs ys = (sort xs) == (sort ys)

mkMap :: [String] -> M.Map String [String]
mkMap = M.filter ((>1).length) . M.fromListWithKey (\_ a b -> a ++ b) . fmap (sort &&& (:[]))

substits :: String -> [[Int]]
substits xs = concatMap permutations $ combinations (length xs) [0..9]

apply :: Eq a => [(a,Int)] -> [a] -> [Int]
apply m xs = fmap (fromJust . (flip lookup) m) xs

fff :: (String, [String]) -> [[Int]]
fff (idx, xs) = filter checkMinimum . map (map toint) . filter checkLeadingZero 
                $ fmap (\m -> fmap (\a -> apply m a) xs) subs
  where
    subs = map (zip idx) $ substits idx

hasLeadingZero :: [Int] -> Bool
hasLeadingZero = (==0) . head

checkLeadingZero :: [[Int]] -> Bool
checkLeadingZero = not . any hasLeadingZero

checkMinimum :: [Int] -> Bool
checkMinimum = any (>9000)

checkSquares :: [Int] -> Bool
checkSquares = all isSquare

solution = do
  mm <- fmap mkMap readData
  let pairs =  filter checkSquares . concat $ fmap fff $ M.assocs mm
  return $ maximum . concat $ pairs
  
main = solution >>= print
