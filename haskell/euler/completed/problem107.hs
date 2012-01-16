{- The following undirected network consists of seven vertices and twelve edges
with a total weight of 243.


The same network can be represented by the matrix below.

        1       2       3       4       5       6       7
    	A	B	C	D	E	F	G
A	-	16	12	21	-	-	-
B	16	-	-	17	20	-	-
C	12	-	-	28	-	31	-
D	21	17	28	-	18	19	23
E	-	20	-	18	-	-	11
F	-	-	31	19	-	-	27
G	-	-	-	23	11	27	-

However, it is possible to optimise the network by removing some edges and still
ensure that all points on the network remain connected. The network which
achieves the maximum saving is shown below. It has a weight of 93, representing
a saving of 243 93 = 150 from the original network.

Using network.txt (right click and 'Save Link/Target As...'), a 6K text file
containing a network with forty vertices, and given in matrix form, find the
maximum saving which can be achieved by removing redundant edges whilst ensuring
that the network remains connected.

-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Equivalence.Monad
import qualified Data.Graph as G
import Data.List( sortBy )
import qualified Data.Map as M( lookup, fromList )
import Control.Monad(filterM)
import Control.Arrow( second )
import Data.Ord(comparing)
import Data.Maybe( isJust, fromJust, catMaybes )
import qualified Data.Text as T

type Weights = G.Edge -> Integer
fromL :: [(G.Edge,Integer)] -> Weights
fromL xs = fromJust . flip M.lookup (M.fromList xs)
run = runEquivM (const ()) (const $ const ())

kruskal weight graph = run $
 filterM go (sortBy (comparing weight) theEdges)
     where
      theEdges = G.edges graph
      go (u,v) = do 
        eq <- equivalent u v
        if eq then return False else
         equate u v >> return True

example01 :: [((Int,Int),Integer)]
example01 = [ ((1,2),16),((1,3),12),((1,4),21)
            , ((2,4),17),((2,5),20)
            , ((3,4),28),((3,6),31)
            , ((4,5),18),((4,6),19),((4,7),23)
            , ((5,7),11)
            , ((6,7),27)]

spanningTree :: [((Int,Int),Integer)] -> [(Int,Int)]
spanningTree xs = kruskal weigths graph
  where
    weigths = fromL xs
    graph = G.buildG (minimum nodes, maximum nodes) $ fmap fst xs
    nodes = concat . fmap ((\(a,b) -> [a,b]) . fst) $ xs
    
cost :: [((Int,Int),Integer)] -> [(Int,Int)] -> Integer
cost table = sum . catMaybes . map (flip lookup table)

solve :: [((Int,Int), Integer)] -> Integer
solve xs = total - reduced
  where
    total = sum $ fmap snd xs
    reduced = cost xs $ spanningTree xs

readData :: String -> IO [((Int, Int), Integer)]
readData filename = do
  dat <- fmap T.pack $ readFile filename
  let rows = zipWith addI [1..] . fmap readLine $ T.lines dat
      vals = filter (\((a,b),_) -> a <= b) $ concat rows
  return vals
  
readLine = map (second fromJust) . filter (isJust . snd) . zip [1..] . map readWeight . T.splitOn ","

addI idx = fmap (\(j,w) -> ((idx,j),w))

readWeight :: T.Text -> Maybe Integer
readWeight text
  | text == "-" = Nothing
  | otherwise = Just . read . T.unpack $ text

solution = fmap solve $ readData "problem107.txt"

main :: IO ()
main = solution >>= print
