\begin{code}
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Array( Array(..), Ix(..), (!), listArray, indices, bounds, assocs, array, ixmap, elems )
import Data.Char( digitToInt )
import Data.Word( Word8(..) )
import Data.List( tails, group, sort, sortBy, foldl' )
import Data.Ratio( Ratio(..), (%) )
import Data.Function( on )
import Data.Maybe( catMaybes, listToMaybe )
import Control.Applicative( (<$>) )
import Parse( Pixel, RGB, Pixmap, parse, parsePPM )
import System.IO( openFile, IOMode(..) )
\end{code}

\begin{code}
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
    where products = mapEveryOther (*3) (reverse ds)
\end{code}

\begin{code}
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f,id])
\end{code}

\begin{code}
leftOddList = ["0001101", "0011001", "0010011", "0111101", "0100011",
               "0110001", "0101111", "0111011", "0110111", "0001011"]
\end{code}

\begin{code}
rightList = map complement <$> leftOddList
    where complement '0' = '1'
          complement '1' = '0'
\end{code}

\begin{code}
leftEvenList = map reverse rightList
\end{code}

\begin{code}
parityList = ["111111", "110100", "110010", "110001", "101100",
              "100110", "100011", "101010", "101001", "100101"]
\end{code}

\begin{code}
listToArray :: [a] -> Array Int a
listToArray xs = listArray (0,l-1) xs
    where l = length xs
\end{code}

\begin{code}
leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String
leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList
\end{code}

\begin{code}
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = go s (indices a)
    where go s (j:js) = let s' = f s (a ! j)
                        in s' `seq` go s' js
          go s _ = s
\end{code}

\begin{code}
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a'
    where a'=array (head ti,last ti) (tail (assocs a))
          ti=tail (indices a)
\end{code}

\begin{code}
encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt
\end{code}

\begin{code}
encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) =
    outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
  where (left, right) = splitAt 5 rest
        lefties = zipWith leftEncode (parityCodes ! first) left
        righties = map rightEncode (right ++ [checkDigit s])
\end{code}

\begin{code}
leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)
\end{code}

\begin{code}
rightEncode :: Int -> String
rightEncode = (rightCodes !)
\end{code}

\begin{code}
outerGuard = "101"
centerGuard = "01010"
\end{code}

\begin{code}
luminance :: RGB -> Pixel
luminance (r,g,b) = round (r' * 0.30 + g' * 0.59 + b' * 0.11)
    where r' = fromIntegral r
          g' = fromIntegral g
          b' = fromIntegral b
\end{code}

\begin{code}
data Bit = Zero | One
           deriving( Show, Eq )
\end{code}

\begin{code}
threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n a = binary <$> a
    where binary i
              | i < pivot = Zero
              | otherwise = One
          pivot = round $ least + (greatest - least) * n
          least    = fromIntegral $ choose (<) a
          greatest = fromIntegral $ choose (>) a
          choose f = foldA1 $ \x y -> if f x y then x else y
\end{code}

\begin{code}
type Run = Int
type RunLength a = [(Run,a)]
\end{code}

\begin{code}
runLength :: Eq a => [a] -> RunLength a
runLength = map rle . group
    where rle xs = (length xs, head xs)
\end{code}

\begin{code}
runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . runLength
\end{code}

\begin{code}
type Score = Ratio Int -- Double
\end{code}

\begin{code}
scaleToOne :: [Run] -> [Score]
scaleToOne xs = map divide xs
    where divide d = fromIntegral d / divisor
          divisor = fromIntegral $ sum xs
\end{code}

\begin{code}
type ScoreTable = [[Score]]
\end{code}

\begin{code}
asSRL :: [String] -> ScoreTable
asSRL = map $ scaleToOne . runLengths
\end{code}

\begin{code}
leftOddSRL = asSRL leftOddList
leftEvenSRL = asSRL leftEvenList
rightSRL = asSRL rightList
paritySRL = asSRL parityList
\end{code}

\begin{code}
distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b
\end{code}

\begin{code}
type Digit = Word8
\end{code}

\begin{code}
bestScores :: ScoreTable -> [Run] -> [(Score, Digit)]
bestScores srl ps = take 3 . sort $ scores
    where scores = zip [distance d (scaleToOne ps) | d <- srl] digits
          digits = [0..9]
\end{code}

\begin{code}
data Parity a = Even a | Odd a | None a
                deriving (Show)
\end{code}

\begin{code}
fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd a) = a
fromParity (None a) = a
\end{code}

\begin{code}
parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Even a) = Even (f a)
parityMap f (Odd a) = Odd (f a)
parityMap f (None a) = None (f a)
\end{code}

\begin{code}
instance Functor Parity where
    fmap = parityMap
\end{code}

\begin{code}
compareWithoutParity :: Ord a => Parity a -> Parity a -> Ordering
compareWithoutParity = compare `on` fromParity
\end{code}

\begin{code}
bestLeft :: [Run] -> [Parity (Score, Digit)]
bestLeft ps = sortBy compareWithoutParity
              (map Odd (bestScores leftOddSRL ps) ++
               map Even (bestScores leftEvenSRL ps))

bestRight :: [Run] -> [Parity (Score, Digit)]
bestRight = map None . bestScores rightSRL
\end{code}

\begin{code}
chunkWith :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f xs = let (h, t) = f xs
                 in h : chunkWith f t
\end{code}

\begin{code}
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunkWith (splitAt n)
\end{code}

\begin{code}
candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_, One):_) = []
candidateDigits rle | length rle < 20 = []
candidateDigits rle
    | any null match = []
    | otherwise      = map (map (fmap snd)) match
  where match = map bestLeft left ++ map bestRight right
        left = chunksOf 4 . take 24 . drop 3 $ runLengths
        right = chunksOf 4 . take 24 . drop 32 $ runLengths
        runLengths = map fst rle
\end{code}

\begin{code}
type Map a = M.Map Digit [a]
type DigitMap = Map Digit
type ParityMap = Map (Parity Digit)
\end{code}

\begin{code}
updateMap :: Parity Digit       -- ^ new digit
          -> Digit              -- ^ existing key
          -> [Parity Digit]     -- ^ existing digit sequence
          -> ParityMap          -- ^ map to update
          -> ParityMap
updateMap digit key seq = insertMap key (fromParity digit) (digit:seq)
\end{code}

\begin{code}
insertMap :: Digit -> Digit -> [a] -> Map a -> Map a
insertMap key digit val m = val `seq` M.insert key' val m
    where key' = (key + digit) `mod` 10
\end{code}

\begin{code}
useDigit :: ParityMap -> ParityMap -> Parity Digit -> ParityMap
useDigit old new digit =
    new `M.union` M.foldWithKey (updateMap digit) M.empty old
\end{code}

\begin{code}
incorporateDigits :: ParityMap -> [Parity Digit] -> ParityMap
incorporateDigits old digits = foldl' (useDigit old) M.empty digits
\end{code}

\begin{code}
finalDigits :: [[Parity Digit]] -> ParityMap
finalDigits = foldl' incorporateDigits (M.singleton 0 [])
            . mapEveryOther (map (fmap (*3)))
\end{code}

\begin{code}
firstDigit :: [Parity a] -> Digit
firstDigit = snd
           . head
           . bestScores paritySRL
           . runLengths
           . map parityBit
           . take 6
  where parityBit (Even _) = Zero
        parityBit (Odd _) = One
\end{code}

\begin{code}
addFirstDigit :: ParityMap -> DigitMap
addFirstDigit = M.foldWithKey updateFirst M.empty
\end{code}

\begin{code}
updateFirst :: Digit -> [Parity Digit] -> DigitMap -> DigitMap
updateFirst key seq = insertMap key digit (digit:renormalize qes)
  where renormalize = mapEveryOther (`div` 3) . map fromParity
        digit = firstDigit qes
        qes = reverse seq
\end{code}

\begin{code}
buildMap :: [[Parity Digit]] -> DigitMap
buildMap = M.mapKeys (10 -)
         . addFirstDigit
         . finalDigits
\end{code}

\begin{code}
solve :: [[Parity Digit]] -> [[Digit]]
solve [] = []
solve xs = catMaybes $ map (addCheckDigit m) checkDigits
    where checkDigits = map fromParity (last xs)
          m = buildMap (init xs)
          addCheckDigit m k = (++[k]) <$> M.lookup k m
\end{code}

\begin{code}
withRow :: Int -> Pixmap -> (RunLength Bit -> a) -> a
withRow n greymap f = f . runLength . elems $ posterized
    where posterized = threshold 0.4 . fmap luminance . row n $ greymap
\end{code}

\begin{code}
row :: (Ix a, Ix b) => b -> Array (a,b) c -> Array a c
row j a = ixmap (l,u) project a
    where project i = (i,j)
          ((l,_), (u,_)) = bounds a
\end{code}

\begin{code}
findMatch :: [(Run, Bit)] -> Maybe [[Digit]]
findMatch = listToMaybe
          . filter (not . null)
          . map (solve . candidateDigits)
          . tails
\end{code}

\begin{code}
findEAN13 :: Pixmap -> Maybe [Digit]
findEAN13 pixmap = withRow center pixmap (fmap head . findMatch)
  where (_, (maxX, _)) = bounds pixmap
        center = (maxX + 1) `div` 2
\end{code}

\begin{code}
example = do
  e <- parse parsePPM <$> L.readFile "barcode01.ppm"
  case e of
    Left err ->     print $ "error: " ++ err
    Right pixmap -> print $ findEAN13 pixmap
\end{code}
