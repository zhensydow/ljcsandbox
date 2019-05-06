\begin{code}
import qualified Data.ByteString as B
import qualified Data.Digest.MD5 as MD5
import Codec.Utils
import qualified Data.Bits as Bit
\end{code}

\begin{code}
bitmapSize :: Int
bitmapSize = 32 * 1024
wordCount = 50000
m_n = div (bitmapSize*8) wordCount
\end{code}

\begin{code}
\end{code}

\begin{code}
md5 :: String -> [Octet]
md5 = MD5.hash . map (fromIntegral.fromEnum)
\end{code}

\begin{code}
convert n = foldr (\a b -> a + b * n) 0 
\end{code}

\begin{code}
hashMD5 :: Int -> String -> Int
hashMD5 n s = convert 256 $ key
    where 
      key = take 3 . drop (3*n) . map (fromInteger . toInteger) $ md5 s
\end{code}

\begin{code}
hashSum :: String -> Int
hashSum = convert 32 . map (fromIntegral.fromEnum)
\end{code}

\begin{code}
getHashBit h = divMod (mod h (bitmapSize * 8)) 8
\end{code}

\begin{code}
emptyBitmap = B.replicate bitmapSize 0
\end{code}

\begin{code}
checkBit :: B.ByteString -> (Int, Int) -> Bool
checkBit xs (a,b) = Bit.testBit (B.index xs a) b
\end{code}

\begin{code}
setBit xs (a,b) = setByte xs byte a
    where
      byte = Bit.setBit (B.index xs a) b
\end{code}

\begin{code}
setByte xs b n = snd $ B.mapAccumL fset 0 xs
    where
      fset a x
          | a == n = (a+1, b)
          | otherwise = (a+1, x)
\end{code}

\begin{code}
hashFuns = [hashSum, hashMD5 0, hashMD5 1, hashMD5 2]
\end{code}

\begin{code}
hash w = map (\a -> getHashBit $ a w) hashFuns
\end{code}

\begin{code}
setHash :: B.ByteString -> String -> B.ByteString
setHash xs w = foldl (\b a-> setBit b a) xs $ hash w
\end{code}

\begin{code}
checkHash :: B.ByteString -> String -> Bool
checkHash xs w = all (checkBit xs) $ hash w
\end{code}

\begin{code}
dictionary = do
  wordsfile <- readFile "wordlist.txt"

  return $ foldl setHash emptyBitmap $ lines wordsfile
\end{code}

\begin{code}
main = do
  dic <- dictionary
  print $ show dic
\end{code}
