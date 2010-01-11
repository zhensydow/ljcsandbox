\begin{code}
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Word( Word8 )
import Data.Int( Int64 )
import Data.Char( chr, isSpace, isDigit )
import Control.Applicative( (<$>) )
import System.IO( openFile, IOMode(..) )
\end{code}

\begin{code}
data Greymap = Greymap {
  greyWidth :: Int
, greyHeight :: Int
, greyMax :: Int
, greyData :: L.ByteString
} deriving( Eq )
\end{code}

\begin{code}
instance Show Greymap where
    show (Greymap w h m d) = "Greymap " 
                             ++ show w ++ "x" ++ show h 
                             ++ " " ++ show m
                             ++ " " ++ show (L.take 5 d) 
\end{code}

\begin{code}
data ParseState = ParseState {
  string :: L.ByteString
, offset :: Int64
} deriving( Show )
\end{code}

\begin{code}
newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
}
\end{code}

\begin{code}
instance Functor Parse where
    fmap f parser = parser ==> \result ->
                    identity (f result)
\end{code}

\begin{code}
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left err -> Left err
        Right (result, _) -> Right result
\end{code}

\begin{code}
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))
\end{code}

\begin{code}
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))
\end{code}

\begin{code}
putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))
\end{code}

\begin{code}
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
    where chainedParser initState =
              case runParse firstParser initState of
                Left err -> Left err
                Right (result, newState) ->
                    runParse (secondParser result) newState
\end{code}

\begin{code}
(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f
\end{code}

\begin{code}
bail :: String -> Parse a
bail err = Parse (\s -> Left $
                  "byte offset " ++ show( offset s ) ++ ": " ++ err)
\end{code}

\begin{code}
assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err
\end{code}

\begin{code}
parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
          bail "no more input"
      Just (byte, remainder) ->
          putState newState ==> \_ ->
          identity byte
        where newState = initState { string = remainder
                                   , offset = newOffset }
              newOffset = offset initState + 1
\end{code}

\begin{code}
parseBytes :: Int -> Parse L.ByteString
parseBytes n = 
    getState ==> \st ->
    let n' = fromIntegral n
        (h,t) = L.splitAt n' (string st)
        st' = st { offset = offset st + L.length h, string = t}
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h
\end{code}

\begin{code}
parseNat :: (Num a, Read a, Ord a) => Parse a
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more digits"
           else let n = read digits
                in if n < 0 
                   then bail "integer overflow"
                   else identity n
\end{code}

\begin{code}
parseNats :: Int -> Parse [Word8]
parseNats 0 = identity []
parseNats n = parseNat ==> \nat -> skipSpaces ==>&
              fmap (nat:) (parseNats (n-1))
\end{code}

\begin{code}
peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState
\end{code}

\begin{code}
skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()
\end{code}

\begin{code}
w2c :: Word8 -> Char
w2c = chr . fromIntegral
\end{code}

\begin{code}
notWhite = (`notElem` " \r\n\t")
\end{code}

\begin{code}
parseRawPGM =
--    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
--    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxgrey -> 
    parseByte ==>&
    parseBytes (width*height) ==> \bitmap ->
    identity $ Greymap width height maxgrey bitmap
\end{code}

\begin{code}
parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p.f)
\end{code}

\begin{code}
parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                   (b:) <$> parseWhile p
               else identity []
\end{code}

\begin{code}
parsePlainPGM =
--    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
--    assert (header == "P2") "invalid plain header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxgrey -> 
    parseByte ==>&
    parseNats (width*height) ==> \bitmap ->
    identity $ Greymap width height maxgrey (L.pack bitmap)
\end{code}

\begin{code}
parsePGM =
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    case header of
      "P2" -> parsePlainPGM
      "P5" -> parseRawPGM
      otherwise -> bail "invalid plain header"
\end{code}

\begin{code}
main = do
  h <- openFile "example1.pgm" ReadMode
  bs <- L.hGetContents h
  print $ parse parsePGM bs

  h <- openFile "example2.pgm" ReadMode
  bs <- L.hGetContents h
  print $ parse parsePGM bs
\end{code}
