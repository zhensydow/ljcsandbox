\begin{code}
module Prettify where
\end{code}

\begin{code}
import Numeric( showHex )
import Data.Bits( shiftR, (.&.) )
import Data.Char( ord )
\end{code}

\begin{code}
data Doc = Empty
         | Char Char
         | Text String 
         | Line 
         | Concat Doc Doc
         | Union Doc Doc
           deriving( Eq, Show )
\end{code}

\begin{code}
empty :: Doc
empty = Empty
\end{code}

\begin{code}
line :: Doc
line = Line
\end{code}

\begin{code}
string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar
\end{code}

\begin{code}
text :: String -> Doc
text "" = Empty
text txt = Text txt
\end{code}

\begin{code}
double :: Double -> Doc
double num = text (show num)
\end{code}

\begin{code}
enclose :: Char -> Char -> Doc -> Doc
enclose l r x = char l <> x <> char r
\end{code}

\begin{code}
(<>) :: Doc -> Doc -> Doc
Empty <> b = b
a <> Empty = a
a <> b = a `Concat` b
\end{code}

\begin{code}
char :: Char -> Doc
char c = Char c
\end{code}

\begin{code}
hcat :: [Doc] -> Doc
hcat = foldr (<>) empty
\end{code}

\begin{code}
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing
                  | mustEscape c -> hexEscape c
                  | otherwise -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])
\end{code}

\begin{code}
smallHex :: Int -> Doc
smallHex x = text "\\u"
             <> text (replicate (4 - length h) '0')
             <> text h
    where h = showHex x ""
\end{code}

\begin{code}
hexEscape :: Char -> Doc
hexEscape c 
    | d < 0x10000 = smallHex d
    | otherwise = astral (d - 0x10000)
    where d = ord c
\end{code}

\begin{code}
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff
\end{code}

\begin{code}
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close 
                         . fsep . punctuate (char ',')  . map item
\end{code}

\begin{code}
fsep :: [Doc] -> Doc
fsep = foldr (</>) empty
\end{code}

\begin{code}
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
\end{code}

\begin{code}
(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y
\end{code}

\begin{code}
softline :: Doc
softline = group line
\end{code}

\begin{code}
group :: Doc -> Doc
group x = flatten x `Union` x
\end{code}

\begin{code}
flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other
\end{code}

\begin{code}
compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (x:xs) = 
              case x of
                Empty -> transform xs
                Char c -> c : transform xs
                Text s -> s ++ transform xs
                Line -> transform xs
                a `Concat` b -> transform (a:b:xs)
                _ `Union` b -> transform (b:xs)
\end{code}

\begin{code}
pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (x:xs) =
              case x of
                Empty -> best col xs
                Char c -> c : best (col+1) xs
                Text s -> s ++ best (col+(length s)) xs
                Line -> '\n' : best 0 xs
                a `Concat` b -> best col (a:b:xs)
                a `Union` b -> nicest col (best col (a:xs))
                               (best col (b:xs))
          best _ _ = []
          nicest col a b 
              | (width - least) `fits` a = a
              | otherwise = b
              where least = min width col
\end{code}

\begin{code}
fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n':_) = True
w `fits` (c:cs) = (w-1) `fits` cs
\end{code}

\begin{code}
nest :: Int -> Doc -> Doc
nest tab x = snd $ handle 0 x
    where handle col x =
              case x of
                Empty -> (col, Empty)
                Char c -> 
                  case c of
                    '[' -> (col+1, Char c)
                    '{' -> (col+1, Char c)
                    ']' -> (col-1, Char c)
                    '}' -> (col-1, Char c)
                    _ -> (col, Char c)
                Line -> (col, Line `Concat` Text 
                         (replicate (col*tab) ' '))
                Text s -> (col, Text s)
                a `Concat` b -> (c3, d2 `Concat` d3)
                    where (c2,d2) = handle col a
                          (c3,d3) = handle c2 b
                a `Union` b -> (col,
                                   snd (handle col a)
                                   `Union`
                                   snd (handle col b))
\end{code}
