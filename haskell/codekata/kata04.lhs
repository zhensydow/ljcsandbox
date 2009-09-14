\begin{code}
import System.IO
import Control.Arrow( (&&&) )
import Data.Ord( comparing )
import Data.List( sortBy )
\end{code}

\begin{code}
chunk :: Int -> Int -> [String] -> [String]
chunk x y = take (y - x) . drop (x-1)
\end{code}

\begin{code}
columns :: [Int] -> [a] -> [a]
columns is xs = map (\a->xs!!a) is
\end{code}

\begin{code}
toint :: String -> Integer
toint a = read a
\end{code}

\begin{code}
totuple3 :: [a] -> (a,a,a)
totuple3 [a,b,c] = (a,b,c)
totuple3 _ = error "no 3 elements"
\end{code}

\begin{code}
cleanWeather [a,b,c]= (a
                      , min (cleanN b) (cleanN c))
    where
      cleanN a = toint $ filter (`elem` "0123456789") a
\end{code}

\begin{code}
cleanFootball [a,b,c]= (a
                       , abs ((cleanN b) - (cleanN c)))
    where
      cleanN a = toint $ filter (`elem` "0123456789") a
\end{code}

\begin{code}
extractMin :: [Int] -> ([String] -> (String,Integer)) -> [String] -> String
extractMin cols f xs = fst . head 
                       $ sortBy (comparing snd)
                       $ map f
                       $ map (columns cols)
                       $ map words xs
\end{code}

\begin{code}
weatherData = do
  weatherFile <- readFile "weather.dat"
  let dat = extractMin [0,1,2] cleanWeather
            $ chunk 9 39 $ lines weatherFile
  return dat
\end{code}

\begin{code}
footballData = do
  footballFile <- readFile "football.dat"
  let dat = extractMin [1,6,8] cleanFootball
            $ (++) 
               (chunk 6 23 $ lines footballFile)
               (chunk 24 27 $ lines footballFile)
  return dat
\end{code}

\begin{code}
main = do
  wd <- weatherData
  print wd

  fd <- footballData
  print fd
\end{code}