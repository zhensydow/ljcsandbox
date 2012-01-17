import qualified Text.Numeral.Roman as R( toRoman )
import Data.Maybe( fromJust )

toRoman :: Integral a => a -> String
toRoman = R.toRoman

table = [ ('I', 1), ('V',5), ('X',10)
        , ('L',50), ('C',100)
        ,( 'D', 500), ('M',1000)]
        
fromRoman :: String -> Maybe Integer
fromRoman xs = 
  case xs of
    [] -> return 0
    ('I':'V':ys) -> do
      val <- fromRoman ys
      return $ 4 + val
    ('I':'X':ys) -> do
      val <- fromRoman ys
      return $ 9 + val
    ('X':'L':ys) -> do
      val <- fromRoman ys
      return $ 40 + val
    ('X':'C':ys) -> do
      val <- fromRoman ys
      return $ 90 + val
    ('C':'D':ys) -> do
      val <- fromRoman ys
      return $ 400 + val
    ('C':'M':ys) -> do
      val <- fromRoman ys
      return $ 900 + val
    (x:ys) -> do
      valYs <- fromRoman ys
      valX <- lookup x table
      return $ valX + valYs

tests = ["IIIIIIIIIIIIIIII", "VIIIIIIIIIII", "VVIIIIII", "XIIIIII", "VVVI", "XVI"]

readData = do
  vals <- fmap lines $ readFile "problem089.txt"
  return vals

checkSaved :: String -> Int
checkSaved x = (length x) - (length . toRoman . fromJust . fromRoman $ x)

solution = readData >>= return . sum . map checkSaved
  
main = solution >>= print