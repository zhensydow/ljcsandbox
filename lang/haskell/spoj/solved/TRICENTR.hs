import Text.Printf( printf )
import Control.Monad( liftM, replicateM_ )

areaTriangle :: Double -> Double -> Double
areaTriangle side centroidDist = (side * centroidDist * 3) / 2

sideTriangle :: Double -> Double -> Double
sideTriangle area centroidDist = (2/3) * area / centroidDist

circumcircleRadius a b c area = a*b*c/(4*area)

orthoCircumDist rad a b c 
  | val <= 0 = 0
  | otherwise = sqrt val
    where val = 9*rad^2 - (a^2 + b^2 + c^2)
          
testCase = do
  [as,cas,cbs,ccs] <- liftM words $ getLine
  let 
    a = (read as)
    area = areaTriangle a (read cas)
    b = sideTriangle area (read cbs)
    c = sideTriangle area (read ccs)
    r = circumcircleRadius a b c area
    ho = orthoCircumDist r a b c
    hg = (2/3)*ho
  printf "%0.3f %0.3f\n" area hg
  
main = do
  num <- getLine
  replicateM_ (read num) testCase
