module Util where

import Data.Ratio( (%) )
import Data.List( sort )

-- returns a Ratio from a mixed fraction
funFraction a n d = ((a * d) + n) % d

-- calculate factorial
factorial 0 = 1
factorial n = product [1..n]

-- calculate binomial coefficent
binomial n k = (factorial n) `div` ((factorial k) * (factorial (n-k)))

-- transform degrees to radians
deg2rad d = (d * pi) / 180

rotate (x, y) d = (x', y')
    where
      r = deg2rad d
      x' = x * (cos r) - y * (sin r)
      y' = x * (sin r) + y * (cos r)


mean xs = sum xs / (fromIntegral . length) xs

median xs = if even n 
            then ((ys !! mid) + (ys !! (mid - 1))) / 2
            else ys !! mid
    where
      n = length xs
      ys = sort xs
      mid = (length ys) `div` 2
