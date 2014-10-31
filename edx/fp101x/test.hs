double x = x + x

quadruple x = double ( double x )

factorial n = product [1..n]

average ns = sum ns `div` length ns


rqsort [] = []
rqsort (x:xs) = reverse (reverse (rqsort smaller) ++ [x] ++ reverse(rqsort larger))
    where
      smaller = [a | a <- xs, a < x]
      larger = [b | b <- xs, b > x]

rqsort' [] = []
rqsort' xs = x: rqsort' larger ++ rqsort' smaller
    where
      x = maximum xs
      smaller = [a | a <- xs, a < x]
      larger = [b | b <- xs, b > x]
