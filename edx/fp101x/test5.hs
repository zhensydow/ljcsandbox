import Prelude hiding( (^), and )

m ^ 0 = 1
m ^ n = m * (^) m (n - 1)

and [] = True
--and (b:bs) = b && and bs
--and (b:bs)
--    | b = and bs
--    | otherwise = False
--and (b:bs)
--    | b == False = False
--    | otherwise = and bs
and (b:bs) = and bs && b

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y 
                      then x : merge xs (y:ys) 
                      else y : merge (x:xs) ys

halve xs = splitAt (length xs `div` 2) xs

msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
    where (ys,zs) = halve xs
