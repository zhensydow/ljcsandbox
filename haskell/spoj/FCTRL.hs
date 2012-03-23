import Data.List( nub, foldl' )

fact1 n = product [1..n]

z1 = length . takeWhile (=='0') . reverse . show . fact1

check3 n = mod_2 || mod_5
  where
    mod_2 = (n `mod` 2) == 0
    mod_5 = (n `mod` 5) == 0

fact3 n = product . filter check3 $ [2..n]

z3 = length . takeWhile (=='0') . reverse . show . fact3

incDic [] k = [(k,1)]
incDic ((k1,v):xs) k
  | k1 == k = (k1,v+1):xs
  | otherwise = (k,1):(k1,v):xs

primeDecomp n = primeDecomp' n [2,5] []
primeDecomp' 1 _ d = d
primeDecomp' _ [] d = d
primeDecomp' n (p:ps) d
    | mm == 0 = primeDecomp' dd (p:ps) (incDic d p)
    | otherwise = primeDecomp' n ps d
      where (dd,mm) = n `divMod` p

countMin n = count2_5 (primeDecomp n) (0,0)

count2_5 [] val = val
count2_5 ((i,n):xs) (v2,v5)
  | i == 2 = count2_5 xs (v2+n,v5)
  | otherwise = count2_5 xs (v2,v5+n)

sum11 (a2,b2) a = (a1+a2,b1+b2)
  where (a1,b1) = countMin a

fact5 :: Int -> (Int,Int)
fact5 n = foldl' sum11 (0,0) [2..n]
z5 = fact5

main = print $ z5 2345611
