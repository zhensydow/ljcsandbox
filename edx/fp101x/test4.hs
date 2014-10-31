import Data.Char( isLower, isUpper, chr, ord )

factors n = [ x | x <- [1..n], n `mod` x == 0]
prime n = factors n == [1,n]

primes n = [x | x <- [2..n], prime x]

pairs xs = zip xs (tail xs)

sorted xs = and [x <= y | (x,y) <- pairs xs]

positions x xs = [i |(x',i) <- zip xs [0..n], x == x']
    where n = length xs - 1

lowers xs = length [x | x <- xs, isLower x]

isPerfect num = sum (init(factors num)) == num

scalarproduct xs ys = sum [x*y | (x,y) <- xs `zip` ys]

let2intL c = ord c - ord 'a'
let2intU c = ord c - ord 'A'

int2letL n = chr (ord 'a' + n)
int2letU n = chr (ord 'A' + n)

shift n c 
    | isLower c = int2letL ((let2intL c + n) `mod` 26)
    | isUpper c = int2letU ((let2intU c + n) `mod` 26)
    | otherwise = c

encode n xs = [shift n x | x <- xs]

riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]
