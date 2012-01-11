{- We shall define a square lamina to be a square outline with a square "hole"
so that the shape possesses vertical and horizontal symmetry. For example, using
exactly thirty-two square tiles we can form two different square laminae:

With one-hundred tiles, and not necessarily using all of the tiles at one time,
it is possible to form forty-one different square laminae.

Using up to one million tiles how many different square laminae can be formed?
-}

biggest k = (k + 4) `div` 4

greaterWidth n = (n-1) `div` 2

numTiles n w = ((2 * n * w)) + ((n - (2 * w)) * 2 * w)

posibles0 k = [ (n,w) | n <- [3 .. biggest k], w <- [1 .. greaterWidth n]]

generate k n w
  | n > biggest k = []
  | w > greaterWidth n = generate k (n+1) 1
  | k < numTiles n w = generate k (n+1) 1
  | otherwise = (n,w) : generate k n (w+1)

posibles1 k = generate k 3 1

solution = length $ map (uncurry numTiles) $ posibles1 1000000

main = print solution