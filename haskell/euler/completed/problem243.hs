{- A positive fraction whose numerator is less than its denominator is called a
proper fraction.  For any denominator, d, there will be d1 proper fractions; for
example, with d = 12: 1/12 , 2/12 , 3/12 , 4/12 , 5/12 , 6/12 , 7/12 , 8/12 ,
9/12 , 10/12 , 11/12 .

We shall call a fraction that cannot be cancelled down a resilient fraction.
Furthermore we shall define the resilience of a denominator, R(d), to be the
ratio of its proper fractions that are resilient; for example, R(12) = 4/11 .
In fact, d = 12 is the smallest denominator having a resilience R(d) < 4/10 .

Find the smallest denominator d, having a resilience R(d) < 15499/94744 .
-}
import Euler( totient, sigma0, primeDecomp, primorial ) 
import Control.Arrow( (&&&) )
import Data.List( foldl' )

resilience d = (fromIntegral $ totient d) / (fromIntegral (d - 1))

-- | Highly Composite Numbers
-- I think the solution is in the form of R(hcn(n)) with n <- [1..]
--hcn = 1 : hcn' 2 1 2
--hcn = 20160 : hcn' 25200 1 5040
--hcn = 554400 : hcn' 665280 1 110880
--hcn = 1396755360 : hcn' 1396755360 1 21621600
--hcn = 1396755360 : hcn' 1396755360 1 10810800
--hcn = 698377680 : hcn' 698377680 1 1663200
--hcn = 1396755360 : hcn' 1396755360 1 665280
--hcn = 1396755360 : hcn' 1396755360 1 60480
hcn = 16761064320 : hcn' 16761064320 1 332640

hcn' n d i
  | (sigma0 n) > d = n : hcn' (n+i) (sigma0 n) i
  | otherwise = hcn' (n+i) d i

-- it should be a multiple of primes, so a try with primorial list
nhcn = nhcn' 0 (map primorial [0..]) 1

nhcn' d (x:y:xs) m
  | x*m == y = nhcn' d (y:xs) 1
  | otherwise = x*m : nhcn' d (x:y:xs) (m+1)

nextI n = foldl' (*) 1 .  map fst $ primeDecomp n

--limite = 0.17
limite = 15499/94744
solution = head $ dropWhile (((<) limite) . snd) . map (id &&& resilience) $ drop 1 nhcn

invList = [32125373280, (32125373280 - 2)..16761064320]
solutionInv = head $ dropWhile (((<) limite) . snd) . map (id &&& resilience) $ drop 1 invList
  
main = print $ fst solution
--main = print solutionInv

-- Obtenido con pruebas
-- (27720,0.20779970417403226)
-- (720720,0.19180845794269333)
-- (36756720,0.1805253619073019)
-- (698377680,0.17102402266209885)
-- (1396755360,0.17102402253965507)

-- (32125373280,0.16358819536068558)

-- (64250746560,0.16358819535813948)

-- (160626866400,0.16358819535661182) -- Este parece un limite superior
