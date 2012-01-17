{- It is easily proved that no equilateral triangle exists with integral length
sides and integral area. However, the almost equilateral triangle 5-5-6 has an
area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two
sides are equal and the third differs by no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with integral
side lengths and area and whose perimeters do not exceed one billion
(1,000,000,000).-}
import Euler( isSquare )

area (a,b,c) = 0.25 * sqrt ((fa+fb-fc)*(fa-fb+fc)*(fb+fc-fa)*(fa+fb+fc))
  where
    fa = fromIntegral a
    fb = fromIntegral b
    fc = fromIntegral c

triangles1 = [ (a,a,b) 
            |  a <- [2 .. 1000000000 `quot` 2], b <- [a-1,a+1]]

triangles2 = [ (a,b) 
            |  a <- [33895685 .. 333333334], b <- [a-1,a+1]]

triangles2a = [ (a,a-1) | a <- [33895685 .. 333333334]]
triangles2b = [ (a,a+1) | a <- [126500417 .. 333333334]]

area2 (a,c) = 0.25 * sqrt ((2*fa-fc)*fc*fc*(2*fa+fc))
  where
    fa = fromIntegral a
    fc = fromIntegral c

check2 (a,c) = isSquare $ (2*a-c)*(2*a+c)
check2a (a,_) = isSquare $ (a+1)*(3*a-1)
check2b (a,_) = isSquare $ (a-1)*(3*a+1)

perimeter2 (a,c) = a + a + c

listT = [(5,6),(17,16),(65,66),(241,240),(901,902),(3361,3360),(12545,12546)
        ,(46817,46816),(174725,174726),(652081,652080),(2433601,2433602)
        ,(9082321,9082320),(33895685,33895686),(126500417,126500416)]

solution = sum . fmap perimeter2 $ listT
--main = print $ sum $ map area triangles1

--main = print $ filter check2 triangles2
main = print solution