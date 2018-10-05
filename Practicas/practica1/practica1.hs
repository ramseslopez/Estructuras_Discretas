media3:: Float -> Float -> Float -> Float
media3 x y z = (x + y + z)/3

absoluto:: Int -> Int
absoluto n|n >= 0 = n
absoluto n|n < 0 = -n

esTriangulo:: (Num a, Ord a) => a -> a -> a -> Bool
esTriangulo x y z = x < y + z
esTriangulo x y z = y<x+z
esTriangulo x y z = z<x+y

maxRect :: (Float,Float) -> (Float,Float) -> (Float,Float)
maxRect (a1,b1) (a2,b2) = if (a1,b1)>(a2,b2) then (a1,b1) else (a2,b2)

dPuntos :: (Float,Float) -> (Float,Float) -> Float
dPuntos (x1,y1) (x2,y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2)

sumaC :: (Double,Double) -> (Double,Double) -> (Double,Double)
sumaC (a,b) (c,d) = (a + c, b + d)

restaC :: (Double,Double) -> (Double,Double) -> (Double,Double)
restaC (a,b) (c,d) = (a-c, b-d)

multC :: (Double,Double) -> (Double,Double) -> (Double,Double)
multC (a,b) (c,d) = (a*c-b*d, a*d+b*c)

divC :: (Double,Double) -> (Double,Double) -> (Double,Double)
divC (a,b) (c,d) = ((a*c + b*d)/(c*c + d*d), (b*c - a*d)/(c*c + d*d))
 
conjugado :: (Double,Double) -> (Double,Double)
conjugado (a,b) = (a,-b)

realC :: (Double,Double) -> Double
realC (a,b) = a

imaginarioC :: (Double,Double) -> Double
imaginarioC (a,b) = b


           
