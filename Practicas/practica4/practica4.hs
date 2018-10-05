import Data.Char

primeros :: [(a,b)] -> [a]
primeros ((x,y):xs) = [x | (x,y) <- ((x,y):xs)]

segundos :: [(a,b)] -> [b]
segundos ((x,y):xs) = [y | (x,y) <- ((x,y):xs)]

divN :: Int -> [Int]
divN n = [x | x <- [1..99], mod x n == 0]

minusculas :: String -> String
minusculas (x:xs)  = toLower x : [ toLower x | x <- xs]

mPares :: Num a => [(a,a)] -> [a]
mPares xs = [x*y | (x,y) <- xs]

sumaDeCuadrados :: Int -> Int
sumaDeCuadrados n = sum [x^2 | x <- [1..n]]

desdobla :: [[a]] -> [a]
desdobla xss = [x | xs <- xss, x <- xs]

separa :: [(a,b)] -> ([a],[b])
separa ((x,y):xs) = ([x | (x,y) <- ((x,y):xs)], [y | (x,y) <- ((x,y):xs)])

prodC :: [a] -> [b] -> [(a,b)]
prodC (x:xs) (y:ys) = [(x,y) | x <- (x:xs), y <- (y:ys)]

primos :: Int -> [Int]
primos n = [x | x <- [2..n], esPrimo x]


-- Auxiliares
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], mod n x == 0]

esPrimo :: Int -> Bool
esPrimo n = divisores n == [1,n]

esPar :: Int -> Bool
esPar n = if mod n 2 == 0 then True else False

esImpar :: Int -> Bool
esImpar n = if mod n 3 == 0 then True else False
