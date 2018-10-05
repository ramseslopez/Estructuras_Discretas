sumaR :: Int -> Int -> Int
sumaR x 0 = x
sumaR 0 y = y
sumaR x y = 1 + (sumaR x (y-1))

restaR :: Int -> Int -> Int
restaR x 0 = x
restaR 0 y = -y
restaR x y = -1 + (restaR x (y-1))

menorQ :: Int -> Int -> Bool
menorQ x 0 = if x < 0 then True else False
menorQ 0 y = if y > 0 then True else False
menorQ x y = menorQ (x-1) (y-1)

mayorQ :: Int->Int->Bool
mayorQ x 0 = if x > 0 then True else False
mayorQ 0 y = if y > 0 then False else True
mayorQ x y = mayorQ (x-1) (y-1)

igualQ :: Int -> Int -> Bool
igualQ x 0 = if x == 0 then True else False
igualQ 0 y = if y == 0 then True else False
igualQ x y = igualQ (x-1) (y-1)

power :: Int -> Int -> Int
power x 0 = 1
power x y = x * power x (y-1)

divInt ::Int -> Int -> Int
divInt x 0 = error "error"
divInt 0 y = 0
divInt x y = if x-y < 0 then 0 else 1 + divInt (x-y) (y)

power2 :: Int -> Int -> Int
power2 n 0 = 1
power2 n k = if ((div k 2 == 0)) then n * (power2 n ((2*(div k 2)))) else n *(power n (k-1))

dobleFactorial :: Int -> Int
dobleFactorial 0 = 1
dobleFactorial n = if n>0 then n*(dobleFactorial (n-2)) else dobleFactorial 0

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n xs = True
pertenece n xs = if (pertenece n xs) then True else False
