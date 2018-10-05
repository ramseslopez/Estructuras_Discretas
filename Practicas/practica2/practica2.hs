gtaveragethree :: Int -> Int -> Int -> Int
gtaveragethree x y z = if x> (div (x+y+z) 3) then x else if y>(div (x+y+z) 3) then y else if z> (div(x+y+z) 3) then z else error "No es posible"

palindromo :: String -> Bool
palindromo [] = False
palindromo [x] = True
palindromo xs = reverse xs == (xs)

minMax :: Ord a => [a] -> (a,a)
minMax [] = error "error"
minMax [x] = (x,x)
minMax xs = (minimum (xs), maximum (xs))

atN :: [a] -> Int -> a
atN [] x = error "error"
atN xs x = head (drop (x-1) (take x (xs)))
atN xs x = if x <= (length xs) then head (drop (x-1) (take x (xs))) else error "Qué mal"

selectMin :: [Int] -> [Int]
selectMin [] = error "error"
selectMin xs = if (length xs) >= (minimum xs) then take (minimum xs) xs else error "error"

deleteMin :: [Int] -> [Int]
deleteMin [] = error "error"
deleteMin xs = if (length xs) >= (minimum xs) then drop (minimum xs) xs else error "error"

delN :: Int -> [a] -> [a]
delN y [] = error "error"
delN y xs = take (y-1) xs ++  drop y xs

intervalos :: Int -> Int -> [a] -> [a]
intervalos y z [] = error "error"
intervalos y z xs = drop (y-1) (take z (xs))

dIntervalos :: Int -> Int -> [a] -> [a]
dIntervalos y z [] = error "error"
dIntervalos y z xs = take (y-1) xs ++ drop z xs

avgLen :: [Int] -> Bool
avgLen [] = error "error"
avgLen xs = if length(xs) > (div (sum xs) (length xs)) then True else False
