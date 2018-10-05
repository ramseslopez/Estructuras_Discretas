
-- López Soto Ramsés Antonio 31531997-4
-- Proyecto 1

-- (1)Naturales

data Natural = Cero | Suce Natural deriving Show

sumaN1 :: Natural -> Natural -> Natural
sumaN1 n Cero = n
sumaN1 Cero m = m
sumaN1 (Suce n) m = (sumaN1 n (Suce m))

multN1 :: Natural -> Natural -> Natural
multN1 Cero Cero = Cero
multN1 n (Suce Cero) = n
multN1 (Suce Cero) m = m
multN1 n (Suce m) = (sumaN1 n (multN1 n m))

natToInt :: Natural -> Int
natToInt Cero = 0
natToInt (Suce n) = 1 + (natToInt n)

intToNat :: Int -> Natural
intToNat 0 = Cero
intToNat n = Suce(intToNat (n-1))

restaSeg :: Natural -> Natural -> Natural
restaSeg n Cero = n
restaSeg Cero m = error "La operación no es posible"
restaSeg (Suce n) (Suce m) = (restaSeg n m)

-- (2)Arboles

data Arbol = Hoja Int | Nodo Arbol Int Arbol deriving Show

aparece :: Int -> Arbol -> Bool
aparece x (Hoja n) = if x == n then True else False
aparece x (Nodo left r right) = if (aparece x left || aparece x right || x == r) then True else False

aplanaPO :: Arbol -> [Int]
aplanaPO (Hoja n) = [n]
aplanaPO (Nodo left r right) = r:(aplanaPO left ++ aplanaPO right)

aplanaIO :: Arbol -> [Int]
aplanaIO (Hoja n) = [n]
aplanaIO (Nodo left r right) = (aplanaIO left) ++ r:(aplanaIO right)

aplanaPsO :: Arbol -> [Int]
aplanaPsO (Hoja n) = [n]
aplanaPsO (Nodo left r right) = (aplanaPsO left ++ aplanaPsO right) ++ [r]

-- (3)Listas

data Lista = Vacia | Conc Int Lista deriving Show

long :: Lista -> Int
long Vacia = 0
long (Conc n Vacia) = 1
long (Conc n (Conc m l)) = 1 + (long (Conc n l))

takeL :: Int -> Lista -> Lista
takeL 0 l = Vacia
takeL 1 (Conc n (Conc m l)) = (Conc n Vacia)
takeL n Vacia = error "Error"
takeL x (Conc n (Conc m l)) = Conc n (takeL (x-1) (Conc m l))

dropL :: Int -> Lista -> Lista
dropL 0 Vacia = Vacia
dropL 1 (Conc n (Conc m l)) = Conc m l
dropL x (Conc n (Conc m l)) = dropL (x-1) (Conc m l)

concatena :: Lista -> Lista -> Lista
concatena l Vacia = l
concatena Vacia m = m
concatena (Conc a b) c = Conc a (concatena b c)

reversa :: Lista -> Lista
reversa Vacia = Vacia
reversa (Conc n Vacia) = (Conc n Vacia)
reversa (Conc n (Conc m l)) = concatena (reversa (Conc m l)) (Conc n Vacia)

-- (4)Relaciones

type ProductoBI = [(Int,Int)]
type RelacionBI = ProductoBI

union :: RelacionBI -> RelacionBI -> RelacionBI
union [] [] = []
union (x:xs) [] = xs
union [] (y:ys) = ys
union (x:xs) (y:ys) = eliminaR((x:xs) ++ (y:ys))

interseccion :: RelacionBI -> RelacionBI -> RelacionBI
interseccion [] [] = []
interseccion (x:xs) [] = []
interseccion [] (y:ys) = []
interseccion (x:xs) (y:ys) = if pertenece y (x:xs) then [y] ++ interseccion (x:xs) ys else interseccion (x:xs) ys

diferencia :: RelacionBI -> RelacionBI -> RelacionBI
diferencia [] [] = []
diferencia (x:xs) [] = xs
diferencia [] (y:ys) = []
diferencia (x:xs) (y:ys) = if pertenece x (y:ys) then diferencia xs (y:ys) else [x] ++ (diferencia xs (y:ys))

inversa :: RelacionBI -> RelacionBI
inversa [] = []
inversa ((x,y):xs) = [(y,x)] ++ inversa xs

composicion :: RelacionBI -> RelacionBI -> RelacionBI
composicion [] [] = []
composicion (x:xs) (y:ys) = if pertenece y (x:xs) then composicion ys (x:xs) else [y] ++ (composicion ys (x:xs))

reflexividad :: RelacionBI -> Bool
reflexividad [] = False
reflexividad ((a,b):xs) = pertRefl(quitaRepetidos(elementos ((a,b):xs))) ((a,b):xs)

antirreflexividad :: RelacionBI -> Bool
antirreflexividad [] = True
antirreflexividad ((a,b):xs) = if ((a /= b) && noPertenece (a,a) xs) then antirreflexividad xs else if pertenece (a,a) xs then True else False

simetria :: RelacionBI -> Bool
simetria [] = True
simetria ((a,b):xs) = if ((a == b) || pertenece (b,a) xs) then simetria xs else False

antisimetria :: RelacionBI -> Bool
antisimetria [] = True
antisimetria ((a,b):xs) = if (a == b) then antisimetria xs else False

asimetria :: RelacionBI -> Bool
asimetria [] = False
asimetria ((a,b):xs) = if ((a /= b && noPertenece (b,a) xs)) then asimetria xs else False

transitividad :: RelacionBI -> Bool
transitividad [] = True
transitividad ((a,b):xs) = (pertAC(axrAC a (first b ((a,b):xs))) xs) && transitividad xs

--Funciones auxiliares

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = if ((n == x) || (pertenece n xs)) then True else False

noPertenece :: Eq a => a -> [a] -> Bool
noPertenece n [] = True
noPertenece n (x:xs) = if ((n /= x) && (noPertenece n xs)) then True else False

eliminaR :: Eq a => [a] -> [a]
eliminaR [] = []
eliminaR (x:xs) = if pertenece x xs then eliminaR xs else [x] ++ eliminaR xs

elementos :: RelacionBI -> [Int]
elementos [] = []
elementos ((x,y):xs) = [x] ++ [y] ++ elementos xs

quitaRepetidos :: [Int] -> [Int]
quitaRepetidos [] = []
quitaRepetidos (x:xs) = if pertenece x xs then quitaRepetidos xs else [x] ++ quitaRepetidos xs

pertRefl :: [Int] -> RelacionBI -> Bool
pertRefl [] l = False
pertRefl [x] l = pertenece (x,x) l
pertRefl (x:xs) l = if pertenece (x,x) l then pertRefl xs l else False

first :: Int -> RelacionBI -> RelacionBI
first r [] = []
first r ((x,y):xs) = if r == x then [(x,y)] ++ first r xs else first r xs

axrAC :: Int -> RelacionBI -> RelacionBI
axrAC a [] = []
axrAC a ((b,c):xs) = [(a,c)] ++ axrAC a xs

pertAC :: RelacionBI -> RelacionBI -> Bool
pertAC [] l = True
pertAC [x] l = pertenece x l
pertAC ((a,c):xs) l = if pertenece (a,c) l then pertAC xs l else False
