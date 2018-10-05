
-- López Soto Ramses Antono 31531997-4
-- Proyecto 2

data Exp = Num N | Suma Exp Exp | Prod Exp Exp | Paren Exp deriving (Eq,Show)
data N = Dig D | ConsN N D deriving (Eq,Show)
data D = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Eq,Show)

data TreeD = NumT Int | SumaT TreeD TreeD | ProdT TreeD TreeD | ParenT TreeD deriving (Eq,Show)

nToIntAux :: N -> Int -> Int
nToIntAux (Dig D0) m = 0 * (10^m)
nToIntAux (Dig D1) m = 1 * (10^m)
nToIntAux (Dig D2) m = 2 * (10^m)
nToIntAux (Dig D3) m = 3 * (10^m)
nToIntAux (Dig D4) m = 4 * (10^m)
nToIntAux (Dig D5) m = 5 * (10^m)
nToIntAux (Dig D6) m = 6 * (10^m)
nToIntAux (Dig D7) m = 7 * (10^m)
nToIntAux (Dig D8) m = 8 * (10^m)
nToIntAux (Dig D9) m = 9 * (10^m)
nToIntAux (ConsN a b) m = nToIntAux a (m+1) + nToIntAux (Dig b) m --b=D N=Dig D | N= ConsN N D

nToInt :: N -> Int
nToInt a = nToIntAux a 0

intToN :: Int -> N
intToN 0 = Dig D0
intToN 1 = Dig D1
intToN 2 = Dig D2
intToN 3 = Dig D3
intToN 4 = Dig D4
intToN 5 = Dig D5
intToN 6 = Dig D6
intToN 7 = Dig D7
intToN 8 = Dig D8
intToN 9 = Dig D9
intToN n = ConsN (intToN (div n 10)) (nToD(intToN (mod n 10)))

nToD :: N -> D
nToD (Dig a) = a

expToTree :: Exp -> TreeD
expToTree (Num n) = NumT (nToInt (n))
expToTree (Suma n m) = SumaT (expToTree n) (expToTree m)
expToTree (Prod n m) = ProdT (expToTree n) (expToTree m)
expToTree (Paren n) = ParenT (expToTree n)

treeToExp :: TreeD -> Exp
treeToExp (NumT n) = Num (intToN n)
treeToExp (SumaT n m) = Suma (treeToExp n) (treeToExp m)
treeToExp (ProdT n m) = Prod (treeToExp n) (treeToExp m)
treeToExp (ParenT n) = Paren (treeToExp n)

evalT :: TreeD -> Int
evalT (NumT n) = n
evalT (SumaT n m) = (evalT n) + (evalT m)
evalT (ProdT n m) = (evalT n) * (evalT m)
evalT (ParenT n) = (evalT n)

a = Prod (Num (ConsN (Dig D1) D0)) (Suma (Num (ConsN (ConsN (Dig D2) D5) D8)) (Suma (Num (ConsN (Dig D4) D7)) (Num (ConsN (ConsN (Dig D3) D6) D9))))
b = Suma (Num (ConsN (ConsN (ConsN (Dig D1) D0) D0) D0)) (Suma (Prod (Num (ConsN (ConsN (Dig D3) D0) D0)) (Num (Dig D2))) (Num (ConsN (Dig D1) D0)))
c = Suma (Prod (Num (Dig D3)) (Num (ConsN (ConsN (Dig D6) D9) D9))) (Num (Dig D2))
d = Prod (Suma (Num (ConsN (ConsN (Dig D1) D5) D4)) (Num (ConsN (ConsN (ConsN (Dig D8) D0) D1) D6))) (Suma (Num (ConsN (ConsN (Dig D2) D9) D9)) (Num (ConsN (ConsN (Dig D7) D3) D4)))
e = Prod (Suma (Num (ConsN (ConsN (Dig D5) D8) D4)) (Num (ConsN (ConsN (ConsN (Dig D3) D1) D1) D5))) (Num (Dig D2))
f = Suma (Num (Dig D2)) (Num (Dig D2))

p1 = (evalT (expToTree a)) == 6740
p2 = (evalT (expToTree b)) == 1610
p3 = (evalT (expToTree c)) == 2099
p4 = (evalT (expToTree d)) == 8439610
p5 = (evalT (expToTree e)) == 7398
p6 = (evalT (expToTree f)) == 4
