data Prop = T | F | Var String | Neg Prop | Conj Prop  Prop | Disy Prop Prop | Imp Prop Prop | Equiv Prop Prop deriving (Show, Eq)

elimImp :: Prop -> Prop
elimImp (Imp p q) = Disy (Neg p) q
elimImp (Equiv p q) = Conj (Disy (Neg p) q) (Disy (Neg q) p)

elimNeg :: Prop -> Prop
elimNeg (Neg (Neg p)) = p
elimNeg (Neg (Disy p q)) = (Conj (Neg p) (Neg q))
elimNeg (Neg (Conj p q)) = (Disy (Neg p) (Neg q))
elimNeg (Neg (Imp p q)) = (Conj p (Neg q))
elimNeg (Neg (Equiv p q)) = (Disy (Conj p (Neg q)) (Conj q (Neg p)))

dist :: Prop -> Prop
dist (Conj x (Disy y z)) = Disy (Conj x y) (Conj x z)

distC :: Prop -> Prop
distC (Disy x (Conj y z)) = Conj (Disy x y) (Disy x z)

formaNormalD :: Prop -> Prop
formaNormalD p = dist (elimImp (elimNeg p))

formaNormalC :: Prop -> Prop
formaNormalC p = distC (elimImp (elimNeg p))
