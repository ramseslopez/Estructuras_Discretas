

type Boolean = Bool

andB :: Boolean -> Boolean -> Boolean
andB False False = False
andB False True = False
andB True False = False
andB True True = True

orB :: Boolean -> Boolean -> Boolean
orB False False = False
orB False True = True
orB True False = True
orB True True = True

notB :: Boolean -> Boolean
notB False = True
notB True = False

nandB :: Boolean -> Boolean -> Boolean
nandB False False = True
nandB False True = True
nandB True False = True
nandB True True = False

norB :: Boolean -> Boolean -> Boolean
norB False False = True
norB False True = False
norB True False = False
norB True True = False

xorB :: Boolean -> Boolean -> Boolean
xorB False False = False
xorB False True = True
xorB True False = True
xorB True True = False

xnorB :: Boolean -> Boolean -> Boolean
xnorB False False = True
xnorB False True = False
xnorB True False = False
xnorB True True = True
