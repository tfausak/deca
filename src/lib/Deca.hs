module Deca where

data Deca significand exponent
    = Deca significand exponent
    deriving (Eq, Show)

instance (Integral s, Num e, Ord e) => Num (Deca s e) where
    abs (Deca s e) = Deca (abs s) e
    fromInteger s = normalize $ Deca (fromInteger s) 0
    negate (Deca s e) = Deca (negate s) e
    signum (Deca s _) = Deca (signum s) 0
    x + y =
        let (Deca s1 e1, Deca s2 _) = denormalize x y
        in normalize $ Deca (s1 + s2) e1
    Deca s1 e1 * Deca s2 e2 = normalize $ Deca (s1 * s2) (e1 + e2)

denormalize :: (Num s, Num e, Ord e) => Deca s e -> Deca s e -> (Deca s e, Deca s e)
denormalize (Deca s1 e1) (Deca s2 e2) = case compare e1 e2 of
    LT -> denormalize (Deca s1 e1) (Deca (s2 * 10) (e2 - 1))
    EQ -> (Deca s1 e1, Deca s2 e2)
    GT -> denormalize (Deca (s1 * 10) (e1 - 1)) (Deca s2 e2)

deca :: (Integral s, Num e) => s -> e -> Deca s e
deca s e = normalize $ Deca s e

normalize :: (Integral s, Num e) => Deca s e -> Deca s e
normalize (Deca s e) = if s == 0
    then Deca s 0
    else let (q, r) = quotRem s 10 in if r == 0
        then normalize $ Deca q (e + 1)
        else Deca s e
