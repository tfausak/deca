module Deca.Internal where

import qualified Data.Ratio as Ratio

data DecaOf significand exponent
    = Deca significand exponent
    deriving (Eq, Show)

instance (Integral s, Integral e) => Fractional (DecaOf s e) where
    fromRational = Deca.Internal.fromRational
    (/) = divide

instance (Integral s, Num e, Ord e) => Num (DecaOf s e) where
    abs = Deca.Internal.abs
    fromInteger = Deca.Internal.fromInteger
    negate = Deca.Internal.negate
    signum = Deca.Internal.signum
    (+) = add
    (*) = multiply

instance (Num s, Num e, Ord s, Ord e) => Ord (DecaOf s e) where
    compare = Deca.Internal.compare

instance (Integral s, Integral e) => Real (DecaOf s e) where
    toRational = Deca.Internal.toRational

instance (Integral s, Integral e) => RealFrac (DecaOf s e) where
    properFraction = Deca.Internal.properFraction

abs :: Num s => DecaOf s e -> DecaOf s e
abs (Deca s e) = Deca (Prelude.abs s) e

add :: (Integral s, Num e, Ord e) => DecaOf s e -> DecaOf s e -> DecaOf s e
add x y =
    let (Deca s1 e1, Deca s2 _) = denormalize x y
    in deca (s1 + s2) e1

compare :: (Num s, Num e, Ord s, Ord e) => DecaOf s e -> DecaOf s e -> Ordering
compare x y =
    let (Deca s1 _, Deca s2 _) = denormalize x y
    in Prelude.compare s1 s2

deca :: (Integral s, Num e) => s -> e -> DecaOf s e
deca s e = normalize $ Deca s e

denormalize :: (Num s, Num e, Ord e) => DecaOf s e -> DecaOf s e -> (DecaOf s e, DecaOf s e)
denormalize (Deca s1 e1) (Deca s2 e2) = case Prelude.compare e1 e2 of
    LT -> denormalize (Deca s1 e1) (Deca (s2 * 10) (e2 - 1))
    EQ -> (Deca s1 e1, Deca s2 e2)
    GT -> denormalize (Deca (s1 * 10) (e1 - 1)) (Deca s2 e2)

divide :: (Integral s, Integral e) => DecaOf s e -> DecaOf s e -> DecaOf s e
divide x y = Prelude.fromRational $ Prelude.toRational x / Prelude.toRational y

factor :: (Num a, Integral b) => b -> a -> b -> (a, b)
factor d c n =
    let (q, r) = quotRem n d
    in if n /= 0 && r == 0
    then factor d (c + 1) q
    else (c, n)

fromInteger :: (Integral s, Num e) => Integer -> DecaOf s e
fromInteger s = deca (Prelude.fromInteger s) 0

fromRational :: (Integral s, Integral e) => Rational -> DecaOf s e
fromRational r =
    let
        n = Ratio.numerator r
        d1 = Ratio.denominator r
        (a, d2) = factor 2 0 d1
        (b, d3) = factor 5 0 d2
        e = max a b
    in if d3 == 1
    then deca (Prelude.fromInteger n * 2 ^ (e - a) * 5 ^ (e - b)) (Prelude.negate e)
    else error $ "fromRational: " <> show r

multiply :: (Integral s, Num e) => DecaOf s e -> DecaOf s e -> DecaOf s e
multiply (Deca s1 e1) (Deca s2 e2) = deca (s1 * s2) (e1 + e2)

negate :: Num s => DecaOf s e -> DecaOf s e
negate (Deca s e) = Deca (Prelude.negate s) e

normalize :: (Integral s, Num e) => DecaOf s e -> DecaOf s e
normalize (Deca s e) = if s == 0
    then Deca s 0
    else let (q, r) = quotRem s 10 in if r == 0
        then normalize $ Deca q (e + 1)
        else Deca s e

properFraction :: (Integral s, Integral e, Integral b) => DecaOf s e -> (b, DecaOf s e)
properFraction = fmap Prelude.fromRational . Prelude.properFraction . Prelude.toRational

signum :: (Num s, Num e) => DecaOf s e -> DecaOf s e
signum (Deca s _) = Deca (Prelude.signum s) 0

toRational :: (Integral e, Real s) => DecaOf s e -> Rational
toRational (Deca s e) = if e < 0
    then Prelude.toRational s / (10 ^ Prelude.negate e)
    else Prelude.toRational s * (10 ^ e)
