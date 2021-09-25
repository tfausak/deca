module Deca where

import qualified Deca.Internal as Internal

type Deca = Internal.DecaOf Integer Integer

deca :: Integer -> Integer -> Deca
deca = Internal.deca

significand :: Deca -> Integer
significand (Internal.Deca s _) = s

exponent :: Deca -> Integer
exponent (Internal.Deca _ e) = e
