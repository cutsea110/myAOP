module Exp where

import Prelude hiding (curry)

data Exp a b = Exp (b -> a)

curry :: ((c, b) -> a) -> c -> Exp a b
curry f c = Exp (\b -> f (c, b))

apply :: (Exp a b, b) -> a
apply (Exp g, x) = g x

ana :: ((c, b) -> a) -> (c, b) -> (Exp a b, b)
ana psi = nu
  where
    nu (c, b) = (curry psi c, b)
