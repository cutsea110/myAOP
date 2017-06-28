module Exp where

import Prelude hiding (curry)

-- Exponentials
data Exp a b = Exp (b -> a)

curry :: ((c, b) -> a) -> c -> Exp a b
curry f c = Exp (\b -> f (c, b))

apply :: (Exp a b, b) -> a
apply (Exp g, x) = g x

-- This is right?
cata :: (a -> (c, b)) -> (Exp a b, b) -> (c, b)
cata phi = mu
  where
    mu (Exp g, b) = phi (g b)

ana :: ((c, b) -> a) -> (c, b) -> (Exp a b, b)
ana psi = nu
  where
    -- :m +Control.Arrow 
    -- nu = curry psi *** id
    nu (c, b) = (curry psi c, b)
