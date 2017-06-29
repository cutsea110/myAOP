module Exp where

import Prelude hiding (curry)

-- Exponentials
data Exp a b = Exp (b -> a)

curry :: ((c, b) -> a) -> c -> Exp a b
curry f c = Exp (\b -> f (c, b))

apply :: (Exp a b, b) -> a
apply (Exp g, x) = g x

-- curry apply == id
-- apply (curry (\(x,y) -> x + y) 3, 4)

-- This is right?
cata :: (a -> (c, b)) -> (Exp a b, b) -> (c, b)
cata phi = mu
  where
    mu (Exp g, b) = phi (g b)

-- cata (\n -> (n, n + 1)) (Exp (+1), 3)

ana :: ((c, b) -> a) -> (c, b) -> (Exp a b, b)
ana psi = nu
  where
    -- :m +Control.Arrow 
    -- nu = curry psi *** id
    nu (c, b) = (curry psi c, b)

-- apply (ana (\(x,y) -> x + y) (3,4))

-- cata . ana :: ((c, b) -> a) -> (Exp (c, b) b, b) -> (Exp a b, b)
-- ana . cata :: (a -> (c, b)) -> (Exp a b, b) -> (Exp (c, b) b, b)
