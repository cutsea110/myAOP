module FAlgebra where

data Nat = Zero | Succ Nat deriving Show

foldn :: (x, x -> x) -> Nat -> x
foldn (c, f) = mu
  where
    mu Zero = c
    mu (Succ n) = f (mu n)

unfoldn :: (x -> Maybe x) -> x -> Nat
unfoldn phi = nu
  where
    nu x = case phi x of
      Nothing -> Zero
      Just x' -> Succ (nu x')
