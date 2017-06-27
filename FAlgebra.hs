module FAlgebra where

import Prelude hiding (foldr, unfoldr)

-- Nat
data Nat = Zero | Succ Nat deriving Show

foldn :: (x, x -> x) -> Nat -> x
foldn (c, f) = mu
  where
    mu Zero = c
    mu (Succ n) = f (mu n)

unfoldn :: (x -> Maybe x) -> x -> Nat
unfoldn psi = nu
  where
    nu x = case psi x of
      Nothing -> Zero
      Just x' -> Succ (nu x')

-- List
data List a = Nil | Cons a (List a) deriving Show

foldr :: (x, a -> x -> x) -> List a -> x
foldr (c, f) = mu
  where
    mu Nil = c
    mu (Cons x xs) = f x (mu xs)

unfoldr :: (x -> Maybe (a, x)) -> x -> List a
unfoldr psi = nu
  where
    nu x = case psi x of
      Nothing -> Nil
      Just (a, x') -> Cons a (nu x')

-- Tree
data Tree a = Tip a | Bin (Tree a) (Tree a) deriving Show

foldtree :: (a -> x, x -> x -> x) -> Tree a -> x
foldtree (f, g) = mu
  where
    mu (Tip a) = f a
    mu (Bin tl tr) = g (mu tl) (mu tr)

unfoldtree :: (x -> Either a (x, x)) -> x -> Tree a
unfoldtree psi = nu
  where
    nu x = case psi x of
      Left a -> Tip a
      Right (xl, xr) -> Bin (nu xl) (nu xr)

-- List^{+}
data ListPlus a = Wrap a | ConsPlus a (ListPlus a) deriving Show

foldrplus :: (a -> x, a -> x -> x) -> ListPlus a -> x
foldrplus (f, g) = mu
  where
    mu (Wrap a) = f a
    mu (ConsPlus a x) = g a (mu x)

unfoldrplus :: (x -> Either a (a, x)) -> x -> ListPlus a
unfoldrplus psi = nu
  where
    nu x = case psi x of
      Left a -> Wrap a
      Right (a, x') -> ConsPlus a (nu x')

-- Snoc List
data SList a = SNil | Snoc (SList a) a deriving Show

foldl :: (x, x -> a -> x) -> SList a -> x
foldl (c, f) = mu
  where
    mu SNil = c
    mu (Snoc x a) = f (mu x) a

unfoldl :: (x -> Maybe (x, a)) -> x -> SList a
unfoldl psi = nu
  where
    nu x = case psi x of
      Nothing -> SNil
      Just (x', a) -> Snoc (nu x') a
