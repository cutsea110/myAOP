module Triv where

import Prelude hiding (maybe, either)

-- Maybe
data Option a = None | Some a deriving Show

maybe :: (x, a -> x) -> Option a -> x
maybe (c, f) = mu
  where
    mu None = c
    mu (Some a) = f a

unmaybe :: (x -> Maybe a) -> x -> Option a
unmaybe psi = nu
  where
    nu x = case psi x of
      Nothing -> None
      Just a -> Some a

-- Either
data Gender a b = Male a | Female b deriving Show

either :: (a -> x, b -> x) -> Gender a b -> x
either (f, g) = mu
  where
    mu (Male a) = f a
    mu (Female b) = g b

uneither :: (x -> Either a b) -> x -> Gender a b
uneither psi = nu
  where
    nu x = case psi x of
      Left a -> Male a
      Right b -> Female b
