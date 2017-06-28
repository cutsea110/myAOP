module Triv where

import Prelude hiding (maybe)

-- Maybe
data Option a = None | Some a deriving Show

maybe :: (x, a -> x) -> Option a -> x
maybe (c, f) = u
  where
    u None = c
    u (Some a) = f a

unmaybe :: (x -> Maybe a) -> x -> Option a
unmaybe phi = u
  where
    u x = case phi x of
      Nothing -> None
      Just a -> Some a
