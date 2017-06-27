data Tree a = Fork a (Forest a) deriving Show
data Forest a = Null | Grows (Tree a) (Forest a) deriving Show

foldt :: (a -> f -> t, f, t -> f -> f) -> Tree a -> t
foldt (g,c,h) (Fork x xs) = g x (foldf (g,c,h) xs)

foldf :: (a -> f -> t, f, t -> f -> f) -> Forest a -> f
foldf (g,c,h) Null = c
foldf (g,c,h) (Grows x xs) = h (foldt (g,c,h) x) (foldf (g,c,h) xs)

unfoldt :: (x -> (a, (x', y)), y -> Maybe ((x, y'), (x', y))) -> (x, y') -> Tree a
unfoldt (phi, psi) (x, y) = case phi x of
  (a, y') -> Fork a (unfoldf (phi, psi) y')

unfoldf :: (x -> (a, (x', y)), y -> Maybe ((x, y'), (x', y))) -> (x', y) -> Forest a
unfoldf (phi, psi) (x, y) = case psi y of
  Nothing -> Null
  Just (x', y') -> Grows (unfoldt (phi, psi) x') (unfoldf (phi, psi) y')
