data Tree a = Fork a (Forest a) deriving Show
data Forest a = Null | Grows (Tree a) (Forest a) deriving Show

foldt :: (a -> f -> t, f, t -> f -> f) -> Tree a -> t
foldt (g,c,h) (Fork x xs) = g x (foldf (g,c,h) xs)

foldf :: (a -> f -> t, f, t -> f -> f) -> Forest a -> f
foldf (g,c,h) Null = c
foldf (g,c,h) (Grows x xs) = h (foldt (g,c,h) x) (foldf (g,c,h) xs)

unfoldt :: (t3 -> (a, (t1, t)), t -> Maybe ((t3, t2), (t1, t))) -> (t3, t2) -> Tree a
unfoldt (phi, psi) (x, y) = case phi x of
  (a, y') -> Fork a (unfoldf (phi, psi) y')

unfoldf :: (t3 -> (a, (t1, t)), t -> Maybe ((t3, t2), (t1, t))) -> (t1, t) -> Forest a
unfoldf (phi, psi) (x, y) = case psi y of
  Nothing -> Null
  Just (x', y') -> Grows (unfoldt (phi, psi) x') (unfoldf (phi, psi) y')
