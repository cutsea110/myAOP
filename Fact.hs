import Data.List (unfoldr)

-- fact = foldr (*) 1 . unfoldr (\n -> if n == 0 then Nothing else Just (n, n-1))

foldn :: (a, a -> a) -> Integer -> a
foldn (c, f) n | n == 0 = c
               | n > 0  = f (foldn (c, f) (n-1))
               | otherwise = error "minus is not Nat"

fact :: Integer -> Integer
fact = snd . foldn ((0, 1), f)
  where
    f (m, n) = (m + 1, (m + 1) * n)

fact' :: Integer -> Integer
fact' = foldr (*) 1 . unfoldr (\n -> if n == 0 then Nothing else Just (n, n -1))
