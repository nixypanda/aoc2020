module Lib.Math where

import Prelude hiding (gcd)

-- Extended GCD
-- Bezout Law: a*s + b*t = gcd(a, b)
egcd :: Integral c => c -> c -> (c, c, c)
egcd a 0 = (a, 0, 1)
egcd a b = (g, t - (a `div` b) * s, s)
    where (g, s, t) = egcd b (a `mod` b)

-- Modular Inverse
inv :: Integral a => a -> a -> a
a `inv` m = let (_, i, _) = egcd m a in i `mod` m

-- Chinese Remainder Theorm
-- >>> crt [(2,7), (0,3), (1,5)]
-- (51,105)
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
    where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1


-- >>> gcd' 45 10
-- 5
-- >>> gcd' 3768 1701
-- 3
-- >>> gcd' 56 15
-- 3
gcd' :: Integral t => t -> t -> t
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)
