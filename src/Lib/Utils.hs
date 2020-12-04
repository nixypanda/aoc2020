module Lib.Utils where

xor :: Bool -> Bool -> Bool
xor = (/=)

between :: Ord a => a -> a -> a -> Bool
between a b c = a <= c && b >= c

all' :: (Foldable t, Functor t) => (a -> Bool) -> t a -> Bool
all' f = all (== True) . fmap f
