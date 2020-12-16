module Lib.Utils where

import Text.Parsec ( digit, many )
import Text.Parsec.String ( Parser )

xor :: Bool -> Bool -> Bool
xor = (/=)

between :: Ord a => a -> a -> a -> Bool
between a b c = a <= c && b >= c

all' :: (Foldable t, Functor t) => (a -> Bool) -> t a -> Bool
all' f = all (== True) . fmap f

number :: Parser Int
number = read <$> many digit

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

