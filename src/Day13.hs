module Day13 (d13p1, d13p2) where

import Control.Arrow ( Arrow((&&&)) )
import Data.List ( minimumBy )
import Data.Ord ( comparing )
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.List.Split ( splitOn )
import Lib.ListUtils ( tuplify )

import Lib.Math ( crt ) 

-- Parsing

-- >>> parse "939\n7,13,x,x,59,x,31,19"
-- (939,[7,13,1,1,59,1,31,19])
parse :: String -> (Integer, [Integer])
parse = bimap read (fmap ( read . \x -> if x == "x" then "1" else x) . splitOn ",") .  tuplify . lines


-- Part 1

-- >>> nextSlotAfter 939 7
-- 945
-- >>> nextSlotAfter 939 13
-- 949
-- >>> nextSlotAfter 939 59
-- 944
nextSlotAfter :: Integral a => a -> a -> a
nextSlotAfter arrivalTime n = n * ((arrivalTime `div` n) + 1)

-- >>> bestSlotAndBus 939 [7,13,59,31,19]
-- (59,944)
bestSlotAndBus :: (Foldable t, Functor t, Integral a) => a -> t a -> (a, a)
bestSlotAndBus arrivalTime = minimumBy (comparing snd) . fmap (id &&& nextSlotAfter arrivalTime )

-- >>> p1 (939, [7,13,59,31,19])
-- 295
-- >>> p1 (939,[7,13,1,1,59,1,31,19])
-- 295
p1 :: (Integer, [Integer]) -> Integer
p1 (arrivalTime, buses) = uncurry (*) . fmap (subtract arrivalTime) $ bestSlotAndBus arrivalTime buses'
    where buses' = filter (/= 1) buses


d13p1 :: IO ()
d13p1 = getContents >>= print . p1 . parse


-- part 2

-- >>> p2 (939, [7,13,59,31,19])
-- [(0,7),(1,13),(2,59),(3,31),(4,19)]
-- >>> p2 (939,[7,13,1,1,59,1,31,19])
-- 1068781
-- >>> p2 (939,[17,1,13,19])
-- 3417
-- >>> p2 (939,[67,7,59,61])
-- 754018
-- >>> p2 (939,[67,1,7,59,61])
-- 779210
-- >>> p2 (939,[1789,37,47,1889])
-- 1202161486
p2 :: (a, [Integer]) -> Integer
p2 = uncurry (flip (-)) . crt . filter (\(_, i) -> i /= 1) . zip [0..] . snd


d13p2 :: IO ()
d13p2 = getContents >>= print . p2 . parse
