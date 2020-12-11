module Lib.Grid where

import Data.Vector ( Vector, (!), (!?) )
import qualified Data.Vector as V
import Data.Maybe ( fromJust ) 


type Location = (Int, Int)
type Grid a = Vector (Vector a)

iiMap :: (Int -> Int -> a -> b) -> Grid a -> Grid b
iiMap f = V.imap (V.imap . f)

fromLists :: [[a]] -> Grid a
fromLists = V.fromList . fmap V.fromList

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

toLists :: Grid a -> [[a]]
toLists = fmap V.toList . V.toList

(!?!?) :: Grid a -> Location -> Maybe a
layout !?!? (row, column) = layout !? row >>= (!? column)

inBoundsOf :: Grid a -> Location -> Bool
inBoundsOf vec (row, col) =
    row >= 0 && col >=0 && row < V.length vec && col < V.length (vec ! 0)

-- >>> [L .. BL]
-- [L,TL,T,TR,R,RB,B,BL]
data Direction = L | TL | T | TR | R | RB | B | BL deriving (Show, Eq, Enum)

locationsFrom :: Location -> Direction -> [Location]
locationsFrom (row, col) d  = drop 1 . zip (iterate rowF row) $ iterate colF col
    where
        (rowF, colF) = case d of
              L  -> (id,         subtract 1)
              TL -> (subtract 1, subtract 1)
              T  -> (subtract 1, id)
              TR -> (subtract 1, (+ 1))
              R  -> (id,         (+ 1))
              RB -> ((+ 1),      (+ 1))
              B  -> ((+ 1),      id)
              BL -> ((+ 1),      subtract 1)

layoutAround :: Location -> Grid a -> [[a]]
layoutAround l layout =
    fmap ( fmap (fromJust . (layout !?!?)) . takeWhile (inBoundsOf layout) . locationsFrom l) [L .. BL]


howMany :: (a -> Bool) -> Grid a -> Int
howMany f = sum . fmap (V.length . V.filter f)
