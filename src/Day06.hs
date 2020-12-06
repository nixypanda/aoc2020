module Day06 (d6p1, d6p2) where

import Data.List (union, intersect)
import Data.List.Split ( splitOn )


-- read

parse :: String -> [[String]]
parse = fmap lines . splitOn "\n\n"


-- part 1

p1 :: [[String]] -> Int
p1 = sum . fmap (length . foldr1 union)

d6p1 :: IO ()
d6p1 = getContents >>= print . p1 . parse


-- part 2

p2 :: [[String]] -> Int
p2 = sum . fmap (length . foldr1 intersect)

d6p2 :: IO ()
d6p2 = getContents >>= print . p2 . parse
