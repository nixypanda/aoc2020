module Day10 (d10p1, d10p2) where

import Control.Arrow ( Arrow((&&&)) ) 
import Data.List ( sort )

parse :: String -> [Int]
parse = fmap read . lines

addBuiltInAdapter :: (Num a, Ord a) => [a] -> [a]
addBuiltInAdapter xs = maximum xs + 3 : xs

addSocket :: [Int] -> [Int]
addSocket = (0:)


-- >>> diffOf 1 [0,1,4,5,6,7,10,11,12,15,16,19,22]
-- 7
-- >>> diffOf 3 [0,1,4,5,6,7,10,11,12,15,16,19,22]
-- 5
-- >>> diffOf 2 [0,1,4,5,6,7,10,11,12,15,16,19,22]
-- 0
diffOf :: (Eq a, Num a) => a -> [a] -> Int
diffOf n xs = length . filter (== n) $ zipWith (-) (tail xs) xs

-- >>> p1 [16,10,15,5,1,11,7,19,6,12,4]
-- 35
p1 :: [Int] -> Int
p1 = uncurry (*) . (diffOf 1 &&& diffOf 3) . sort . addSocket . addBuiltInAdapter

d10p1 :: IO ()
d10p1 = getContents >>= print . p1 . parse


-- >>> f $ reverse [0,1,4,5,6,7,10,11,12,15,16,19,22]
-- [(22,8),(19,8),(16,8),(15,8),(12,8),(11,4),(10,4),(7,4),(6,2),(5,1),(4,1),(1,1),(0,1)]
arrangements :: [Int] -> [(Int, Int)]
arrangements [x] = [(x,1)]
arrangements (x:xs) = (x, waysToCombine) : others
    where
        others = arrangements xs
        canCombineWith = takeWhile (\e -> fst e >= x - 3) others
        waysToCombine = sum $ fmap snd canCombineWith

-- >>> p2 [0,1,4,5,6,7,10,11,12,15,16,19,22]
-- 8
-- >>> p2 [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]
-- 19208
p2 :: [Int] -> Int
p2 = snd . head . arrangements . reverse . sort . addSocket . addBuiltInAdapter

d10p2 :: IO ()
d10p2 = getContents >>= print . p2 . parse
