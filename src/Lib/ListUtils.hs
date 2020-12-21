module Lib.ListUtils where

import Data.List (sort, intersect, tails, inits)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

allTwoCombinations :: [Int] -> [[Int]]
allTwoCombinations l = fmap (\(x,y) -> [x, y]) $ (,) <$> l <*> l

allThreeCombinations :: [Int] -> [[Int]]
allThreeCombinations l = fmap (\(x, y, z) -> [x, y, z]) $  (,,) <$> l <*> l <*> l

-- >>> oneWithSum 6  [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
-- [3,3]
oneWithSum :: Int -> [[Int]] -> Maybe [Int]
oneWithSum target = safeHead . filter ((== target) . sum)

isSubListOf :: Ord a => [a] -> [a] -> Bool
isSubListOf a = (== sort a) . sort . intersect a

howMany :: (a -> Bool) -> [a] -> Int
howMany f = length . filter f

tuplify :: [b] -> (b, b)
tuplify [a, b] = (a, b)
tuplify _ = error "Fuck yeah! Error"

tuplify3 :: [c] -> (c, c, c)
tuplify3 [a, b, c] = (a, b, c)
tuplify3 _ = error "Fuck yeah! Error"

-- >>> consectiveMissingNumbers [1,2,3,4,6,7,8,10,11]
-- [5,9]
consectiveMissingNumbers :: (Ord a, Enum a) => [a] -> [a]
consectiveMissingNumbers xs = go (sort xs) [(minimum xs)..]
    where
        go (x:xs) (y:ys)
          | x == y = go xs ys
          | x > y = y : go (x:xs) ys
          | otherwise = error "Impossible"
        go [] _ = []

-- >>> allContinousCombinations [1,2,3]
-- [[],[1],[],[1,2],[2],[],[1,2,3],[2,3],[3],[]]
allContinousCombinations :: [a] -> [[a]]
allContinousCombinations = concatMap tails . inits


hasOneElem :: [a] -> Bool
hasOneElem [_] = True
hasOneElem _ = False

-- >>> scrub [[1], [1,2], [1,2,3] ]
-- [[1],[2],[2,3]]
scrub :: Eq a => [[a]] -> [[a]]
scrub xss = fmap (\xs -> if hasOneElem xs then xs else filter (`notElem` singles) xs) xss
    where singles = head <$> filter hasOneElem xss

