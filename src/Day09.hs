module Day09 (d9p1, d9p2) where

import Prelude hiding (length)
import Data.Vector (Vector, toList, slice, (!), fromList, length)
import Data.Maybe (isJust)
import Control.Arrow ((&&&))

import Lib.ListUtils (allTwoCombinations, oneWithSum, allContinousCombinations)


-- >>> elmAtIndexFollowsRule 25 25 (fromList $ [1..25] ++ [26,49,50])
-- True
-- >>> elmAtIndexFollowsRule 25 26 (fromList $ [1..25] ++ [26,49,50])
-- True
-- >>> elmAtIndexFollowsRule 25 27 (fromList $ [1..25] ++ [26,49,100])
-- False
elmAtIndexFollowsRule :: Vector Int -> Int -> Int -> Bool
elmAtIndexFollowsRule vec preambleSize currIndex =
    isJust . oneWithSum curr . allTwoCombinations $ elmsToCheck
    where
        elmsToCheck = toList $ slice (currIndex - preambleSize) preambleSize vec
        curr = vec ! currIndex


-- >>> oneThatDoesNotFollowTheRule (fromList $ [1..25] ++ [26,49,100])
-- 100
oneThatDoesNotFollowTheRule :: Vector Int -> Int
oneThatDoesNotFollowTheRule vec =
    head . fmap (vec !) . filter (not . elmAtIndexFollowsRule vec 25) $ [25..(length vec - 1)]


parse :: String -> Vector Int
parse = fromList . fmap read . lines

d9p1 :: IO ()
d9p1 = getContents >>= print . oneThatDoesNotFollowTheRule . parse


-- part 2

continousSubsetsWithSum :: (Eq a, Num a) => a -> [a] -> [[a]]
continousSubsetsWithSum s = filter ((== s) . sum) . allContinousCombinations

p2 :: Vector Int -> Int
p2 vec = sumMinMax . head . continousSubsetsWithSum sumTo $ toList vec
    where
        sumMinMax = uncurry (+) . (minimum &&& maximum)
        sumTo = oneThatDoesNotFollowTheRule vec

d9p2 :: IO ()
d9p2 = getContents >>= print . p2 . parse

