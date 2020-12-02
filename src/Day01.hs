module Day01
    ( d1p1
    , d1p2
    ) where

import Lib.ListUtils ( allTwoCombinations, allThreeCombinations, oneWithSum )


-- >>> productOfSum allTwoCombinations [1721,979,366,299,675,1456]
-- 514579
productOfSumUpto :: Int -> ([Int] -> [[Int]]) -> [Int] -> Maybe Int
productOfSumUpto n f = fmap product . oneWithSum n . f

driver :: (Show a, Read b) => ([b] -> a) -> IO ()
driver f = getContents >>= (print . f . fmap read . lines)

d1p1 :: IO ()
d1p1 = driver (productOfSumUpto 2020 allTwoCombinations)


d1p2 :: IO ()
d1p2 = driver (productOfSumUpto 2020 allThreeCombinations)
