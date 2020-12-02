module Lib.ListUtils where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- day 01

allTwoCombinations :: [Int] -> [[Int]]
allTwoCombinations l = fmap (\(x,y) -> [x, y]) $ (,) <$> l <*> l


allThreeCombinations :: [Int] -> [[Int]]
allThreeCombinations l = fmap (\(x, y, z) -> [x, y, z]) $  (,,) <$> l <*> l <*> l


-- >>> oneWithSum 6  [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
-- [3,3]
oneWithSum :: Int -> [[Int]] -> Maybe [Int]
oneWithSum target = safeHead . filter ((== target) . sum)


