{-# LANGUAGE RecordWildCards #-}
module Day23 (d23p1, d23p2) where

import Data.List ( intercalate )
import qualified Data.IntMap as IM


data Game = Game
    { current :: Int
    , list :: IM.IntMap Int
    , size :: Int
    } deriving (Show)

-- >>> mkGame [3,8,9,1,2,5,4,6,7]
-- Game {current = 3, list = fromList [(1,2),(2,5),(3,8),(4,6),(5,4),(6,7),(7,3),(8,9),(9,1)]}
mkGame :: [Int] -> Game
mkGame xs = Game
    { current = head xs
    , list = IM.fromList $ zip xs (tail xs ++ [head xs])
    , size = length xs + 1
    }

-- >>> disp $ mkGame [3,8,9,1,2,5,4,6,7]
-- [3,8,9,1,2,5,4,6,7]
disp :: Game -> [Int]
disp Game{..} = take (IM.size list) $ go current
    where go element = element : go (list IM.! element)

-- >>> disp . (!! 9) $ iterate game' $ mkGame [3,8,9,1,2,5,4,6,7]
-- [5,7,4,1,8,3,9,2,6]
game' :: Game -> Game
game' g@Game{..} = g {current = fourth, list = updatedMap}
    where
        first = list IM.! current
        second = list IM.! first
        third = list IM.! second
        fourth = list IM.! third
        dest = head $ filter (`notElem` [0,first,second,third]) [(current - i) `mod` size | i <- [1..]]
        afterDest = list IM.! dest
        updatedMap = foldr (\(k, v) acc -> IM.insert k v acc) list [(current, fourth), (dest, first), (third, afterDest)]


-- >>> p1 [3,8,9,1,2,5,4,6,7] 
-- [1,6,7,3,8,4,5,2,9]
p1 :: [Int] -> [Int]
p1 = take 9 . dropWhile (/= 1) . cycle . disp . (!! 100) . iterate game' . mkGame

parse :: [Char] -> [Int]
parse = fmap (read . (:[])) . init

d23p1 :: IO ()
d23p1 = getContents >>= print . intercalate "" . fmap show . p1 . parse


-- Part 2

p2 :: [Int] -> Int
p2 = product . take 3 . dropWhile (/= 1) . disp . (!! (10^7)) . iterate game' . mkGame . toInput
    where
        toInput xs = xs ++ [(maximum xs + 1) .. (10^6)]

d23p2 :: IO ()
d23p2 = getContents >>= print . p2 . parse
