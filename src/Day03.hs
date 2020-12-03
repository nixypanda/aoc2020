{-# LANGUAGE RecordWildCards #-}

module Day03 (d3p1, d3p2) where


testData :: [[Char]]
testData =
    [ "..##......."
    , "#...#...#.."
    , ".#....#..#."
    , "..#.#...#.#"
    , ".#...##..#."
    , "..#.##....."
    , ".#.#.#....#"
    , ".#........#"
    , "#.##...#..."
    , "#...##....#"
    , ".#..#...#.#"
    ]


data Slope = Slope { right :: Int, down :: Int} deriving (Show)

-- >>> f 0 3 1 $ fmap cycle testData 
-- "..#.##.####"
f :: Int -> Slope -> [[Char]] -> [Char]
f _ _ [] = []
f index s@Slope {..} (xs:xss) = xs !! index : f (index + right) s (drop (down -1) xss)


numTrees :: [Char] -> Int
numTrees = length . filter (== '#')

-- >>> p1 testData 
-- 7
p1 :: Slope -> [[Char]] -> Int
p1 s = numTrees . f 0 s . fmap cycle 

-- >>> p2 testData 
-- 336
p2 :: [[Char]] -> Int
p2 xss = product $ fmap (`p1` xss)
    [ Slope 1 1
    , Slope 3 1
    , Slope 5 1
    , Slope 7 1
    , Slope 1 2
    ]


d3p1 :: IO ()
d3p1 = getContents >>= print . p1 (Slope 3 1) . lines


d3p2 :: IO ()
d3p2 = getContents >>= print . p2 . lines
