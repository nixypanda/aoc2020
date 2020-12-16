{-# LANGUAGE RecordWildCards #-}

module Lib.Range where


type Location = (Int, Int)

data Range = Range { lower :: Int, upper :: Int}
    deriving (Show, Eq)

-- >>> mid $ Range 0 127
-- 63
-- >>> mid $ Range 0 63
-- 31
-- >>> mid $ Range 32 47
-- 39
mid :: Range -> Int
mid Range {..} = lower + (upper - lower) `div` 2

-- >>> lowerHalf $ Range 0 127
-- Range {lower = 0, upper = 63}
-- >>> lowerHalf . lowerHalf $ Range 0 127
-- Range {lower = 0, upper = 31}
lowerHalf :: Range -> Range
lowerHalf r = r { upper = mid r}

-- >>> lowerHalf . lowerHalf . upperHalf . upperHalf . lowerHalf . upperHalf . lowerHalf $ Range 0 127
-- Range {lower = 44, upper = 44}
-- >>> upperHalf . lowerHalf . upperHalf $ Range 0 7
-- Range {lower = 5, upper = 5}
upperHalf :: Range -> Range
upperHalf r = r { lower = mid r + 1}

inRangeInc :: Int -> Range -> Bool
inRangeInc x Range{..} = x >= lower && x <= upper

isSubsumedBy :: Range -> Range -> Bool
isSubsumedBy (Range l1 u1) (Range l2 u2) = l1 >= l2 && u1 <= u2

data SplitDirection = B | F | R | L
    deriving (Show)

fromChar :: Char -> Maybe SplitDirection
fromChar 'B' = Just B
fromChar 'F' = Just F
fromChar 'R' = Just R
fromChar 'L' = Just L
fromChar _ = Nothing


data GridRange = GridRange {rowRange :: Range, colRange :: Range}
    deriving (Show)

-- >>> bisect base B
-- Variable not in scope: base :: GridRange
bisect :: GridRange -> SplitDirection -> GridRange
bisect s@GridRange {..} B = s {rowRange = upperHalf rowRange}
bisect s@GridRange {..} F = s {rowRange = lowerHalf rowRange}
bisect s@GridRange {..} R = s {colRange = upperHalf colRange}
bisect s@GridRange {..} L = s {colRange = lowerHalf colRange}

-- >>> narrowRange base [B,F,F,F,B,B,F,R,R,R]
-- Variable not in scope: base :: GridRange
narrowRange :: GridRange -> [SplitDirection] -> GridRange
narrowRange = foldl bisect


-- >>> toLocation $ GridRange {rowRange = Range {lower = 70, upper = 70}, colRange = Range {lower = 7, upper = 7}}
-- Just (70,7)
toLocation :: GridRange -> Maybe Location
toLocation (GridRange (Range rl ru) (Range cl cu))
  | rl == ru && cl == cu = Just (rl, cl)
  | otherwise = Nothing

