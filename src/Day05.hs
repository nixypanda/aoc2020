{-# LANGUAGE RecordWildCards #-}
module Day05 (d5p1, d5p2) where

import Data.Maybe ( fromJust )

import Lib.ListUtils (consectiveMissingNumbers)
import Lib.Range
    ( Range(Range)
    , SplitDirection
    , fromChar
    , narrowRange
    , GridRange(..)
    , Location
    , toLocation
    )


-- part 1

base :: GridRange
base = GridRange {rowRange = Range 0 127, colRange = Range 0 7}

-- >>> seatId $ Seat {row = 70, col = 7}
-- 567
seatId :: Location -> Int
seatId (row, col) = row * 8 + col

-- >>> parse "BFFFBBFRRR\nFFFBBBFRRR"
-- [[B,F,F,F,B,B,F,R,R,R],[F,F,F,B,B,B,F,R,R,R]]
parse :: String -> [[SplitDirection]]
parse = fromJust . mapM (mapM fromChar) . lines

-- >>> occupiedSeats [[B,F,F,F,B,B,F,R,R,R],[F,F,F,B,B,B,F,R,R,R]]
-- [567,119]
occupiedSeats :: [[SplitDirection]] -> [Int]
occupiedSeats = fmap seatId . fromJust . mapM (toLocation . narrowRange base)

p1 :: [[SplitDirection]] -> Int
p1 = maximum . occupiedSeats

d5p1 :: IO ()
d5p1 = getContents >>= print . p1 . parse


-- part 2

p2 :: [[SplitDirection]] -> [Int]
p2 = consectiveMissingNumbers . occupiedSeats

d5p2 :: IO ()
d5p2 = getContents >>= print . p2 . parse
