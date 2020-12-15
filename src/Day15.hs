{-# LANGUAGE RecordWildCards #-}
module Day15 (d15p1, d15p2) where

import Control.Monad ( foldM, forM_ )
import Control.Monad.ST ( ST, runST )
import Data.Array.ST ( Ix, readArray, writeArray, MArray(newArray), STUArray )
import Data.List.Split ( splitOn )

-- parse

parse :: String -> [Int]
parse = fmap read . splitOn ","


-- Part 1

update :: (MArray a b m, Ix i, Eq b, Num b) => a i b -> i -> b -> m b
update arr v turn = do
    val <- readArray arr v
    let newV = if val == -1 then 0 else turn - val
    writeArray arr v turn
    pure newV

init' :: (MArray a e m, Ix i, Num i, Num e, Enum e) => i -> [i] -> m (a i e)
init' n input = do
    array <- newArray (0, n) (-1)
    forM_ (zip [1..] input) (\(i, val) -> writeArray array val i)
    pure array

run :: Int -> [Int] -> Int
run n input = runST $ do
    array <- init' n input :: ST s (STUArray s Int Int)
    foldM (update array) 0 [length input + 1 .. n - 1]


d15p1 :: IO ()
d15p1 = getContents >>= print . run 2020 . parse


-- Part 2

d15p2 :: IO ()
d15p2 = getContents >>= print . run 30000000 . parse

