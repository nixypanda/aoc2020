{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Day17 (d17p1, d17p2) where

import Control.Arrow ( Arrow((&&&)) )
import qualified Data.Map as M

type Point3D = (Int, Int, Int)
data Cube = Inactive | Active deriving (Eq, Show)

type Space a = M.Map a Cube

class Point a where
    neighbours :: a -> [a]

instance Point Point3D where
    neighbours :: Point3D -> [Point3D]
    neighbours (x, y, z) =
        [ (x + dx, y + dy, z + dz)
        | dx <- [-1 .. 1] , dy <- [-1 .. 1] , dz <- [-1 .. 1]
        , (dx, dy, dz) /= (0, 0, 0)
        ]


-- Parsing

fromChar :: Char -> Cube
fromChar '.' = Inactive
fromChar '#' = Active

parse :: String -> [(Point3D, Cube)]
parse inp = [((x,y,0), fromChar c) | (x, l) <- zip [0..] (lines inp), (y, c) <- zip [0..] l]


-- Part 1

initialSpace :: (Ord a, Point a) => [(a, Cube)] -> Space a
initialSpace = M.fromList 

step :: (Ord a, Point a) => Space a -> Space a
step m = M.fromList . fmap (id &&& changeCubeState) . concatMap neighbours $ M.keys m
    where
        changeCubeState point = changeCube (M.findWithDefault Inactive point m) (activeNeighbours point)
        activeNeighbours = length . filter (== Active) . fmap (\p -> M.findWithDefault Inactive p m) . neighbours

        changeCube :: Cube -> Int -> Cube
        changeCube Active actN = if actN == 2 || actN == 3 then Active else Inactive
        changeCube Inactive actN = if actN == 3 then Active else Inactive

-- >>> p1 $ parse _test
-- 112
p1 :: (Ord a, Point a) => [(a, Cube)] -> Int
p1 = length . filter (== Active) . M.elems . (!! 6) . iterate step . initialSpace

d17p1 :: IO ()
d17p1 = getContents >>= print . p1 . parse

-- Part 2

type Point4D = (Int, Int, Int, Int)

instance Point Point4D where
    neighbours :: Point4D -> [Point4D]
    neighbours (x, y, z, w) =
        [ (x + dx, y + dy, z + dz, w + dw)
        | dx <- [-1 .. 1] , dy <- [-1 .. 1] , dz <- [-1 .. 1], dw <- [-1 .. 1]
        , (dx, dy, dz, dw) /= (0, 0, 0, 0)
        ]

parse' :: String -> [(Point4D, Cube)]
parse' inp = [((x,y,0,0), fromChar c) | (x, l) <- zip [0..] (lines inp), (y, c) <- zip [0..] l]

d17p2 :: IO ()
d17p2 = getContents >>= print . p1 . parse'
