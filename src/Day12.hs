{-# LANGUAGE RecordWildCards #-}
module Day12 (d12p1, d12p2) where

import Lib.Geometry 

data ShipDirection = North' | South' | East' | West' | Forward' | Left' | Right'
    deriving (Show, Eq)

type WeightedShipDirection = (ShipDirection, Double)

-- >>> simplify East [(Forward', 10), (North', 3), (Forward', 7), (Right', 90), (Forward', 11)]
-- [(East,10.0),(North,3.0),(East,7.0),(South,11.0)]
simplify :: CompassDirection -> [WeightedShipDirection] -> [WeightedCompassDirection]
simplify _                []  = []
simplify c ((North',   v):ds) = (North, v) : simplify c ds
simplify c ((South',   v):ds) = (South, v) : simplify c ds
simplify c ((East',    v):ds) = (East,  v) : simplify c ds
simplify c ((West',    v):ds) = (West,  v) : simplify c ds
simplify c ((Left',    v):ds) = simplify c' ds where c' = iterate turnCC c !! (round v `div` 90)
simplify c ((Right',   v):ds) = simplify c' ds where c' = iterate turnC c !! (round v `div` 90)
simplify c ((Forward', v):ds) = (c, v) : simplify c ds


parse :: String -> [WeightedShipDirection]
parse = fmap toWeightedShipDirection . lines

toWeightedShipDirection :: String -> WeightedShipDirection
toWeightedShipDirection ('F':xs) = (Forward', read xs)
toWeightedShipDirection ('N':xs) = (North',   read xs)
toWeightedShipDirection ('S':xs) = (South',   read xs)
toWeightedShipDirection ('E':xs) = (East',    read xs)
toWeightedShipDirection ('W':xs) = (West',    read xs)
toWeightedShipDirection ('R':xs) = (Right',   read xs)
toWeightedShipDirection ('L':xs) = (Left',    read xs)


-- >>> p1 [(Forward', 10), (North', 3), (Forward', 7), (Right', 90), (Forward', 11)]
-- 25.0
p1 :: [WeightedShipDirection] -> Double
p1 = manhattenDistance origin . foldl move origin . simplify East

d12p1 :: IO ()
d12p1 = getContents >>= print . p1 . parse


-- Part 2

data NavigationState = NavigationState
    { ship :: Point
    , waypoint :: Point
    } deriving (Show, Eq)

defaultNavigationState :: NavigationState
defaultNavigationState = NavigationState origin (Point {x = 10, y = 1})

moveShip :: NavigationState -> WeightedShipDirection -> NavigationState
moveShip ns@NavigationState {..} (North',   v) = ns {waypoint = wp'} where wp' = waypoint {y = y waypoint + v}
moveShip ns@NavigationState {..} (South',   v) = ns {waypoint = wp'} where wp' = waypoint {y = y waypoint - v}
moveShip ns@NavigationState {..} (East',    v) = ns {waypoint = wp'} where wp' = waypoint {x = x waypoint + v}
moveShip ns@NavigationState {..} (West',    v) = ns {waypoint = wp'} where wp' = waypoint {x = x waypoint - v}
moveShip ns@NavigationState {..} (Left',    v) = ns {waypoint = wp'} where wp' = iterate turnCC' waypoint !! (round v `div` 90)
moveShip ns@NavigationState {..} (Right',   v) = ns {waypoint = wp'} where wp' = iterate turnC' waypoint !! (round v `div` 90)
moveShip ns@NavigationState {..} (Forward', v) = ns {ship = ship'} where ship' = moveInDirection v ship waypoint

-- >>> moveInDirection 10 origin (Point 10 1)
-- Point {x = 100.0, y = 10.0}
-- >>> moveInDirection 7 (Point 100 10) (Point 10 4)
-- Point {x = 170.0, y = 38.0}
moveInDirection :: Double -> Point -> Point -> Point
moveInDirection weight p1 p2 = Point {x = x p1 + weight*dx, y = y p1 + weight*dy}
    where (dx, dy) = direction origin p2

-- >>> p2 [(Forward', 10), (North', 3), (Forward', 7), (Right', 90), (Forward', 11)]
-- 286.0
p2 :: [WeightedShipDirection] -> Double
p2 = manhattenDistance origin . ship . foldl moveShip defaultNavigationState

d12p2 :: IO ()
d12p2 = getContents >>= print . p2 . parse
