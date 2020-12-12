{-# LANGUAGE RecordWildCards #-}
module Lib.Geometry where


data Point = Point {x :: Double, y :: Double}
    deriving (Show, Eq)

data CompassDirection = North | East | South | West
    deriving (Show, Eq)

type WeightedCompassDirection = (CompassDirection, Double)

-- >>> move (0, 0) (North, 10)
-- (10,0)
move :: Point -> WeightedCompassDirection -> Point
move p@Point {..} (North, w) = p {y = y + w}
move p@Point {..} (East,  w) = p {x = x + w}
move p@Point {..} (South, w) = p {y = y - w}
move p@Point {..} (West,  w) = p {x = x - w}


direction :: Point -> Point -> (Double, Double)
direction Point {x = x1, y = y1} Point {x = x2, y = y2} =
    (x2 - x1, y2 - y1)

manhattenDistance :: Point -> Point -> Double
manhattenDistance Point {x = x1, y = y1} Point {x = x2, y = y2} =
    abs (x2 - x1) + abs (y2 - y1)


turnC :: CompassDirection -> CompassDirection
turnC North = East
turnC East  = South
turnC South = West
turnC West  = North

turnCC :: CompassDirection -> CompassDirection
turnCC North = West
turnCC East  = North
turnCC South = East
turnCC West  = South


origin :: Point
origin = Point 0 0 

-- >>> turnC' (Point 10 4)
-- Point {x = 4.0, y = -10.0}
turnC' :: Point -> Point
turnC' Point {..} = Point {x = y, y = -x}

-- >>> turnCC' (Point 10 4)
-- Point {x = -4.0, y = 10.0}
-- >>> turnCC' $ turnCC' (Point 10 4)
-- Point {x = -10.0, y = -4.0}
turnCC' :: Point -> Point
turnCC' Point {..} = Point {x = -y, y = x}

