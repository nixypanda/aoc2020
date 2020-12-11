module Day11 (d11p1, d11p2) where


import Lib.Grid
    ( Grid
    , Location
    , iiMap
    , converge
    , layoutAround
    , fromLists
    , toLists
    , howMany
    )


data Seat = Occupied | Empty | Void deriving (Show, Eq)
type Layout = Grid Seat

fromChar :: Char -> Seat
fromChar '.' = Void
fromChar 'L' = Empty
fromChar '#' = Occupied

_toChar :: Seat -> Char
_toChar Void = '.'
_toChar Empty = 'L'
_toChar Occupied = '#'

parse :: String -> Layout
parse = fromLists . fmap (fmap fromChar) . lines

_showSeats :: Layout -> String
_showSeats = unlines . fmap (fmap _toChar) . toLists


-- p1

-- >>> getNeighbours (0,0) $ parse _l1
-- [Void,Empty,Empty]
-- >>> getNeighbours (1,1) $ runSim $ parse _l1
-- [Occupied,Occupied,Void,Occupied,Occupied,Occupied,Void,Occupied]
getNeighbours :: Location -> Grid a -> [a]
getNeighbours l = concatMap (take 1) . layoutAround l

-- >>> whatToDoWithSeat Occupied [Occupied,Void,Occupied,Occupied,Occupied,Occupied,Void,Occupied]
-- Empty
whatToDoWithSeat :: Seat -> [Seat] -> Seat
whatToDoWithSeat seat neighbours
  | seat == Empty && notElem Occupied neighbours = Occupied
  | seat == Occupied && (length (filter (== Occupied) neighbours) >= 4) = Empty
  | otherwise = seat


-- >>> _l1Final == (_showSeats . runSim . runSim . runSim . runSim . runSim . parse $ _l1)
-- True
-- >>> _l1First == (_showSeats . runSim . parse $ _l1)
-- True
-- >>> _l1Second == (_showSeats . runSim . runSim . parse $ _l1)
-- True
-- >>> _showSeats . runSim . runSim . parse $ _l1
-- "#.LL.L#.##\n#LLLLLL.L#\nL.L.L..L..\n#LLL.LL.L#\n#.LL.LL.LL\n#.LLLL#.##\n..L.L.....\n#LLLLLLLL#\n#.LLLLLL.L\n#.#LLLL.##\n"
runSim :: Layout -> Layout
runSim layout = iiMap (\r c s -> whatToDoWithSeat s $ getNeighbours (r, c) layout) layout


-- >>> p1 $ parse _l1
-- 37
p :: (Grid Seat -> Grid Seat) -> Grid Seat -> Int
p runSim = howMany (== Occupied) . converge runSim


d11p1 :: IO ()
d11p1 = getContents >>= print . p runSim . parse


-- part 2 

-- >>> occupiedInVision (0,0) $ parse _l1
-- [Empty,Empty,Empty]
-- >>> occupiedInVision (1,1) $ runSim $ parse _l1
-- [Occupied,Occupied,Occupied,Occupied,Occupied,Occupied,Occupied]
neighboursInVision :: Location -> Grid Seat -> [Seat]
neighboursInVision l = concatMap (take 1 . dropWhile (== Void)) . layoutAround l


-- >>> whatToDoWithSeat Occupied [Occupied,Void,Occupied,Occupied,Occupied,Occupied,Void,Occupied]
-- Empty
whatToDoWithSeat' :: Seat -> [Seat] -> Seat
whatToDoWithSeat' seat neighbours
  | seat == Empty && notElem Occupied neighbours = Occupied
  | seat == Occupied && (length (filter (== Occupied) neighbours) >= 5) = Empty
  | otherwise = seat


-- >>> _showSeats . runSim' . runSim' . runSim' . parse $ _l1
-- "#.L#.##.L#\n#L#####.LL\nL.#.#..#..\n##L#.##.##\n#.##.#L.##\n#.#####.#L\n..#.#.....\nLLL####LL#\n#.L#####.L\n#.L####.L#\n"
runSim' :: Layout -> Layout
runSim' layout = iiMap (\r c s -> whatToDoWithSeat' s $ neighboursInVision (r, c) layout) layout


d11p2 :: IO ()
d11p2 = getContents >>= print . p runSim' . parse


-- test Data

_l1 = unlines
    [ "L.LL.LL.LL"
    , "LLLLLLL.LL"
    , "L.L.L..L.."
    , "LLLL.LL.LL"
    , "L.LL.LL.LL"
    , "L.LLLLL.LL"
    , "..L.L....."
    , "LLLLLLLLLL"
    , "L.LLLLLL.L"
    , "L.LLLLL.LL"
    ]

_l1First = unlines
    [ "#.##.##.##"
    , "#######.##"
    , "#.#.#..#.."
    , "####.##.##"
    , "#.##.##.##"
    , "#.#####.##"
    , "..#.#....."
    , "##########"
    , "#.######.#"
    , "#.#####.##"
    ]

_l1Second = unlines
    [ "#.LL.L#.##"
    , "#LLLLLL.L#"
    , "L.L.L..L.."
    , "#LLL.LL.L#"
    , "#.LL.LL.LL"
    , "#.LLLL#.##"
    , "..L.L....."
    , "#LLLLLLLL#"
    , "#.LLLLLL.L"
    , "#.#LLLL.##"
    ]

_l1Final = unlines
    [ "#.#L.L#.##"
    , "#LLL#LL.L#"
    , "L.#.L..#.."
    , "#L##.##.L#"
    , "#.#L.LL.LL"
    , "#.#L#L#.##"
    , "..L.L....."
    , "#L#L##L#L#"
    , "#.LLLLLL.L"
    , "#.#L#L#.##"
    ]
