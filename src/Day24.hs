module Day24 (d24p1, d24p2) where

import Text.Parsec ( string, many1, (<|>), parse, try )
import Text.Parsec.String ( Parser )

import qualified Data.Set as S

-- Parsing

fromRight :: Either a p -> p
fromRight (Right r) = r
fromRight _ = error "No Left allowed"

type Position = (Int, Int)

data Direction = E | SE | SW | W | NW | NE
    deriving (Eq, Show, Enum)

type Path = [Direction]

parseE, parseSE, parseSW, parseW, parseNW, parseNE :: Parser Direction
parseE  = E  <$ string "e"
parseSE = SE <$ string "se"
parseSW = SW <$ string "sw"
parseW  = W  <$ string "w"
parseNW = NW <$ string "nw"
parseNE = NE <$ string "ne"

-- >>> parse parseDirections "" "esenee"
-- Variable not in scope: parseDirections :: Parsec [Char] () a
path :: Parser Path
path = many1 (try parseE <|> try parseSE <|> try parseSW <|> try parseW <|> try parseNW <|> parseNE)

-- >>> parse' _test
parse' :: String -> [Path]
parse' = fromRight . mapM (parse path "") . lines


-- Part 1

addVector :: Position -> Position -> Position
addVector (x, y) (a, b) = (x+a, y+b)

directionToPositionDelta :: Direction -> Position
directionToPositionDelta NE = ( 0,  1)
directionToPositionDelta SW = ( 0, -1)
directionToPositionDelta E  = ( 1,  0)
directionToPositionDelta W  = (-1,  0)
directionToPositionDelta NW = (-1,  1)
directionToPositionDelta SE = ( 1, -1)

followPathFrom :: Position -> Path -> Position
followPathFrom origin = foldl addVector origin . fmap directionToPositionDelta

exclusiveInsert :: Ord a => a -> S.Set a -> S.Set a
exclusiveInsert k s = if S.member k s then S.delete k s else S.insert k s

mkFloor :: [Path] -> FloorArt
mkFloor = foldr exclusiveInsert S.empty . fmap (followPathFrom (0, 0))

-- >>> p1 $ parse' _test
-- 10
p1 :: [Path] -> Int
p1 = S.size . mkFloor

d24p1 :: IO ()
d24p1 = getContents >>= print . p1 . parse'


-- Part 2

type FloorArt = S.Set Position
data Tile = White | Black deriving (Eq, Show)

flipTile :: Tile -> [Tile] -> Tile
flipTile tile neighbours
    | tile == Black = if n == 0 || n > 2 then White else Black
    | tile == White = if n == 2 then Black else White
    where n = length $ filter (== Black) neighbours

-- >>> neighboursOf (1,1)
-- [(2,1),(2,0),(1,0),(0,1),(0,2),(1,2)]
neighboursOf :: Position -> [Position]
neighboursOf o = fmap (addVector o . directionToPositionDelta) [E .. NE]

tileColor :: FloorArt -> Position -> Tile
tileColor s pos = if S.member pos s then Black else White

-- >>> length $ step $  mkFloor $ parse' _test
-- 15
step :: FloorArt -> FloorArt
step s = S.filter ((== Black) . flipT) $ S.foldr S.union s (S.map (S.fromList . neighboursOf) s)
    where flipT e = flipTile (tileColor s e) (tileColor s <$> neighboursOf e)

-- >>> p2 $ parse' _test
-- 2208
p2 :: [Path] -> Int
p2 = S.size . (!! 100) . iterate step . mkFloor

d24p2 :: IO ()
d24p2 = getContents >>= print . p2 . parse'


_test :: String
_test = unlines
    [ "sesenwnenenewseeswwswswwnenewsewsw"
    , "neeenesenwnwwswnenewnwwsewnenwseswesw"
    , "seswneswswsenwwnwse"
    , "nwnwneseeswswnenewneswwnewseswneseene"
    , "swweswneswnenwsewnwneneseenw"
    , "eesenwseswswnenwswnwnwsewwnwsene"
    , "sewnenenenesenwsewnenwwwse"
    , "wenwwweseeeweswwwnwwe"
    , "wsweesenenewnwwnwsenewsenwwsesesenwne"
    , "neeswseenwwswnwswswnw"
    , "nenwswwsewswnenenewsenwsenwnesesenew"
    , "enewnwewneswsewnwswenweswnenwsenwsw"
    , "sweneswneswneneenwnewenewwneswswnese"
    , "swwesenesewenwneswnwwneseswwne"
    , "enesenwswwswneneswsenwnewswseenwsese"
    , "wnwnesenesenenwwnenwsewesewsesesew"
    , "nenewswnwewswnenesenwnesewesw"
    , "eneswnwswnwsenenwnwnwwseeswneewsenese"
    , "neswnwewnwnwseenwseesewsenwsweewe"
    , "wseweeenwnesenwwwswnew"
    ]
