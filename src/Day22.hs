module Day22 (d22p1, d22p2) where

import Data.List.Split
import Lib.Utils

import qualified Data.Set as S


type Deck = [Int]

data SpaceCards = SpaceCards
    { p1Deck :: Deck
    , p2Deck :: Deck
    , history :: S.Set (Deck, Deck)
    } deriving (Show, Eq)

mkGame :: Deck -> Deck -> SpaceCards
mkGame xs ys = SpaceCards xs ys S.empty

winningDeck :: SpaceCards -> Either String Deck
winningDeck (SpaceCards [] [] _) = Left "Impossible state"
winningDeck (SpaceCards [] ys _) = Right ys
winningDeck (SpaceCards xs [] _) = Right xs
winningDeck (SpaceCards xs _  _) = Right xs


-- Parsing

parse :: [Char] -> SpaceCards
parse = (\[p1, p2] -> mkGame (toDeck p1) (toDeck p2)) . splitOn "\n\n"
    where toDeck = fmap read . tail . lines

p1Won, p2Won :: SpaceCards -> SpaceCards
p1Won (SpaceCards xs'@(x:xs) ys'@(y:ys) s) = SpaceCards (xs ++ [x, y]) ys (S.insert (xs', ys') s)
p2Won (SpaceCards xs'@(x:xs) ys'@(y:ys) s) = SpaceCards xs (ys ++ [y, x]) (S.insert (xs', ys') s)

-- Part 1

playRound :: SpaceCards -> SpaceCards
playRound sc@(SpaceCards (x:_) (y:_) _)
    | x > y = p1Won sc
    | x < y = p2Won sc
playRound sc = sc


deckScore :: Deck -> Int
deckScore = sum . zipWith (*) [1..] . reverse 

-- >>> p1 _test
-- Right 306
p1 :: SpaceCards -> Either String Int
p1 = fmap deckScore . winningDeck . converge playRound

d22p1 :: IO ()
d22p1 = getContents >>= print . p1 . parse 


-- Part 2

-- >>> _disp . (!! 17) $ iterate playRound' _test
-- ([],[7,5,6,2,4,1,10,8,9,3])
-- >>> _disp . (!! 6) $ iterate playRound' (mkGame [9,8,5,2] [10,1,7])
-- ([],[5,10,2,9,8,7,1])
-- >>> _disp . (!! 10) $ iterate playRound' _testInf
-- ([43,19],[2,29,14])
playRound' :: SpaceCards -> SpaceCards
playRound' sc@(SpaceCards xs'@(x:xs) ys'@(y:ys) s)
    | S.member (xs', ys') s = sc
    | x <= length xs && y <= length ys = recurse (mkGame (take x xs) (take y ys))
    | x > y = p1Won sc
    | x < y = p2Won sc
    | otherwise = error "WTF"
    where
        recurse sc' =
            case converge playRound' sc' of
                SpaceCards [] [] _ -> error "Unexpected shit"
                SpaceCards _ [] _ -> p1Won sc
                SpaceCards [] _ _ -> p2Won sc
                _ -> p1Won sc
playRound' sc = sc


-- >>> p2 _test
-- Right 291
p2 :: SpaceCards -> Either String Int
p2 = fmap deckScore . winningDeck . converge playRound'

d22p2 :: IO ()
d22p2 = getContents >>= print . p2 . parse 


-- Testing

_disp :: SpaceCards -> (Deck, Deck)
_disp (SpaceCards xs ys _) = (xs, ys)

_test = parse . unlines $
    [ "Player 1:"
    , "9"
    , "2"
    , "6"
    , "3"
    , "1"
    , ""
    , "Player 2:"
    , "5"
    , "8"
    , "4"
    , "7"
    , "10"
    ]


_testInf :: SpaceCards
_testInf = parse . unlines $
    [ "Player 1:"
    , "43"
    , "19"
    , ""
    , "Player 2:"
    , "2"
    , "29"
    , "14"
    ]
