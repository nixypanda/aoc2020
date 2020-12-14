{-# LANGUAGE RecordWildCards #-}
module Day14 (d14p1, d14p2) where

import Data.Bits ( Bits(clearBit, setBit) )
import Data.Map (Map, insert, empty, elems)
import Text.Parsec (many, (<|>), parse, ParseError, try)
import Text.Parsec.Char (string, digit, alphaNum)
import Text.Parsec.String (Parser)
import Data.List (sort)

-- Parsing

type BitMask = [Maybe Bool]

-- >>> parse' "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0"
-- Right [SetMask [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just True,Nothing,Nothing,Nothing,Nothing,Just False,Nothing],SetValue 8 11,SetValue 7 101,SetValue 8 0]
parse' :: String -> Either ParseError [DockingCommand]
parse' = mapM (parse (try mask <|> setVal) "") . lines
    where
        mask :: Parser DockingCommand
        mask = SetMask <$> (string "mask = " *> fmap stringToMask (many alphaNum))

        setVal :: Parser DockingCommand
        setVal = SetValue
            <$> (string "mem[" *> fmap read (many digit))
            <*> (string "] = " *> fmap read (many digit))

stringToMask :: String -> BitMask
stringToMask = fmap f
    where
        f 'X' = Nothing
        f '1' = Just True
        f '0' = Just False


-- Part 1

-- >>> applyBitMask 11 _testMask 
-- 73
-- >>> applyBitMask 101 _testMask
-- 101
applyBitMask :: Int -> BitMask -> Int
applyBitMask i = foldr f i . zip [0..] . reverse
    where
        f :: (Int, Maybe Bool) -> Int -> Int
        f (_, Nothing) int = int
        f (i, Just True) int = int `setBit` i
        f (i, Just False) int = int `clearBit` i


data DockingCommand = SetMask BitMask | SetValue Int Int
      deriving (Show)

data DockingState = DockingState
    { store :: Map Int Int
    , currentMask :: BitMask
    } deriving (Show)


defaultDock :: DockingState
defaultDock = DockingState empty (stringToMask "X")

-- >>> store $ foldl applyCommand defaultDock [SetMask _testMask, SetValue 8 11, SetValue 7 101, SetValue 8 0]
-- fromList [(7,101),(8,64)]
applyCommand :: DockingState -> DockingCommand -> DockingState
applyCommand ds (SetMask mask) = ds {currentMask = mask}
applyCommand ds@DockingState {..} (SetValue key value) =
    ds {store = insert key (applyBitMask value currentMask) store}

-- >>> p1 [SetMask _testMask, SetValue 8 11, SetValue 7 101, SetValue 8 0]
-- 165
p1 :: [DockingCommand] -> Int
p1 = sum . elems . store . foldl applyCommand defaultDock

d14p1 :: IO ()
d14p1 = getContents >>= print . fmap p1 . parse'


-- part 2


-- >>> applyBitMask' 42 _testMask2
-- [26,27,58,59]
-- >>> applyBitMask' 26 _testMask1
-- [16,17,18,19,24,25,26,27]
applyBitMask' :: Int -> BitMask -> [Int]
applyBitMask' i = sort . foldr f [i] . zip [0..] . reverse
    where
        f :: (Int, Maybe Bool) -> [Int] -> [Int]
        f (i, Nothing) ints = fmap (`setBit` i) ints ++ fmap (`clearBit` i) ints
        f (i, Just True) ints = fmap (`setBit` i) ints
        f (_, Just False) ints = ints

applyCommand' :: DockingState -> DockingCommand -> DockingState
applyCommand' ds (SetMask mask) = ds {currentMask = mask}
applyCommand' ds@DockingState {..} (SetValue key value) =
    ds {store = foldr (`insert` value) store (applyBitMask' key currentMask)}

-- >>> p2 [SetMask _testMask2, SetValue 42 100, SetMask _testMask1, SetValue 26 1]
-- 208
p2 :: [DockingCommand] -> Int
p2 = sum . elems . store . foldl applyCommand' defaultDock

d14p2 :: IO ()
d14p2 = getContents >>= print . fmap p2 . parse'


-- test data

_testMask :: BitMask
_testMask = stringToMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"

_testMask1 :: BitMask
_testMask1 = stringToMask "00000000000000000000000000000000X0XX"

_testMask2 :: BitMask
_testMask2 = stringToMask "000000000000000000000000000000X1001X"
