{-# LANGUAGE FlexibleContexts #-}
module Day19 (d19p1, d19p2) where

import Prelude hiding (and, or)

import Text.Parsec
    ( char
    , digit
    , letter
    , string
    , between
    , many1
    , sepBy
    , (<|>)
    , many
    , parse
    )
import Text.Parsec.String ( Parser )
import Data.List.Split ( splitOn )
import Control.Monad ( foldM )

import qualified Data.Map as M
import Data.Map ((!), Map)


data Rule = And [Int] | Or [Rule] | Terminal Char
    deriving (Show, Eq)

symbol :: String -> Parser String
symbol s = string s <* ws

ws = many (char ' ')

-- >>> parse rule "" "1: 2 3 | 3       2 "
-- Right (1,Or [And [2,3],And [3,2]])
-- >>> parse rule "" "4: \"a\""
-- Right (4,Or [Terminal 'a'])
rule :: Parser (Int, Rule)
rule = (,) <$> (num <* symbol ":") <*> or
    where
        num = read <$> (ws *> many1 digit <* ws)
        terminal = Terminal <$> between (symbol "\"") (symbol "\"") letter
        and = terminal <|> (And <$> num `sepBy` ws)
        or = Or <$> and `sepBy` symbol "|"


parse' :: [Char] -> (Map Int Rule, [String])
parse' = (\[rules, d] -> (M.fromList $ parseRules rules, parseData d)) . splitOn "\n\n"
    where
        parseData = lines
        parseRules = (\(Right r) -> r) . mapM (parse rule "") . lines


-- Part 1

-- >>>  (\(rules, input) -> eval rules (rules ! 0) (input !! 3)) _test1
-- []
-- >>>  (\(rules, input) -> eval rules (rules ! 0) (input !! 0)) _test1
-- [""]
-- >>>  (\(rules, input) -> eval rules (rules ! 0) (input !! 1)) _test1
-- []
-- >>>  (\(rules, input) -> eval rules (rules ! 0) (input !! 4)) _test1
-- ["b"]
eval :: Map Int Rule -> Rule -> String -> [String]
eval _     (Terminal _) []        = []
eval _     (Terminal c) (c':rest) = [rest | c == c']
eval rules (And ns) s             = foldM (\s n -> eval rules (rules ! n) s) s ns
eval rules (Or  rs) s             = concat [ eval rules r s | r <- rs ]


countMatches :: (Map Int Rule, [String]) -> Int
countMatches (rules, inputs) = length
    [ input
    | input <- inputs
    , let matches = eval rules (rules ! 0) input
    , "" `elem` matches
    ]

d19p1 :: IO ()
d19p1 = getContents >>= print . countMatches . parse'


-- Part 2

p2 :: (Map Int Rule, [String]) -> Int
p2 (rules, inputs) = countMatches
  ( M.insert 8 (Or [And [42], And [42, 8]]) $ M.insert 11 (Or [And [42, 31], And [42, 11, 31]]) rules
  , inputs
  )

d19p2 :: IO ()
d19p2 = getContents >>= print . p2 . parse'


_test1 = parse' . unlines $
    [ "0: 4 1 5"
    , "1: 2 3 | 3 2"
    , "2: 4 4 | 5 5"
    , "3: 4 5 | 5 4"
    , "4: \"a\""
    , "5: \"b\""
    , ""
    , "ababbb"
    , "bababa"
    , "abbbab"
    , "aaabbb"
    , "aaaabbb"
    ]
