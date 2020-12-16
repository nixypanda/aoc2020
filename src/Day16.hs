{-# LANGUAGE RecordWildCards #-}
module Day16 (d16p1, d16p2) where

import Text.Parsec ( char, letter, string, sepBy, (<|>), many, parse )
import Text.Parsec.String ( Parser )
import Data.List.Split (splitOn)
import Data.List ( transpose, isPrefixOf, intersect )
import Lib.Range ( Range(Range), inRangeInc )
import Lib.Utils ( number, converge )
import Lib.ListUtils ( scrub )


data Rule = Rule { name ::  String, ranges :: [Range] } deriving (Show, Eq)
type Ticket = [Int]
type AnotatedTicket = [(String, Int)]


-- Parsing

-- >>> parse range "" "1-2"
-- Right (Range {lower = 1, upper = 2})
range :: Parser Range
range = Range <$> number <*> (string "-" *> number)

-- >>> parse rule "" "class: 1-3 or 5-7"
-- Right (Rule {name = "class", ranges = [Range {lower = 1, upper = 3},Range {lower = 5, upper = 7}]})
rule :: Parser Rule
rule = Rule <$> (many (letter <|> char ' ') <* string ": ") <*> (range `sepBy` string " or ")

parse' :: String -> ([Rule], Ticket, [Ticket])
parse' = (\[rs, t, ot] -> (parseRules rs, parseMyTicket t, parseOtherTickets ot)) . splitOn "\n\n"
    where
        parseRules = (\(Right r) -> r) . mapM (parse rule "") . lines
        parseMyTicket = fmap read . splitOn "," . last . lines
        parseOtherTickets = fmap (fmap read . splitOn ",") . tail . lines


-- Part 1

satisfiesRule :: Int -> Rule -> Bool
satisfiesRule val Rule{..} = or $ fmap (val `inRangeInc`) ranges

-- >>> satisfiesAnyRule 4 _rules
-- False
satisfiesAnyRule :: Int -> [Rule] -> Bool
satisfiesAnyRule val = or . fmap (val `satisfiesRule`)

-- >>> p1 _test
-- 71
p1 :: ([Rule], Ticket, [Ticket]) -> Int
p1 (rules, _, nts) = sum $ filter (not . (`satisfiesAnyRule` rules)) allTickets
    where allTickets = concat nts

d16p1 :: IO ()
d16p1 = getContents >>= print . p1 . parse'


-- Part 2

-- >>> isTicketValid _rules [7,3,47]
-- True
-- >>> isTicketValid _rules [40,4,50]
-- False
isTicketValid :: [Rule] -> Ticket -> Bool
isTicketValid rules = and . fmap (`satisfiesAnyRule` rules)

satisfiedRules :: Int -> [Rule] -> [Rule]
satisfiedRules r = filter (r `satisfiesRule`)

determineRanges :: [Rule] -> [[Int]] -> [Rule]
determineRanges rules =
    fmap head . converge scrub . fmap (foldr1 intersect . fmap (`satisfiedRules` rules)) . transpose

annotateTicket :: Ticket -> [Rule] -> AnotatedTicket
annotateTicket t rules = zip (fmap name rules) t

-- >>> p2 _test2
-- 1
p2 :: ([Rule], Ticket, [Ticket]) -> Int
p2 (rules, mt, ots) =
    product . departureFields . annotateTicket mt . determineRanges rules $ filter (isTicketValid rules) ots
        where departureFields = fmap snd . filter (("departure" `isPrefixOf`) . fst)

d16p2 :: IO ()
d16p2 = getContents >>= print . p2 . parse'


-- Test Data

_rules :: [Rule]
_rules = (\(rules, _, _) -> rules) _test

-- >>> _test
_test :: ([Rule], Ticket, [Ticket])
_test = parse' $ unlines
    [ "class: 1-3 or 5-7"
    , "row: 6-11 or 33-44"
    , "seat: 13-40 or 45-50"
    , ""
    , "your ticket:"
    , "7,1,14"
    , ""
    , "nearby tickets:"
    , "7,3,47"
    , "40,4,50"
    , "55,2,20"
    , "38,6,12"
    ]

_test2 :: ([Rule], Ticket, [Ticket])
_test2 = parse' $ unlines
    [ "class: 0-1 or 4-19"
    , "row: 0-5 or 8-19"
    , "seat: 0-13 or 16-19"
    , ""
    , "your ticket:"
    , "11,12,13"
    , ""
    , "nearby tickets:"
    , "3,9,18"
    , "15,1,5"
    , "5,14,9"
    ]
