module Main where

import Options.Applicative
    ( (<**>)
    , auto
    , fullDesc
    , info
    , long
    , option
    , progDesc
    , short
    , execParser
    , helper
    , Parser
    )

import Day01 ( d1p1, d1p2 )
import Day02 ( d2p1, d2p2 )
import Day03 ( d3p1, d3p2 )
import Day04 ( d4p1, d4p2 )
import Day05 ( d5p1, d5p2 )
import Day06 ( d6p1, d6p2 )
import Day07 ( d7p1, d7p2 )
import Day08 ( d8p1, d8p2 )
import Day09 ( d9p1, d9p2 )
import Day10 ( d10p1, d10p2 )
import Day11 ( d11p1, d11p2 )
import Day12 ( d12p1, d12p2 )
import Day13 ( d13p1, d13p2 )
import Day14 ( d14p1, d14p2 )
import Day15 ( d15p1, d15p2 )
import Day16 ( d16p1, d16p2 )
import Day17 ( d17p1, d17p2 )
import Day18 ( d18p1, d18p2 )
import Day19 ( d19p1, d19p2 )
import Day21 ( d21p1, d21p2 )
import Day22 ( d22p1, d22p2 )
import Day23 ( d23p1, d23p2 )
import Day24 ( d24p1, d24p2 )


data Question = Question
  { day  :: Int
  , part :: Int
  }


questionParser :: Parser Question
questionParser =
    let
        dayOption = option auto ( long "day" <> short 'd')
        partOption = option auto ( long "part" <> short 'p')
    in
        Question <$> dayOption <*> partOption


main :: IO ()
main = solve =<< execParser opts
  where
    opts = info
        (questionParser <**> helper)
        (fullDesc <> progDesc "Solve AoC 2020 invke \\cat | stack run -- -d 1 -p 1")


solve :: Question -> IO ()
solve Question {day = 1, part = 1} = d1p1
solve Question {day = 1, part = 2} = d1p2
solve Question {day = 2, part = 1} = d2p1
solve Question {day = 2, part = 2} = d2p2
solve Question {day = 3, part = 1} = d3p1
solve Question {day = 3, part = 2} = d3p2
solve Question {day = 4, part = 1} = d4p1
solve Question {day = 4, part = 2} = d4p2
solve Question {day = 5, part = 1} = d5p1
solve Question {day = 5, part = 2} = d5p2
solve Question {day = 6, part = 1} = d6p1
solve Question {day = 6, part = 2} = d6p2
solve Question {day = 7, part = 1} = d7p1
solve Question {day = 7, part = 2} = d7p2
solve Question {day = 8, part = 1} = d8p1
solve Question {day = 8, part = 2} = d8p2
solve Question {day = 9, part = 1} = d9p1
solve Question {day = 9, part = 2} = d9p2
solve Question {day = 10, part = 1} = d10p1
solve Question {day = 10, part = 2} = d10p2
solve Question {day = 11, part = 1} = d11p1
solve Question {day = 11, part = 2} = d11p2
solve Question {day = 12, part = 2} = d12p2
solve Question {day = 12, part = 1} = d12p1
solve Question {day = 13, part = 1} = d13p1
solve Question {day = 13, part = 2} = d13p2
solve Question {day = 14, part = 1} = d14p1
solve Question {day = 14, part = 2} = d14p2
solve Question {day = 15, part = 1} = d15p1
solve Question {day = 15, part = 2} = d15p2
solve Question {day = 16, part = 1} = d16p1
solve Question {day = 16, part = 2} = d16p2
solve Question {day = 17, part = 1} = d17p1
solve Question {day = 17, part = 2} = d17p2
solve Question {day = 18, part = 1} = d18p1
solve Question {day = 18, part = 2} = d18p2
solve Question {day = 19, part = 1} = d19p1
solve Question {day = 19, part = 2} = d19p2
solve Question {day = 21, part = 1} = d21p1
solve Question {day = 21, part = 2} = d21p2
solve Question {day = 22, part = 1} = d22p1
solve Question {day = 22, part = 2} = d22p2
solve Question {day = 23, part = 1} = d23p1
solve Question {day = 23, part = 2} = d23p2
solve Question {day = 24, part = 1} = d24p1
solve Question {day = 24, part = 2} = d24p2
