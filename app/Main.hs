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
solve (Question {day = 1, part = 1}) = d1p1
solve (Question {day = 1, part = 2}) = d1p2
solve (Question {day = 2, part = 1}) = d2p1
solve (Question {day = 2, part = 2}) = d2p2
