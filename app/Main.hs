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
solve = undefined

