module Lib.ParsingUtils where

import Text.Parsec
import Text.Parsec.String

-- >>> parse ws "" ""
-- Right "    "
maybews :: Parser String
maybews = many (char ' ')

ws :: Parser String
ws = many1 (char ' ')

-- >>> parse word "" "myman "
-- Right "myman"
word :: Parser String
word = many letter

-- >>> parse ssvWords "" "myman  myman   l "
-- Right ["myman","myman","l",""]
ssvWords :: Parser [String]
ssvWords = word `sepBy1` ws

symbol :: String -> Parser String
symbol s = string s <* maybews

-- >>> parse csvWords "" "myman, myman"
-- Right ["myman","myman"]
csvWords :: Parser [String]
csvWords = word `sepBy` (symbol "," <* maybews)


number :: Parser Integer
number = read <$> many digit

