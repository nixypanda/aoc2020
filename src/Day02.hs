{-# LANGUAGE RecordWildCards #-}

module Day02 (d2p1, d2p2) where

import Text.Parsec ( anyChar, char, digit, many, parse )
import Text.Parsec.String (Parser)

import Lib.Utils ( xor )

data Rule =
    Occurence { minOcc :: Int, maxOcc :: Int, char' :: Char}
    deriving (Show)


satisfiesRuleMinMax :: Rule -> String -> Bool
satisfiesRuleMinMax Occurence {..} xs = minOcc <= occ && occ <= maxOcc
    where
        occ = length $ filter (== char') xs

-- >>> parse ruleParser "" "1-3 a"
-- Right (Occurence {minOcc = 1, maxOcc = 3, char' = 'a'})
ruleParser :: Parser Rule
ruleParser = do
    minOcc <- read <$> many digit
    _ <- char '-'
    maxOcc <- read <$> many digit
    _ <- many (char ' ')
    c <- anyChar
    return $ Occurence minOcc maxOcc c


lineParser :: Parser (Rule, String)
lineParser = do
    rule <- ruleParser
    _ <- char ':'
    _ <- many (char ' ')
    xs <- many anyChar
    return (rule, xs)


howManySatisfyRule :: (Rule -> String -> Bool) -> [(Rule, String)] -> Int
howManySatisfyRule r = length . filter (== True) . fmap (uncurry r)


driver :: Show b => ([(Rule, String)] -> b) -> IO ()
driver f =
    getContents >>= (print . fmap f . mapM (parse lineParser "") . lines)

d2p1 :: IO ()
d2p1 = driver (howManySatisfyRule satisfiesRuleMinMax)


-- >>> satisfiesRuleIndexMatch (Occurence {minOcc = 1, maxOcc = 3, char' = 'a'}) "abcde"
-- True
-- >>> satisfiesRuleIndexMatch (Occurence {minOcc = 1, maxOcc = 3, char' = 'b'}) "cdefg"
-- False
-- >>> satisfiesRuleIndexMatch (Occurence {minOcc = 2, maxOcc = 9, char' = 'c'}) "cccccccccccccccc"
-- False
satisfiesRuleIndexMatch :: Rule -> String -> Bool
satisfiesRuleIndexMatch Occurence {..} xs =
    (xs !! (minOcc - 1) == char') `xor` (xs !! (maxOcc - 1) == char')


d2p2 :: IO ()
d2p2 = driver (howManySatisfyRule satisfiesRuleIndexMatch)
