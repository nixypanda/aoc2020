{-# LANGUAGE RecordWildCards #-}
module Day07 (d7p1, d7p2) where

import Text.Parsec (many, (<|>), parse, sepBy, optional, ParseError)
import Text.Parsec.Char (letter, char, string, digit)
import Text.Parsec.String (Parser)
import Data.Functor (($>))
import Data.Map ( fromList, Map, (!) )
import Control.Arrow ( Arrow((&&&)) )


-- Parsing

type Color = String

data Bag = Bag {color :: Color, children :: [(Int, Color)]}
    deriving (Show, Eq)


word :: Parser String
word = many letter


bagColor :: Parser String
bagColor = do
    _ <- optional (string " ")
    c1 <- word
    c2 <- string " "
    c3 <- word
    _ <- string " bag"
    _ <- optional (string "s")
    pure $ concat [c1, c2, c3]

-- >>> parse bag "" "muted white bags contain 4 dark orange bags, 3 bright white bags."
-- Right (Bag {color = "muted white", children = [(4,"dark orange"),(3,"bright white")]})
-- >>> parse bag "" "muted white bags contain no other bags."
-- Right (Bag {color = "muted white", children = []})
bag :: Parser Bag
bag = do
    color <- bagColor 
    _ <- string " contain "
    let
        nBagColor = (,) <$> fmap read (many digit) <*> bagColor
        noBags = string "no other bags" $> []
    children <- noBags <|> nBagColor `sepBy` string ", "
    _ <- char '.'
    return $ Bag color children


-- >>> bagMap allBags'
bagMap :: [Bag] -> Map Color [(Int, Color)]
bagMap xs = fromList [(k, v) | Bag k v <- xs]


data Packing = Packing { color' :: Color, inside :: [(Int, Packing)]}
    deriving (Show, Eq)


toPacking :: Map Color [(Int, Color)] -> Color -> Packing
toPacking bagMap color = Packing
    { color' = color
    , inside = [(k, toPacking bagMap v) | (k, v) <- bagMap ! color]
    }

-- >>> (uncurry allPacking . (id &&& bagMap) $ allBags') == allPackings'
-- True
allPacking :: [Bag] -> Map Color [(Int, Color)] -> [Packing]
allPacking bags m = toPacking m <$> fmap color bags


-- >>> parse' bagData
parse' :: String -> Either ParseError [Packing]
parse' = fmap (uncurry allPacking . (id &&& bagMap)) . mapM (parse bag "") . lines


-- Part 1

-- >>> fmap (howManyWaysToPack "shiny gold") allPackings'
-- [2,2,1,1,1,0,0,0,0]
howManyWaysToPack :: Color -> Packing -> Int
howManyWaysToPack color Packing {..}
    | color == color' = 1 + rest
    | otherwise = rest
    where rest = sum $ fmap (howManyWaysToPack color . snd) inside


p1 :: [Packing] -> Int
p1 = length . filter (> 0) . fmap (howManyWaysToPack "shiny gold")


d7p1 :: IO ()
d7p1 = getContents >>= print . fmap p1 . parse'


-- Part 2

-- >>> kitneBagChiye Packing {color' = "shiny gold", inside = [(1,Packing {color' = "dark olive", inside = [(3,Packing {color' = "faded blue", inside = []}),(4,Packing {color' = "dotted black", inside = []})]}),(2,Packing {color' = "vibrant plum", inside = [(5,Packing {color' = "faded blue", inside = []}),(6,Packing {color' = "dotted black", inside = []})]})]}
-- 32
kitneBagChiye :: Packing -> Int
kitneBagChiye Packing {..} = sum [n * (1 + kitneBagChiye v) | (n, v) <- inside]


-- >>> p2 allPackings'
-- 32
p2 :: [Packing] -> Int
p2 = kitneBagChiye . head . filter ((== "shiny gold") . color')


d7p2 :: IO ()
d7p2 = getContents >>= print . fmap p2 . parse'


-- Test Data

allBags' = (\(Right a) -> a) . mapM (parse bag "") . lines $ bagData
allPackings' = (\(Right a) -> a) $ parse' bagData
bagData = unlines
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
    , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
    , "bright white bags contain 1 shiny gold bag."
    , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
    , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
    , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
    , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
    , "faded blue bags contain no other bags."
    , "dotted black bags contain no other bags."
    ]
