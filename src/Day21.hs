{-# LANGUAGE RecordWildCards #-}
module Day21 (d21p1, d21p2) where

import Data.List ( (\\), intercalate, intersect, union )
import Text.Parsec ( string, between, parse )
import Text.Parsec.String ( Parser )

import Lib.ParsingUtils ( ssvWords, csvWords, symbol, maybews )
import Lib.ListUtils (scrub)
import Lib.Utils (converge)

import qualified Data.Map as M


-- Parsing

type Ingredient = String
type Allergen = String
data Food = Food { ingredients :: [Ingredient], allergens :: [Allergen]}
    deriving (Show, Eq)

-- >>> parse food "" "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
-- Right (Food {ingredients = ["mxmxvkd","kfcds","sqjhc","nhms"], allergens = ["dairy","fish"]})
food :: Parser Food
food = do
    ingredients <- ssvWords
    allergens <- between (symbol "(") (symbol")") (string "contains" *> maybews *> csvWords)
    return $ Food (init ingredients) allergens

-- >>> parse' _test
-- [Food {ingredients = ["mxmxvkd","kfcds","sqjhc","nhms"], allergens = ["dairy","fish"]},Food {ingredients = ["trh","fvjkl","sbzzf","mxmxvkd"], allergens = ["dairy"]},Food {ingredients = ["sqjhc","fvjkl"], allergens = ["soy"]},Food {ingredients = ["sqjhc","mxmxvkd","sbzzf"], allergens = ["fish"]}]
parse' :: String -> [Food]
parse' = (\(Right r) -> r) . mapM (parse food "") . lines


-- Part 1

-- >>> allergenMap $ parse' _test
-- fromList [("dairy",["mxmxvkd"]),("fish",["sqjhc"]),("soy",["fvjkl"])]
allergenMap :: [Food] -> M.Map Allergen [Ingredient]
allergenMap foods = M.fromList . zip (M.keys m) . converge scrub $ M.elems m
    where m = M.fromListWith intersect $ [(a, is) | Food {ingredients = is, allergens = as} <- foods, a <- as]

-- >>> p1 $ parse' _test
-- 5
p1 foods = length . filter (`elem` ingredientsWithoutAllergens) $ concatMap ingredients foods
    where
        ingredientsWithoutAllergens = allIngrediants foods \\ allergensContainingIngrediants
        allergensContainingIngrediants = concat . M.elems $ allergenMap foods
        allIngrediants = foldr1 union . fmap ingredients

d21p1 :: IO ()
d21p1 = getContents >>= print . p1 . parse'


-- Part 2

p2 :: [Food] -> [Char]
p2 = intercalate "," . concat . M.elems . allergenMap

d21p2 :: IO ()
d21p2 = getContents >>= print . p2 . parse'


-- Test data

_test = unlines
    ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
    ,"trh fvjkl sbzzf mxmxvkd (contains dairy)"
    ,"sqjhc fvjkl (contains soy)"
    ,"sqjhc mxmxvkd sbzzf (contains fish)"
    ]
