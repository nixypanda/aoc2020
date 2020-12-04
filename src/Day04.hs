module Day04 (d4p1, d4p2) where

import Data.List.Split ( splitOn )
import Data.Char (isHexDigit, isDigit)

import Lib.Utils ( between, all' )
import Lib.ListUtils ( isSubListOf, howMany, tuplify )


-- Parsing

parse :: String -> [Passport]
parse = fmap (fmap (tuplify . splitOn ":") . concatMap words . lines) . splitOn "\n\n"


-- Part 1

type Passport = [(String, String)]

validFields :: [String]
validFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

containsRequiredFields :: Passport -> Bool
containsRequiredFields = (validFields `isSubListOf`) . fmap fst


p1 :: [Passport] -> Int
p1 = howMany (== True) . fmap containsRequiredFields

d4p1 :: IO ()
d4p1 = getContents >>= print . p1 . parse


-- Part 2

-- >>> isFieldValid "byr" "2002"
-- True
-- >>> isFieldValid "byr" "2003"
-- False
-- >>> isFieldValid "hgt" "60in"
-- True
-- >>> isFieldValid "hgt" "190cm"
-- True
-- >>> isFieldValid "hgt" "190in"
-- False
-- >>> isFieldValid "hgt" "190"
-- False
-- >>> isFieldValid "hcl" "#123abc"
-- True
-- >>> isFieldValid "hcl" "#123abz"
-- False
-- >>> isFieldValid "hcl" "123abc"
-- False
-- >>> isFieldValid "ecl" "brn"
-- True
-- >>> isFieldValid "ecl" "wat"
-- False
-- >>> isFieldValid "pid" "000000001"
-- True
-- >>> isFieldValid "pid" "0123456789"
-- False
isFieldValid :: String ->  String -> Bool
isFieldValid "byr" xs = all' isDigit xs && between 1920 2002 (read xs)
isFieldValid "iyr" xs = all' isDigit xs && between 2010 2020 (read xs)
isFieldValid "eyr" xs = all' isDigit xs && between 2020 2030 (read xs)
isFieldValid "hcl" ('#':xs) = all' isHexDigit xs && length xs == 6
isFieldValid "hcl" _ = False
isFieldValid "ecl" xs = xs `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isFieldValid "pid" xs = all' isDigit xs && length xs == 9
isFieldValid "hgt" [a,b,c,'c','m'] = all' isDigit xs && between 150 193 (read xs) where xs = [a, b, c]
isFieldValid "hgt" [a,b,'i','n'] = all' isDigit xs && between 59 76 (read xs) where xs = [a, b]
isFieldValid "hgt" _ = False
isFieldValid _ _ = True


allFieldsValid :: Passport -> Bool
allFieldsValid = all' (uncurry isFieldValid)

p2 :: [[(String, String)]] -> Int
p2 = howMany (== True) . fmap (\xs -> allFieldsValid xs && containsRequiredFields xs)


d4p2 :: IO ()
d4p2 = getContents >>= print . p2 . parse
