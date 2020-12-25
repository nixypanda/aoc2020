module Day25 (d25p1, d25p2) where

import Math.NumberTheory.Powers.Modular ( powMod )


modulus :: Int
modulus = 20201227

-- >>> take 10 $ loop 7
-- [7,49,343,2401,16807,117649,823543,5764801,20152380,19859298]
loop :: Int -> [Int]
loop subjectNum = [powMod subjectNum n modulus | n <- [1..]]

-- >>> loopSize 5764801 7
-- 8
-- >>> loopSize 17807724 7
-- 11
loopSize :: Int -> Int -> Int
loopSize pubKey = (+1) . length . takeWhile (/= pubKey) . loop 


-- >>> encKey 8 17807724
-- 14897079
-- >>> encKey 11 5764801
-- 14897079
encKey :: Int -> Int -> Int
encKey loopSize = (!! (loopSize - 1)) . loop 


p1 :: Int -> Int -> Int -> Int -> Int
p1 doorPubKey cardPubKey doorSubjectNum cardSubjectNum = encKey doorLoopSize cardPubKey
    where
        doorLoopSize = loopSize doorPubKey doorSubjectNum
        cardLoopSize = loopSize cardPubKey cardSubjectNum


d25p1 :: IO ()
d25p1 = print $ p1 9717666 20089533 7 7

d25p2 :: IO ()
d25p2 = print "It's free"
