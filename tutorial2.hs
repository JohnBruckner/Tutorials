-- Informatics 1 - Functional Programming
-- Tutorial 2
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck

import Data.Function
import Data.Maybe


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | x `mod` 2 == 0 = x `div` 2 : halveEvensRec xs
                     | otherwise = halveEvensRec xs


halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvens xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec a b (x:xs) | x < a = inRangeRec a b xs
                      | x > b = inRangeRec a b xs
                      | otherwise =  x : inRangeRec a b xs


inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRangeRec lo hi xs == inRangeRec lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | x > 0 = 1 + countPositivesRec xs
                         | otherwise = 0 + countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs


-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x = digitToInt x * multDigitsRec xs
                     | otherwise  = multDigitsRec xs

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp c s  = if null [snd x | x <- s, fst x == c]
              then c
              else head [snd x | x <- s, fst x == c]


lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec a [] = a
lookUpRec a (x:xs) | fst x == a = snd x
                   | otherwise = lookUpRec a xs


prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUpRec c k == lookUp c k


-- 6.

encipher :: Int -> Char -> Char
encipher i c = lookUpRec c (makeKey i)

-- 7.

normalize :: String -> String
normalize s = [toUpper x | x <- s, isAlpha x || isDigit x]


-- 8.

encipherStr :: Int -> String -> String
encipherStr i str = [encipher i x | x <- normalize str]


-- Optional Material
-- =================

-- 9.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [(snd x, fst x) | x <- xs]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (x:xs) = (snd x, fst x) : reverseKeyRec xs


--reverseKeyHS :: [(Char, Char)] -> [(Char, Char)]

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = reverseKey xs == reverseKeyRec xs


-- 10.undefined

decipher :: Int -> Char -> Char
decipher i c = lookUp c (reverseKey (makeKey i))

-- @TODO error when i is negative
--prop_encDec :: Int -> Char -> Bool
--prop_encDec i c = decipher i (encipher i c) == c


decipherStr :: Int -> String -> String
decipherStr i s = [decipher i x | x <- s]


-- 11.

suffixes :: String -> [String]
suffixes xs = [drop i xs | i <- [0..length xs]]

contains :: String -> String -> Bool
contains str substr = [] /= [ True | s <- suffixes str, isPrefixOf substr s ]

-- 12.

candidates :: String -> [(Int, String)]
candidates str = [(x, y) | x <- [0..26], let y = (decipherStr x str), contains y "AND" || contains y "THE"]



-- 13.

addX :: String -> String
addX s | length s < 5 = addX (s ++ "X")
       | length s == 5 = s

splitEachFive :: String -> [String]
splitEachFive s | length s > 5 =  take 5 s : splitEachFive (drop 5 s)
                | otherwise = [addX s]


-- 14.

prop_transpose :: String -> Bool
prop_transpose = undefined



-- 15.

lipici :: [String] -> String
lipici [] = ""
lipici (x:xs) = x ++ lipici xs

encrypt :: Int -> String -> String
encrypt i s = lipici (transpose (splitEachFive (encipherStr i s)))



-- 16.

splitNEach :: Int -> String -> [String]
splitNEach i s | length s > i = take i s : splitNEach i (drop i s)
               | otherwise = [s]

decrypt :: Int -> String -> String
decrypt i s = decipherStr i (lipici (transpose (splitNEach ((length s) `div` 5) s)))

candidates' :: String -> [(Int, String)]
candidates' str = [(x, y) | x <- [0..26], let y = (decrypt x str), contains y "AND" || contains y "THE"]
