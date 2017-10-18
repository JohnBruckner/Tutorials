-- Informatics 1 - Functional Programming
-- Tutorial 3
--
-- Week 5 - Due: 19-20 Oct.

module Tutorial3 where

import Data.Char
import Test.QuickCheck



-- 1. Map
-- a.
uppers :: String -> String
uppers s = map toUpper s

-- b.
doubles :: [Int] -> [Int]
doubles xs = map (\x -> 2*x) xs
            --where double x = 2*x

-- c.
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map divTen xs
            where divTen x = (fromIntegral x) / 10.0

-- d.
uppers' :: String -> String
uppers' s = [toUpper x | x <- s]

prop_uppers :: String -> Bool
prop_uppers s = uppers s == uppers' s




-- 2. Filter
-- a.
alphas :: String -> String
alphas s = filter isAlpha s

-- b.
rmChar ::  Char -> String -> String
rmChar c s = filter (\x -> x /= c) s


-- c.
above :: Int -> [Int] -> [Int]
above i xs = filter (\x -> x>= i) xs
              --where isAbove x = x >= i

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals ps = filter (\(x, y) -> x /= y) ps
               --where equal xs = fst xs /= snd xs

-- e.
rmCharComp :: Char -> String -> String
rmCharComp c s = [x | x <- s, x /= c]

prop_rmChar :: Char -> String -> Bool
prop_rmChar c s = rmChar c s == rmCharComp c s



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' = filter isAlpha . map toUpper

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' = map dbls . filter abv
              where dbls x = 2*x
                    abv x = x > 3

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' = map reverse . filter (even . length)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold xs = foldr (*) 1 xs

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (b:bs) = b && andRec bs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- c.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec [] s = s
--rmCharsRec _ [] = ""
rmCharsRec (x:xs) s = rmCharsRec xs (rmChar x s)

rmCharsFold :: String -> String -> String
rmCharsFold xs s = undefined

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform [] = True
uniform (x:xs) = all (==x) xs

-- b.
valid :: Matrix -> Bool
valid m = uniform (map length m) && notElem 0 (map length m)


-- 6.
--a. Returns the sum of 8 and 10

--b.
zipWith' :: (a -> a) -> [a] -> [a] -> [(a, a)]
zipWith' f xs ys = zip [f x | x <- xs] [f y | y <- ys]

--prop_zipWith :: (a -> a) -> [a] -> [a] -> [(a, a)] -> Bool
--prop_zipWith f xs ys = zipWith' f xs ys == zipWith f xs ys



-- 7.
plusM :: Matrix -> Matrix -> Matrix
plusM = undefined

-- 8.
timesM :: Matrix -> Matrix -> Matrix
timesM = undefined

-- Optional material
-- 9.
