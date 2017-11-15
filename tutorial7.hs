-- Informatics 1 Functional Programming
-- Tutorial 7
--
-- Due: 17/18 November

module Tutorial7 where

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen lst = maximum . map length $ (map (\(x,y) -> fst y) lst)

printN :: Int -> String
printN 0 = ""
printN n = "." ++ printN (n-1)

formatLine :: Int -> (Barcode, Item) -> String
formatLine i (b, it) = show b ++ "..." ++ show (fst it) ++ printN (i - length (fst it))
                              ++ "..." ++ (show (snd it)) ++ "\n"

--showCatalogue :: Catalogue -> String
--showCatalogue cat | size cat == 0 = show (head (toList cat)) ++ "\n"
--                  | otherwise = show (head (toList cat)) ++ showCatalogue (toList (tail (toList cat)))


-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList (Nothing) = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe [a] = Just a

catMaybes :: Eq a => [Maybe a] -> [a]
catMaybes (x:xs) | x == Nothing = catMaybes xs
                 | otherwise = maybeToList x ++ catMaybes xs

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems = undefined






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
