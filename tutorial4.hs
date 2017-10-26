-- Informatics 1 - Functional Programming
-- Tutorial 4
--
-- Due: the tutorial of week 6 (26/27 Oct)

module Tutorial4 where

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:stefan.fehrenbach@ed.ac.uk\">Stefan Fehrenbach</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:stefan.fehrenbach@ed.ac.uk\">Stefan Fehrenbach</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Stefan Fehrenbach","stefan.fehrenbach@ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.

strUpper :: String -> String
strUpper = map toUpper

strLower :: String -> String
strLower = map toLower

sameString :: String -> String -> Bool
sameString s s1 = strUpper s == strUpper s1 && strLower s == strLower s1


-- 2.
prefix :: String -> String -> Bool
prefix s s1 = sameString s (take (length s) s1)

prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) &&
		         prefix substr (map toUpper str)
                           where
                             substr  =  take n str

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str


-- 3.
contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains s s1 = prefix s1 s || contains (tail s) s1

prop_contains :: String -> Int -> Int -> Bool
prop_contains = undefined


-- 4.
takeUntil :: String -> String -> String
takeUntil _ [] = ""
takeUntil s (c:s1) | not (prefix s s1) = [c] ++ takeUntil s s1
                   | otherwise = [c]

dropUntil :: String -> String -> String
dropUntil _ [] = []
dropUntil s s1 | prefix s s1 = drop (length s) s1
               | otherwise = dropUntil s (drop 1 s1)


-- 5.
split :: String -> String -> [String]
split [] _ = error "Separator is not valid"
split _ [] = [""]
split s s1 | contains s1 s = takeUntil s s1 : split s (dropUntil s s1)
           | otherwise = [s1]

reconstruct :: String -> [String] -> String
reconstruct _ [] = ""
reconstruct s (x:s1) | length s1 >= 1 = x ++ s ++ reconstruct s s1
                     | otherwise = x

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML = drop 1 . split "<a href=\""

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails links = [x | x <- links, prefix "mailto:" x]


-- 8.
link2pair :: Link -> (Name, Email)
link2pair lk = (takeUntil "<" (dropUntil ">" lk), takeUntil "\"" (dropUntil ":" lk))


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML =  nub . map link2pair . takeEmails . linksFromHTML

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail _ [] = []
findEmail n (x:tp) | contains (fst x) n = x : findEmail n tp
                   | otherwise = findEmail n tp


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML ht nm  = findEmail nm  (emailsFromHTML ht)


-- Optional Material

-- 12.
hasInitials :: String -> Name -> Bool
hasInitials [] [] = True
hasInitials _ [] = False
hasInitials [] _ = False
hasInitials (x:xs) ys = [x] `prefix` str ys && hasInitials xs (original ys)
                        where str l = head (split " " l)
                              original l = reconstruct " " (drop 1 (split " " l))

-- 13.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML f ht = [x | x <- emailsFromHTML ht, f (fst x)]

emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML str ht = emailsByMatchFromHTML (hasInitials str) ht

-- 14.

-- If your criteria use parameters (like hasInitials), change the type signature.
myCriteria :: Name -> Bool
myCriteria nm = contains nm "ed.ac.uk"

emailsByMyCriteriaFromHTML :: HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML = emailsByMatchFromHTML myCriteria

-- 15
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
