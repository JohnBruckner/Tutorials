-- Informatics 1 - Functional Programming
-- Tutorial 6
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

--module Tutorial6 where


import LSystem
import Data.List
import Test.QuickCheck

main :: IO ()
main = display (arrowhead 10)


-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (Go d) = [Go d]
split (Turn a) = [Turn a]
split (Sit) = []
split (c :#: c1) = (split c) ++ (split c1)


-- 1b. join
join :: [Command] -> Command
join [(Go d)] = Go d
join [(Turn a)] = Turn a
join (c:commands) = (join [c]) :#: (join commands)

-- 1c. equivalent
equivalent :: Command -> Command -> Bool
equivalent c c1 = (split c) == (split c1)

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent c (join (split c))

prop_split :: Command -> Bool
prop_split c = (not (Sit `elem` commands))
                where commands = split c


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy 1 c = c
copy n c = c :#: (copy (n-1) c)

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d s = copy s (Go d :#: Turn (360 / (fromIntegral s)))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral side 1 step angle = (Go side :#: Turn angle)
spiral side n step angle = (Go side :#: Turn angle)
                            :#:
                           (spiral (side + step) (n-1) step angle)


-- Exercise 4
-- Remember that Go does not take negative arguments.

-- removeUnused :: Command -> Command
-- removeUnused c = join [x | x <- (split c),
--                       (x /= (Go 0)) && (x /= (Turn 0)) && (x /= (Turn 360))]
--
-- rules :: Command -> Command -> Command
-- rules (Go a) (Go b) = Go (a+b)
-- rules (Turn a) (Turn b) = Turn (a+b)
-- rules c c1 = c :#: c1
--
-- optimiseCmds :: [Command] -> [Command]
-- optimseCmds [x] = x
-- optimiseCmds (x:y:xs) = [rules ]
-- -- optimiseCmds (x:xs) | optimiseCmds (x:xs) == (join (x:xs)) = join (x:xs)
-- --                     | otherwise = optimiseCmds ([rules a b | (a,b) <- zip (x:xs) xs])
--
-- optimise :: Command -> Command
-- optimise c = undefined --optimiseCmds (split c)


-- L-Systems

-- 5. arrowhead
-- angle: 60
-- start: f
-- rewrite:
--   f → g+f+g
--   g → f-g-f

arrowhead :: Int -> Command
arrowhead x = f x
              where f 0 = GrabPen red :#: Go 10
                    f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
                    g 0 = GrabPen blue :#: Go 10
                    g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
                    n = Turn (-60)
                    p = Turn (60)


-- 6. snowflake
-- angle: 60
-- start: f--f--f--
-- rewrite: f → f+f--f+f

snowflake :: Int -> Command
snowflake x = (f x) :#: n :#: n :#: (f x) :#: n :#: n :#: (f x) :#: n :#: n
              where
                f 0 = GrabPen red :#: Go 10
                f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
                p = Turn 60
                n = Turn (-60)


-- 7. hilbert
-- angle: 90
-- start: l
-- rewrite:
-- l → +rf-lfl-fr+
-- r → -lf+rfr+fl-

hilbert :: Int -> Command
hilbert x = l x
          where
            f = GrabPen red :#: Go 10.0
            l 0 = p
            l x = p :#: r (x-1) :#: f :#: n :#: l (x-1) :#: f :#: l (x-1) :#: n :#: f :#: r (x-1) :#: p
            r 0 = n
            r x = n :#: l (x-1) :#: f :#: p :#: r (x-1) :#: f :#: r (x-1) :#: p :#: f :#: l (x-1) :#: n
            p = Turn 90
            n = Turn (-90)

-- Bonus L-Systems

peanoGosper = undefined


cross = undefined


branch = undefined

thirtytwo = undefined
