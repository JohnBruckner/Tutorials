-- Informatics 1 - Functional Programming
-- Tutorial 8
--
-- Week 10 - due: 23--24 November

module Tutorial8 where

import Data.List
import Test.QuickCheck
import Data.Char
import Debug.Trace


-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int]
dm1 =  ([ [],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]


states (u, _, _, _, _) = u
alph   (_, a, _, _, _) = a
start (_, _, s, _, _) = s
final (_, _, _, f, _) = f
trans (_, _, _, _, t) = t


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta fsa i a = map (\(_, _, z) -> z) (filter (\(x, y, _) -> x == i && y == a) $ trans fsa)


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts m xs = acceptsFrom m (start m) xs

acceptsFrom :: (Eq q) => FSM q -> q -> String -> Bool
acceptsFrom m q [] = q `elem` final m
acceptsFrom m q (x:xs) = or [acceptsFrom m s xs | s <- delta m q x]

-- acceptsFrom m q (x:xs) = (or [trace ("comprehending " ++ show (acceptsFrom m s [x]))
--                                           (acceptsFrom m s x) | s <- delta m q x])

-- acceptsFrom :: (Eq q) => FSM q => q -> String -> Bool
-- acceptsFrom m q [] = q `elem` final m


-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical  = sort . nub


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
--ddelta fsa l a = canonical (map (\(_, _, z) -> z) (filter (\(x, y, _) -> (x `elem` l)  && y == a) $ trans fsa))
ddelta fsa l a = canonical . concat $ [delta fsa x a | x <- l]

-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next fsa l = canonical $ l ++ [ddelta fsa x y | x <- l, y <- (alph fsa)]


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable fsa l | l /= next fsa l = reachable fsa (next fsa l)
                | otherwise = l


-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal m l = [x | x <- l, y <- x, y `elem` final m]

-- dstart :: (Ord q) => FSM q -> [[q]] -> [[q]]
-- dstart m l = [x | x <- l, y <- x, y == start m]


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans m l = canonical [(x, y, ddelta m x y) | x <- l, y <- alph m]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic m = (r, a, s, (dfinal m (r)), (dtrans m (r)))
                  where
                    s = [start m]
                    f = final m
                    u = states m
                    r = canonical (reachable m [s])
                    a = alph m

-- Optional Material
--11.
charFSM :: Char -> FSM Int
charFSM = undefined

emptyFSM :: FSM Int
emptyFSM = undefined

--12
intFSM :: (Ord q) => FSM q -> FSM Int
intFSM = undefined

concatFSM :: Ord q => Ord q' => FSM q -> FSM q' -> FSM Int
concatFSM = undefined

--13
stringFSM :: String -> FSM Int
stringFSM = undefined


-- For quickCheck
safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

prop_stringFSM1 n = accepts (stringFSM n') n'
      where n' = safeString n
prop_stringFSM2 n m = (m' == n') || (not $ accepts (stringFSM n') m')
                where m' = safeString m
                      n' = safeString n

--14
completeFSM :: (Ord q) => FSM q -> FSM (Maybe q)
completeFSM = undefined

unionFSM :: (Ord q) => FSM q -> FSM q -> FSM Int
unionFSM a b = undefined

prop_union n m l =  accepts (unionFSM (stringFSM n') (stringFSM m')) l' == (accepts (stringFSM n') l'|| accepts (stringFSM m') l') &&
                    accepts (unionFSM (stringFSM n') (stringFSM m')) n' && accepts (unionFSM (stringFSM n') (stringFSM m')) m'
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l

--15
star :: (Ord q) => FSM q -> FSM q
star = undefined


prop_star a n = (star $ stringFSM a') `accepts` (concat [a' | x <- [0..n]]) &&
                (star $ stringFSM a') `accepts` ""
      where a' = safeString a

--16
complement :: (Ord q) => FSM q -> FSM Int
complement = undefined

prop_complement :: String -> String -> Bool
prop_complement n m = (n' == m')
                      || accepts (complement $ stringFSM n') m'
                      && (not $ accepts (complement $ stringFSM n') n)
                      where n' = safeString n
                            m' = safeString m

-- 17.
intersectFSM :: (Ord q) => FSM q -> FSM q -> FSM (q,q)
intersectFSM a b = undefined

prop_intersect n m l = accepts (intersectFSM (stringFSM n') (stringFSM m')) l' == (accepts (stringFSM n') l' && accepts (stringFSM m') l')
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l



prop1 a b = star ((stringFSM a') `unionFSM` (stringFSM b')) `accepts` (a'++b'++a'++a')
 where a' = safeString a
       b' = safeString b

prop2 a b = ((stringFSM a') `intersectFSM` (intFSM ((stringFSM b') `unionFSM` (stringFSM a')))) `accepts` a'
             where a' = safeString a
                   b' = safeString b
