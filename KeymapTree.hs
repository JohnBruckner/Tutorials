-- INF 1 Functional Programming
--
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf
                               (Node 4 40 Leaf Leaf ))


-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) | depth (left) > depth (right) = 1 + depth (left)
                            | otherwise = 1 + depth (right)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k a left right) = toList left ++ [(k, a)] ++ toList right

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = f left
                              | otherwise = f right

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key Leaf                              =  Nothing
get key (Node k v left right) | key == k  = Just v
                              | key < k   = get key left
                              | otherwise = get key right
-- get key = f
--           where
--               f Leaf = Nothing
--               f (Node k v left right) | key == k = value
--                                       | key < k = f left
--                                       | otherwise = f right


prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList xs = foldr (\(key,value) tree -> set key value tree) (Leaf) xs
-- fromList [] = Leaf
-- fromList (x:xs) = uncurry set x (fromList xs)
-- fromList xs = foldr f Leaf xs
--               where
--                 f x = set (fst x) (snd x)


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
--filterLT i l = fromList (filter (\(key, _) -> key > i) (toList l))
filterLT _ Leaf = Leaf
filterLT key (Node k a (left) (right)) | key > k = Node k a (left) (filterLT key right)
                                       | key == k = left
                                       | otherwise = filterLT key left



filterGT :: Ord k => k -> Keymap k a -> Keymap k a
--filterGT i l = fromList (filter (\(key, _) -> key < i) (toList l))
filterGT _ Leaf = Leaf
filterGT key (Node k a (left) (right)) | key > k = filterGT key right
                                       | key == k = right
                                       | otherwise = Node k a (filterGT key left) right




-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge tree1 tree2 = fromList ((toList tree1) ++ (toList tree2))

merge' :: Ord k => Keymap k  a -> Keymap k a -> Keymap k a
merge' Leaf Leaf = Leaf
merge' Leaf tree2 = tree2
merge' tree1 Leaf = tree1
--merge' (Node k a (left) (right)) tree2 = set (fromList [(k, a)]) (merge' tree tree2)



-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del = undefined

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select = undefined

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
