module Btree where

data Btree a = Leaf a | Fork (Btree a) (Btree a)
  deriving Show

size :: Num a => Btree b -> a
size (Leaf a) = 1
size (Fork xt yt) = size xt + size yt

flatten :: Btree a -> [a]
flatten (Leaf a) = [a]
flatten (Fork xt yt) = flatten xt ++ flatten yt

nodes :: Num a => Btree b -> a
nodes (Leaf a) = 0
nodes (Fork xt yt) = 1 + nodes xt + nodes yt

-- Prove that size xt = 1 + nodes xt

height :: (Ord a, Num a) => Btree b -> a
height (Leaf a) = 0
height (Fork xt yt) = 1 + (height xt `max` height yt)

depths :: Btree a -> Btree Int
depths = down 0

down :: Int -> Btree a -> Btree Int
down n (Leaf x) = Leaf n
down n (Fork xt yt) = Fork (down (n+1) xt) (down (n+1) yt)

maxBtree :: Ord a => Btree a -> a
maxBtree (Leaf x) = x
maxBtree (Fork xt yt) = (maxBtree xt) `max` (maxBtree yt)

-- prove that height = maxBtree . depths

-- O(n*log n), on a list of size n
-- Build a minimum height tree from a list
mkBtree :: [a] -> Btree a
mkBtree xs
  | (m == 0)  = Leaf (unwrap xs)
  | otherwise = Fork (mkBtree ys) (mkBtree zs)
  where m        = (length xs) `div` 2
        (ys, zs) = splitAt m xs

unwrap :: [a] -> a
unwrap [x] = x

mapBtree :: (a -> b) -> Btree a -> Btree b
mapBtree f (Leaf x)     = Leaf (f x)
mapBtree f (Fork xt yt) = Fork (mapBtree f xt) (mapBtree f yt)

foldBtree :: (a -> b) -> (b -> b -> b) -> Btree a -> b
foldBtree f g (Leaf x)     = f x
foldBtree f g (Fork xt yt) = g (foldBtree f g xt) (foldBtree f g yt)

subtree (Leaf x)     = [Leaf x]
subtree (Fork xt yt) = [Fork xt yt] ++ subtree xt ++ subtree yt

t1 = Leaf 1
t2 = Fork t1 t1
t3 = Fork t2 t1
t4 = Fork t1 t3
t5 = Fork t4 t1
t6 = Fork t3 t3
