module Rose where

import Btree

data Rose a = Node a [Rose a]

sizeR :: Rose a -> Int
sizeR (Node a rs) = 1 + sum (map sizeR rs)

heightR :: Rose a -> Int
heightR (Node a rs) = 1 + maxlist (map heightR rs)

maxlist :: Ord a => [a] -> a
maxlist = foldl1 max

depthsR :: Rose a -> Rose Int
depthsR = downR 1

downR :: Int -> Rose a -> Rose Int
downR n (Node x xts) = Node n (map (downR (n+1)) xts)

maxRose :: Ord a => Rose a -> a
maxRose (Node x xts) = x `max` maxlist (map maxRose xts)

foldRose :: (a -> [b] -> b) -> Rose a -> b
foldRose f (Node x xts) = f x (map (foldRose f) xts)

-- size    = foldRose f where f x ns = 1 + sum ns
-- maxRose = foldRose f where f x ns = x `max` maxlist ns

mapRose :: (a -> b) -> Rose a -> Rose b
mapRose f = foldRose (Node . f)

toB :: Rose a -> Btree a
toB (Node x xts) = foldl Fork (Leaf x) (map toB xts)

-- Not very efficient version
-- --------------------------
--toR (Leaf x) = Node x []
--toR (Fork xb yb) = Node x (xts ++ [xt])
--  where Node x xts = toR xb
--        xt         = toR yb

-- More efficient

toR :: Btree a -> Rose a
toR xb = collect xb []

collect :: Btree a -> [Rose a] -> Rose a
collect (Leaf x) yts = Node x yts
collect (Fork xb yb) yts = collect xb (toR yb:yts)

flattenR :: Rose a -> [a]
flattenR (Node x xts) = x : concat (map flattenR xts)

levelsR :: Rose a -> [[a]]
levelsR (Node x xts) = [x] : combine (map levelsR xts)

levelR :: Rose a -> [a]
levelR = concat . levelsR

index :: Int -> [[a]] -> [a]
index i xss = if i < length xss
                 then xss !! i
                 else []

combine :: [[[a]]] -> [[a]]
combine = foldr f []
  where f :: [[a]] -> [[a]] -> [[a]]
        [] `f` yss            = yss
        (xs:xss) `f` []       = xs:xss
        (xs:xss) `f` (ys:yss) = (xs ++ ys) : (xss `f` yss)
