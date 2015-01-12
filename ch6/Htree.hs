module Htree where

data Htree a = Null | Fork a (Htree a) (Htree a)
  deriving Show

flatten :: Ord a => Htree a -> [a]
flatten Null = []
flatten (Fork x xt yt) = x : merge (flatten xt) (flatten yt)

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
  if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

--heapOrdered :: (Ord a) => Htree a -> Bool
--heapOrdered = ordered . flatten

--mkHeap :: Ord a => [a] -> Htree a
--mkHeap = heapify . mkHtree

heapify :: Ord a => Htree a -> Htree a
heapify Null = Null
heapify (Fork x xt yt) = sift x (heapify xt) (heapify yt)

sift :: Ord a => a -> Htree a -> Htree a -> Htree a
sift x Null Null = Fork x Null Null
sift x (Fork y a b) Null = if x <= y
                              then Fork x (Fork y a b) Null
                              else Fork y (sift x a b) Null
sift x Null (Fork z c d) = if x <= z
                              then Fork x Null (Fork z c d)
                              else Fork z Null (sift x c d)
sift x (Fork y a b) (Fork z c d)
     | x <= (y `min` z) = Fork x (Fork y a b) (Fork z c d)
     | y <= (x `min` z) = Fork y (sift x a b) (Fork z c d)
     | z <= (x `min` y) = Fork z (Fork y a b) (sift x c d)

--sort :: Ord a => [a] -> [a]
--sort = flatten . mkHeap

levels :: [a] -> [[a]]
levels = levelsWith 1

levelsWith :: Integral a => a -> [b] -> [[b]]
levelsWith lvl xs = p : levelsWith (2*lvl) r
  where (p,r) = splitAt lvl xs

mkHtrees :: [[a]] -> [Htree a]
mkHtrees = foldr addLayer [Null]

addLayer :: [a] -> [Htree a] -> [Htree a]
addLayer [] _               = []
addLayer (x:xs) []          = Fork x Null Null : addLayer xs []
addLayer (x:xs) [xt]        = Fork x xt Null : addLayer xs []
addLayer (x:xs) (xt:yt:zts) = Fork x xt yt : addLayer xs zts

mkHtree = head . mkHtrees . levels
