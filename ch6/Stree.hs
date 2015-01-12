data Stree a = Null | Fork (Stree a) a (Stree a)

flatten :: (Ord a) => Stree a -> [a]
flatten Null = []
flatten (Fork xt x yt) = flatten xt ++ [x] ++ flatten yt

inordered :: (Ord a) => Stree a -> Bool
inordered = ordered . flatten
  where ordered []       = True
        ordered [_]      = True
        ordered (x:y:xs) = x <= y && (ordered (y:xs))

member :: (Ord a) => a -> Stree a -> Bool
member _ Null = False
member m (Fork xt x yt)
       | m < x  = member m xt 
       | m == x = True
       | m > x  = member m yt

empty :: Stree a -> Bool
empty Null = True
empty (Fork _ _ _) = False

height :: (Ord a) => Stree a -> Int
height Null = 0
height (Fork xt x yt) = 1 + (height xt `max` height yt)

mkStree :: (Ord a) => [a] -> Stree a
mkStree [] = Null
mkStree (x:xs) = Fork (mkStree ys) x (mkStree zs)
  where (ys, zs) = partition (<= x) xs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

sort :: (Ord a) => [a] -> [a]
sort = flatten . mkStree

size :: Stree a -> Int
size Null = 0
size (Fork xt x yt) = 1 + size xt + size yt

insert :: (Ord a) => a -> Stree a -> Stree a
insert x Null = Fork Null x Null
insert x (Fork xt y yt)
       | x < y  = Fork (insert x xt) y yt
       | x == y = Fork xt y yt
       | x > y  = Fork xt y (insert x yt)
 
delete x Null = Null
delete x (Fork xt y yt)
       | x < y = Fork (delete x xt) y yt
       | x == y = joinTree xt yt
       | x > y  = Fork xt y (delete x yt)

joinTree :: Stree a -> Stree a -> Stree a
joinTree xt yt = if empty yt 
                    then xt 
                    else Fork xt (headTree yt) (tailTree yt)

splitTree :: Stree a -> (a, Stree a)
splitTree (Fork xt y yt) = if empty xt
                              then (y,yt)
                              else (x, Fork wt y yt)
  where (x,wt) = splitTree xt

headTree :: Stree a -> a
headTree = fst . splitTree

tailTree :: Stree a -> Stree a
tailTree = snd . splitTree

mapStree :: (a -> b) -> Stree a -> Stree b
mapStree f Null = Null
mapStree f (Fork xt x yt) = Fork (mapStree f xt) (f x) (mapStree f yt)

foldStree :: (a -> b -> b -> b) -> b -> (Stree a) -> b
foldStree f z Null = z
foldStree f z (Fork xt x yt) = f x (foldStree f z xt) (foldStree f z yt)
