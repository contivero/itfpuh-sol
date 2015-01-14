module Huffman where

import Btree
import Prelude hiding (lookup)

data Huff = Tip Int Char | Node Int Huff Huff

type Bit = Int

type CodeTable = [(Char, [Bit])]

decode :: Btree Char -> [Bit] -> [Char]
decode t cs = if null cs
                 then []
                 else decode1 t cs
  where decode1 (Leaf x) cs = x :decode t cs
        decode1 (Fork xt yt) (0:cs) = decode1 xt cs
        decode1 (Fork xt yt) (1:cs) = decode1 yt cs

encode :: Btree Char -> [Char] -> [Bit]
encode t = concat . map (lookup codetable)
  where codetable = transform t

lookup :: CodeTable -> Char -> [Bit]
lookup ((x,bs) : xbs) y = if x == y
                             then bs
                             else lookup xbs y

transform :: Btree Char -> CodeTable
transform (Leaf x) = [(x,[])]
transform (Fork xt yt) = hufmerge (transform xt) (transform yt)

hufmerge :: CodeTable -> CodeTable -> CodeTable
hufmerge [] ycs = [(y, 1:cs) | (y,cs) <- ycs]
hufmerge xbs [] = [(x, 0:bs) | (x,bs) <- xbs]
hufmerge ((x, bs):xbs) ((y, cs):ycs)
        | length bs <= length cs = (x, 0:bs) : hufmerge xbs ((y, cs) : ycs)
        | otherwise              = (y, 1:cs) : hufmerge ((x, bs) : xbs) ycs

unlabel :: Huff -> Btree Char
unlabel (Tip w x)      = Leaf x
unlabel (Node w xt yt) = Fork (unlabel xt) (unlabel yt)

--mkHuff :: [(Char, Int)] -> Huff
--mkHuff = unwrap . until singleton combine . map mkTip

mkTip :: (Char, Int) -> Huff
mkTip (c, w) = Tip w c

-- combine ts combines to trees in ts with the lightest weight
combine :: [Huff] -> [Huff]
combine (xt:yt:xts) = insert (Node w xt yt) xts
  where w = weight xt + weight yt

weight :: Huff -> Int
weight (Tip w c)      = w
weight (Node w xt yt) = w

-- Inserts a tree in the correct place to maintain the property that the list
-- of trees is in ascending order of weight
insert :: Huff -> [Huff] -> [Huff]
insert xt yts = uts ++ [xt] ++ vts
  where (uts, vts) = span p yts
        p yt       = (weight yt <= weight xt)
