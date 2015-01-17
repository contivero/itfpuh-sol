module Arbint where

import Prelude hiding (replicate)

newtype ArbInt = Norm [Digit]
type Digit = Int
type Carry = Int

digits :: ArbInt -> [Digit]
digits (Norm xs) = xs

base, baseSize :: Int
base = 10000
baseSize = 4

carry :: Digit -> (Carry, [Digit]) -> (Carry, [Digit])
carry x (c, xs) = ((x + c) `div` base, (x + c) `mod` base : xs)

norm :: [Int] -> ArbInt
norm = Norm . dropWhile (==0) . addCarry . foldr carry (0,[])

addCarry :: (Carry, [Digit]) -> [Digit]
addCarry (c,xs) = if -1 <= c && c < base
                     then c:xs
                     else addCarry (c `div` base, c `mod` base : xs)

align :: ([Digit], [Digit]) -> ([Digit], [Digit])
align (xs, ys)
     | n > 0 = (replicate n 0 ++ xs, ys)
     | n <= 0 = (xs, replicate (-n) 0 ++ ys)
      where n = length ys - length xs

replicate :: Int -> a -> [a]
replicate n x = [x | k <- [1..n]]

instance Eq ArbInt where
  (==) = translate (==)

instance Ord ArbInt where
  (<=) = translate (<=)

translate :: ([Digit] -> [Digit] -> Bool) -> (ArbInt -> ArbInt -> Bool)
translate op x y = op xs ys
  where (xs, ys) = align (digits x, digits y)

-- isZero uses the assumption that the argument is a numeral in normal form
isZero :: ArbInt -> Bool
isZero = null . digits

zipp :: ([a], [b]) -> [(a,b)]
zipp = uncurry zip

zippWith :: (a -> b -> c) -> ([a], [b]) -> [c]
zippWith f = map (uncurry f) . zipp

instance Num ArbInt where
  x + y = norm (zippWith (+) (align (digits x, digits y)))
  x - y = norm (zippWith (-) (align (digits x, digits y)))
  x * y = foldl splus (Norm []) (sums x y)
  negate = norm . map neg . digits
    where neg x = -x
  abs x = if x > 0
             then x
             else (-x)
  signum x
      | x > 0  = 1
      | x == 0 = 0
      | x < 0  = -1
  fromInteger i = undefined

negative :: ArbInt -> Bool
negative (Norm xs) = not (null xs) && (head xs < 0)


sums :: ArbInt -> ArbInt -> [ArbInt]
sums x y = map (mult1 x) (digits y)

mult1 :: ArbInt -> Digit -> ArbInt
mult1 x d = norm (map (*d) (digits x))

splus :: ArbInt -> ArbInt -> ArbInt
splus x y = shift x + y

shift :: ArbInt -> ArbInt
shift (Norm xs) = if null xs
                     then Norm []
                     else Norm (xs ++ [0])

splus1 :: ArbInt -> Digit -> ArbInt
splus1 r d = Norm (digits r ++ [d])

step y (q,r) d
    | m < n     = (0,x)
    | m == n    = bstep x y
    | otherwise = cstep x y
      where m = length (digits r) + 1
            n = length (digits y)
            x = splus1 r d

bstep x y = if negative z
               then (0,x)
               else (1,z)
  where z = x - y

cstep :: ArbInt -> ArbInt -> (Digit, ArbInt)
cstep x y
     | not (negative r0) = (q, r0)
     | not (negative r1) = (q - 1, r1)
     | otherwise         = (q - 2, r2)
      where q  = guess x y
            r0 = x - mult1 y q
            r1 = r0 + y
            r2 = r1 + y

guess :: ArbInt -> ArbInt -> Digit
guess x y = if x0 >= y1
               then base - 1
               else (x0 * base + x1) `div` y1
  where x0 = xs !! 0
        x1 = xs !! 1
        xs = digits x
        y1 = (digits y) !! 0

quotrem1 :: ArbInt -> Digit -> (ArbInt, Digit)
quotrem1 x d = finish1 (scanl (step1 d) (0,0) (digits x))

step1 :: Digit -> (Digit, Digit) -> Digit -> (Digit, Digit)
step1 d (q, r) x = (y `div` d, y `mod` d)
  where y = base*r + x

finish1 :: [(Digit, Digit)] -> (ArbInt, Digit)
finish1 qrs = (norm (map fst qrs), snd (last qrs))

quotrem :: ArbInt -> ArbInt -> (ArbInt, ArbInt)
quotrem x y = finish d (scanl (step y') (0, Norm []) (digits x'))
  where x' = mult1 x d
        y' = mult1 y d
        d = base `div` (head (digits y) + 1)

finish :: Digit -> [(Digit, ArbInt)] -> (ArbInt, ArbInt)
finish d qrs = (norm (map fst qrs), div1 (snd (last qrs)) d)

div1 :: ArbInt -> Digit -> ArbInt
div1 x d = fst (quotrem1 x d)

instance Show ArbInt where
  showsPrec k x
      | isZero x   = showChar '0'
      | negative x = showChar '-' . showDigits (digits (negate x))
      | otherwise  = showDigits (digits x)

showDigits :: [Digit] -> String -> String
showDigits (d:ds) = showString (show d) . showString (concat (map showDigit ds))

showDigit :: Digit -> String
showDigit d = zeros ++ ds
  where zeros = replicate (baseSize - length ds) '0'
        ds    = show d

