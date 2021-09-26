module Recursion where

fibonnaci :: (Eq a, Num a, Num p) => a -> p
fibonnaci 0 = 0
fibonnaci 1 = 1
fibonnaci x = fibonnaci (x - 1) + fibonnaci (x - 2)

data DividedResult = Result (Integer, Integer) | DividedByZero deriving (Show, Eq)

dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denominator = go num (abs denominator) 0
  where
    go n d count
      | n < d = Result (if denominator < 0 then (- count) else count, n)
      | otherwise =
        go (n - d) d (count + 1)

getSum :: (Eq a, Num a) => a -> a
getSum x
  | x == 0 = 0
  | otherwise = x + getSum (x -1)

multiply :: (Integral a) => a -> a -> a
multiply x y
  | y == 1 = x
  | otherwise = x + multiply x (y - 1)

mc91 :: (Ord a, Num a) => a -> a
mc91 x
  | x > 100 = x - 10
  | x <= 100 = 91

fibs = 1 : scanl (+) 1 fibs