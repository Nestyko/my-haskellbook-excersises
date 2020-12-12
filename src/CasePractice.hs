functionC :: Ord p => p -> p -> p
functionC x y = if (x > y) then x else y

functionC' :: Ord p => p -> p -> p
functionC' x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 :: Integral p => p -> p
ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

nums :: (Ord a, Num a, Num p) => a -> p
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2

avgGrade :: (Ord a, Fractional a) => a -> Char
avgGrade x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | y
      < 0.59 =
    'F'
  where
    y = x / 100

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    (_, d) = xLast `divMod` 10

hunsD :: Integer -> Integer
hunsD = (`mod` 10) . (`div` 100)

foldBool :: a -> a -> Bool -> a
foldBool x y flag =
  case flag of
    True -> x
    False -> y

foldBool2 :: p -> p -> Bool -> p
foldBool2 x y flag
  | flag = x
  | not flag = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
