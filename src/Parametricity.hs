module Parametricity where

myHope :: a -> a -> a
myHope a _ = a

jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined


