{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module AlgebraicDatatypes where

import Data.Char
import Data.Int
import Data.List
import qualified Data.Map as Map

data PugType = PugData

data HuskyType = HyskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar x = case x of
  Car _ _ -> True
  _ -> False

isPlane :: Vehicle -> Bool
isPlane x = case x of
  Plane _ _ -> True
  _ -> False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

data Example = MakeExample Int deriving (Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

instance TooMany (Int, Int) where
  tooMany (x, y) = (x + y) > 42

data BigSmall
  = Big Bool
  | Small Bool
  deriving (Eq, Show)

data NumberOrBool
  = Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

myNumba = Numba (-128)

data FlowerType
  = Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving (Show)

type Gardener = String

data Garden = Garden Gardener FlowerType deriving (Show)

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x : _) = x : xs

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = False
isSubseqOf (x : xs) ys = case x `elemIndex` ys of
  Just index -> isSubseqOf xs (drop index ys)
  Nothing -> False
isSubseqOf _ _ = True

capitalize :: [Char] -> [Char]
capitalize (x : xs) = Data.Char.toUpper x : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\xs -> (xs, capitalize xs)) . words

type DaPhoneNumber = (Char, String)

data DaPhone = DaPhone [DaPhoneNumber]

daPhone =
  DaPhone
    [ ('2', "abc2"),
      ('3', "def3"),
      ('4', "ghi4"),
      ('5', "jkl5"),
      ('6', "mno6"),
      ('7', "pqrs7"),
      ('8', "tuv8"),
      ('9', "wxyz9"),
      ('*', "^"),
      ('0', " "),
      ('#', ".,")
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

message = concat $ intersperse " " convo

type Digit = Char

type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone x = case findCharacterOnPhone phone (toLower x) of
  Just ((num, _), index) -> [('*', 1) | isUpper x] ++ [(num, index + 1)]
  Nothing -> []

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

findCharacterOnPhone :: DaPhone -> Digit -> Maybe (DaPhoneNumber, Int)
findCharacterOnPhone (DaPhone []) _ = Nothing
findCharacterOnPhone (DaPhone ((num, values) : xs)) y = case y `elemIndex` values of
  Just index -> Just ((num, values), index)
  Nothing -> findCharacterOnPhone (DaPhone xs) y

getKeyPresses :: DaPhone -> String -> [(Digit, Presses)]
getKeyPresses phone = concatMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

getGtValue :: Map.Map Char Int -> (Char, Int)
getGtValue =
  let getGt k x (y, count) = if x > count then (k, x) else (y, count)
   in Map.foldrWithKey getGt (' ', 0)

convertToListMap :: String -> [(Char, Int)]
convertToListMap = map (,1)

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

mostPopularLetter :: String -> Char
mostPopularLetter = fst . getGtValue . foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty . removeSpaces