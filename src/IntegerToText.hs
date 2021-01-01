module IntegerToText where

import Data.List

getNumber :: (Eq a, Num a) => a -> String
getNumber 0 = "zero"
getNumber 1 = "one"
getNumber 2 = "two"
getNumber 3 = "three"
getNumber 4 = "four"
getNumber 5 = "five"
getNumber 6 = "six"
getNumber 7 = "seven"
getNumber 8 = "eight"
getNumber 9 = "nine"
getNumber 10 = "ten"
getNumber 11 = "eleven"
getNumber 12 = "twelve"
getNumber 13 = "thirdteen"
getNumber 14 = "fourteen"
getNumber 15 = "fifteen"
getNumber 16 = "sixteen"
getNumber 17 = "seventeen"
getNumber 18 = "eighteen"
getNumber 19 = "nineteen"
getNumber 20 = "twenty"
getNumber 30 = "thirty"
getNumber 40 = "fourty"
getNumber 50 = "fifty"
getNumber 60 = "sixty"
getNumber 70 = "seventy"
getNumber 80 = "eighty"
getNumber 90 = "ninety"
getNumber 100 = "hundred"
getNumber 1000 = "thousand"
getNumber 1000000 = "million"
getNumber 1000000000 = "billion"
getNumber 1000000000000 = "trillion"

integerToText :: Int -> [Char]
integerToText num
  | num <= 20 = getNumber num
  | otherwise = intercalate " " . reverse $ intToTextInternal num 0

intToTextInternal :: (Integral t1, Integral t2) => t1 -> t2 -> [[Char]]
intToTextInternal num _
  | num <= 0 = []
intToTextInternal num place
  | unit == 0 = nextNumber
  | place < 2 = lowNumberTail
  | otherwise = highNumberTail
  where
    unit = mod num 10
    power = 10 ^ place
    lowNumberTail = getNumber (unit * power) : nextNumber
    highNumberTail = getNumber (1 * power) : getNumber (unit) : nextNumber
    nextNumber = intToTextInternal (quot num 10) (place + 1)

testResult :: Bool -> String
testResult True = "Passed"
testResult False = "Failed"

assertEqual :: (Show a, Eq a) => a -> a -> String
assertEqual expect toEqual =
  if expect == toEqual
    then "Passed"
    else "Expected: " ++ show (expect) ++ "; Recieved: " ++ show (toEqual)

main :: IO ()
main =
  do
    putStrLn ("Running IntegerToText")
    putStrLn ("0: " ++ (assertEqual "zero" $ integerToText (0)))
    putStrLn ("1: " ++ (assertEqual "one" $ integerToText (1)))
    putStrLn ("7: " ++ (assertEqual "seven" $ integerToText (7)))
    putStrLn ("10: " ++ (assertEqual "ten" $ integerToText (10)))
    putStrLn ("15: " ++ (assertEqual "fifteen" $ integerToText (15)))
    putStrLn ("22: " ++ (assertEqual "twenty two" $ integerToText (22)))
    putStrLn ("27: " ++ (assertEqual "twenty seven" $ integerToText (27)))
    putStrLn ("37: " ++ (assertEqual "thirty seven" $ integerToText (37)))
    putStrLn ("88: " ++ (assertEqual "eighty eight" $ integerToText (88)))
    -- putStrLn ("115: " ++ (assertEqual "one hundred fifteen" $ integerToText (115)))
    putStrLn ("126: " ++ (assertEqual "one hundred twenty six" $ integerToText (126)))
    putStrLn ("254: " ++ (assertEqual "two hundred fifty four" $ integerToText (254)))
    putStrLn ("3700: " ++ (assertEqual "three thousand seven hundred" $ integerToText (3700)))
    putStrLn ("1000002: " ++ (assertEqual "one million two" $ integerToText (1000002)))
    putStrLn ("2000189: " ++ (assertEqual "two million one hundred eighty nine" $ integerToText (2000189)))
