module ListChapter where

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : []) = Nothing
safeTail (_ : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool True False = []
eftBool True True = [True]
eftBool False True = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
  | a > b = []
  | a < b = a : eftOrd (succ a) b

eft :: (Ord a, Enum a) => a -> a -> [a]
eft a b
  | a > b = []
  | a <= b = a : eft (succ a) b

myTail [] = []
myTail (_ : xs) = xs

myWords :: String -> [String]
myWords [] = []
myWords xs = takeWhile (/= ' ') xs : myWords ((dropWhile (== ' ') . dropWhile (/= ' ')) xs)

mySplit :: Eq a => a -> [a] -> [[a]]
mySplit _ [] = []
mySplit x xs = takeWhile (/= x) xs : mySplit x (rest xs)
  where
    rest = myTail . dropWhile (/= x)

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = mySplit '\n'

shouldEqual =
  [ "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

poemTest :: IO ()
poemTest =
  print $
    "Are they equal? "
      ++ show
        ( myLines sentences
            == shouldEqual
        )