module Chapter14 where

import GHC.Base (Char)
import Recursion (DividedResult (DividedByZero, Result), dividedBy, multiply)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 + 2) `shouldBe` 4
    it "x + 1 is always greater than x" $
      do property $ \x -> x + 1 > (x :: Int)
    it "22 divided by 5 is 4 reminder 2" $ do
      dividedBy 22 5 `shouldBe` Recursion.Result (4, 2)
    it "should not return a result on division by 0" $ do
      dividedBy 32 0 `shouldBe` Recursion.DividedByZero
    it "should return 12 when 4 is multiplied by 3" $ do
      multiply 4 3 `shouldBe` 12
    it "should not matter the order of the args on the multiplication" $ do
      multiply 23 43 `shouldBe` multiply 43 23

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- What QuickCheck does so
-- you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency
    [ (1, return Nothing),
      (3, return (Just a))
    ]

prop_additionaGreater :: Int -> Bool
prop_additionaGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionaGreater

-- left on page 547