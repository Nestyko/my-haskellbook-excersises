module Chapter14 where

import Recursion (DividedResult (DividedByZero, Result), dividedBy, multiply)
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 + 2) `shouldBe` 4
    prop "x + 1 is always greater than x" $
      \x -> (x + 1) > (x :: Int)
    it "22 divided by 5 is 4 reminder 2" $ do
      dividedBy 22 5 `shouldBe` Recursion.Result (4, 2)
    it "should not return a result on division by 0" $ do
      dividedBy 32 0 `shouldBe` Recursion.DividedByZero
    it "should return 12 when 4 is multiplied by 3" $ do
      multiply 4 3 `shouldBe` 12
    it "should not matter the order of the args on the multiplication" $ do
      multiply 23 43 `shouldBe` multiply 43 23

-- left on page 547