module Main (main) where

import Test.Hspec

import Database.SqlServer.Types.Sequence

{-
greaterThanMin :: Maybe Integer -> Maybe Integer -> Bool
lessThanMax :: Maybe Integer -> Maybe Integer -> Bool
-}

greaterThanMinSpecs :: Spec
greaterThanMinSpecs = do
  describe "greaterThanMin" $ do
    it "returns true if between a specific range" $ do
      greaterThanMin Nothing Nothing `shouldBe` True
    it "returns true if the value is greater than specified" $ do
      greaterThanMin (Just 1) (Just 10) `shouldBe` True
    it "returns false if the value is less than specified" $ do
      greaterThanMin (Just 1) (Just 0) `shouldBe` False
    it "returns true if the second value is nothing" $ do
      greaterThanMin (Just 101) Nothing `shouldBe` True

lessThanMaxSpecs :: Spec
lessThanMaxSpecs = do
  describe "lessThanMax" $ do
    it "returns true if between a specific range" $ do
      lessThanMax Nothing Nothing `shouldBe` True
    it "returns true if the value is greater than specified" $ do
      lessThanMax (Just 1) (Just 10) `shouldBe` False
    it "returns false if the value is less than specified" $ do
      lessThanMax (Just 1) (Just 0) `shouldBe` True

main :: IO()
main = hspec $ do
  greaterThanMinSpecs
  lessThanMaxSpecs
