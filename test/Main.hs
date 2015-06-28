module Main (main) where

import Test.Hspec

import Database.SqlServer.Types.Sequence
import Database.SqlServer.Types.Identifiers

import qualified Data.Set as S

a :: RegularIdentifier
a = RegularIdentifier "a"

aCaps :: RegularIdentifier
aCaps = RegularIdentifier "A"

identifierSpecs :: Spec
identifierSpecs = do
  describe "identifiers" $ do
    it "compare equal even if differing case" $ do
      a == aCaps `shouldBe` True
    it "are handled correctly in sets even if differing in case" $ do
      (S.size (S.fromList [a, aCaps])) `shouldBe` 1

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
  identifierSpecs
  greaterThanMinSpecs
  lessThanMaxSpecs
