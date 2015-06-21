{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.Identifiers where

import Database.SqlServer.Types.Reserved (isReserved)

import Test.QuickCheck
import Test.QuickCheck.Gen

-- https://msdn.microsoft.com/en-us/subscriptions/downloads/ms175874
newtype RegularIdentifier = RegularIdentifier String deriving Eq

firstChars :: String
firstChars = ['a'..'z'] ++ ['A'..'Z'] ++ "_@#"

subsequentChars :: String
subsequentChars = firstChars ++ ['0'..'9']

maximumLengthOfRegularIdentifier :: Int
maximumLengthOfRegularIdentifier = 128

validLength :: String -> Bool
validLength x = length x < maximumLengthOfRegularIdentifier

-- https://msdn.microsoft.com/en-us/library/ms189822.aspx
newtype Keyword = Keyword String

instance Arbitrary RegularIdentifier where
  arbitrary = do
    x <- elements firstChars
    y <- listOf (elements subsequentChars) `suchThat` validLength
    return (RegularIdentifier $ x : y)

instance Show RegularIdentifier where
  show (RegularIdentifier s) = s