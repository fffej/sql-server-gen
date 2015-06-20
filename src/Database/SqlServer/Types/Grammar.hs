{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.Grammar where

import Database.SqlServer.Types.Reserved (isReserved)

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Data.List (nub,intersperse)

import Data.DeriveTH



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

-- Size of arbitrary data (>= 1 && <= 8000)
newtype FixedRange = FixedRange Int

-- See for example https://msdn.microsoft.com/en-us/library/ms176089.aspx
data Range = Sized FixedRange
           | Max

-- https://msdn.microsoft.com/en-us/library/ms187752.aspx
data Type = BigInt
          | Bit
          | Numeric
          | SmallInt
          | Decimal
          | SmallMoney
          | Int
          | TinyInt
          | Money
          | Float
          | Real
          | Date
          | DataTimeOffset
          | DateTime2
          | SmallDateTime
          | DateTime
          | Time
          | Char FixedRange
          | VarChar Range
          | Text
          | NChar
          | NVarChar
          | NText
          | Binary FixedRange
          | VarBinary Range -- FILESTREAM valid only for varbinary(max)
          | Image
          | Cursor
          | Timestamp
          | HierarchyId
          | UniqueIdentifier
          | SqlVariant
          | Xml
          | Table
          | Geography
          | Geometry

newtype ColumnDefinitions = ColumnDefinitions [ColumnDefinition]

-- https://msdn.microsoft.com/en-us/library/ms174979.aspx
data TableDefinition = TableDefinition
             {
               table_name    :: RegularIdentifier
             , column_definitions :: ColumnDefinitions
             }

uniqueNames :: [ColumnDefinition] -> Bool
uniqueNames xs = length xs == length (nub $ map column_name xs)

data ColumnDefinition = ColumnDefinition
                        {
                          column_name :: RegularIdentifier
                        , data_type   :: Type
                        , null_constraint :: Maybe Bool
                        }

instance Arbitrary FixedRange where
  arbitrary = liftM FixedRange (choose (1,8000))

instance Arbitrary RegularIdentifier where
  arbitrary = do
    x <- elements firstChars
    y <- listOf (elements subsequentChars) `suchThat` validLength
    return (RegularIdentifier $ x : y)

instance Arbitrary ColumnDefinitions where
  arbitrary = liftM ColumnDefinitions $ listOf1 arbitrary
    
derive makeArbitrary ''Type

derive makeArbitrary ''TableDefinition

derive makeArbitrary ''ColumnDefinition

derive makeArbitrary ''Range

instance Show RegularIdentifier where
  show (RegularIdentifier s) = s

instance Show FixedRange where
  show (FixedRange x) = "(" ++ show x ++ ")"

instance Show Range where
  show Max       = "(max)"
  show (Sized r) = show r

instance Show Type where
  show _ = "type"

instance Show ColumnDefinitions where
  show (ColumnDefinitions xs) = concat $ intersperse ",\n" $ map show xs

instance Show ColumnDefinition where
  show c = show (column_name c) ++ " " ++ show (data_type c) ++
           nullConstraint
    where
      nullConstraint = maybe "" (\x -> if x then " NULL" else " NOT NULL") $ null_constraint c

instance Show TableDefinition where
  show t = "CREATE TABLE " ++ show (table_name t) ++
           "(" ++ show (column_definitions t) ++  ")"
