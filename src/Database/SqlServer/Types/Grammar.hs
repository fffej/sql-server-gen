{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.Grammar where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad

import Data.DeriveTH

-- https://msdn.microsoft.com/en-us/subscriptions/downloads/ms175874
newtype SymbolName = SymbolName String 

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
            deriving (Show)

-- https://msdn.microsoft.com/en-us/library/ms174979.aspx
data TableDefinition = TableDefinition
             {
               table_name    :: SymbolName
             , column_definitions :: [ColumnDefinition]
             }

data ColumnDefinition = ColumnDefinition
                        {
                          column_name :: SymbolName
                        , data_type   :: Type
                        }

instance Arbitrary FixedRange where
  arbitrary = liftM FixedRange (choose (1,8000))

instance Show FixedRange where
  show (FixedRange x) = "(" ++ show x ++ ")"

instance Arbitrary Range where
  arbitrary = oneof [return Max,liftM Sized arbitrary]

instance Show Range where
  show Max       = "(max)"
  show (Sized r) = show r

derive makeArbitrary ''Type
