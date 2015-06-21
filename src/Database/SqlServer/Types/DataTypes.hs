{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.DataTypes where

import Database.SqlServer.Types.Collations (collations, Collation)

import Test.QuickCheck
import Control.Monad 
import Data.DeriveTH


-- Size of arbitrary data (>= 1 && <= 8000)
newtype FixedRange = FixedRange Int

instance Show FixedRange where
  show (FixedRange x) = "(" ++ show x ++ ")"

instance Arbitrary FixedRange where
  arbitrary = liftM FixedRange (choose (1,8000))

-- See for example https://msdn.microsoft.com/en-us/library/ms176089.aspx
data Range = Sized FixedRange
           | Max

derive makeArbitrary ''Range

-- Only valid for var binary max
data FileStream = FileStream

instance Show Range where
  show Max       = "(max)"
  show (Sized r) = show r




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
          | Char FixedRange (Maybe Collation)
          | VarChar Range (Maybe Collation)
          | Text (Maybe Collation)
          | NChar (Maybe Collation)
          | NVarChar (Maybe Collation)
          | NText (Maybe Collation)
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
          | Geometry deriving (Show)

instance Arbitrary Type where
  arbitrary = do
    let possibilities = [ \f m r -> BigInt
                        , \f m r -> Bit
                        , \f m r -> Numeric
                        , \f m r -> SmallInt
                        , \f m r -> Decimal
                        , \f m r -> SmallMoney
                        , \f m r -> Int
                        , \f m r -> TinyInt
                        , \f m r -> Money
                        , \f m r -> Float
                        , \f m r -> Real
                        , \f m r -> Date
                        , \f m r -> DataTimeOffset
                        , \f m r -> DateTime2
                        , \f m r -> SmallDateTime
                        , \f m r -> DateTime
                        , \f m r -> Time
                        , \f m r -> Char f m
                        , \f m r -> VarChar r m
                        , \f m r -> Text m
                        , \f m r -> NChar m
                        , \f m r -> NVarChar m
                        , \f m r -> NText m
                        , \f m r -> Binary f
                        , \f m r -> VarBinary r -- FILESTREAM valid only for varbinary(max)
                        , \f m r -> Image
                        , \f m r -> Cursor
                        , \f m r -> Timestamp
                        , \f m r -> HierarchyId
                        , \f m r -> UniqueIdentifier
                        , \f m r -> SqlVariant
                        , \f m r -> Xml
                        , \f m r -> Table
                        , \f m r -> Geography
                        , \f m r -> Geometry ]
    constructor <- elements possibilities
    liftM3 constructor arbitrary arbitrary arbitrary 

