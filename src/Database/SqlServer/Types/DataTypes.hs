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
          | DateTimeOffset
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
          | Geometry 


render_collation :: Type -> String
render_collation (Char _ mc)    = maybe "" show mc
render_collation (VarChar _ mc) = maybe "" show mc
render_collation (Text mc)      = maybe "" show mc
render_collation (NChar mc)     = maybe "" show mc
render_collation (NVarChar mc)  = maybe "" show mc
render_collation (NText mc)     = maybe "" show mc
render_collation _              = ""

render_data_type :: Type -> String
render_data_type BigInt = "bigint"
render_data_type Bit = "bit"
render_data_type Numeric = "numeric"
render_data_type SmallInt = "smallint"
render_data_type Decimal = "decimal"
render_data_type SmallMoney = "smallmoney"
render_data_type Int = "int"
render_data_type TinyInt = "tinyint"
render_data_type Money = "money"
render_data_type Float = "float"
render_data_type Real = "real"
render_data_type Date = "date"
render_data_type DateTimeOffset = "datetimeoffset"
render_data_type DateTime2 = "datetime2"
render_data_type SmallDateTime = "smalldatetime"
render_data_type DateTime = "datetime"
render_data_type Time = "time"
render_data_type (Char fixedRange _)  = "char" ++ show fixedRange
render_data_type (VarChar range _) = "varchar" ++ show range
render_data_type (Text _) = "text"
render_data_type (NChar _) = "nchar"
render_data_type (NVarChar _) = "nvarchar"
render_data_type (NText _) = "ntext"
render_data_type (Binary fixedRange)  = "binary" ++ show fixedRange
render_data_type (VarBinary range) = "varbinary" ++ show range
render_data_type Image = "image"
render_data_type Cursor = "cursor"
render_data_type Timestamp = "timestamp"
render_data_type HierarchyId = "hierarchyid"
render_data_type UniqueIdentifier = "uniqueidentifier"
render_data_type SqlVariant = "sqlvariant"
render_data_type Xml = "xml"
render_data_type Table = "table"
render_data_type Geography = "geography"
render_data_type Geometry = "geometry"


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
                        , \f m r -> DateTimeOffset
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

