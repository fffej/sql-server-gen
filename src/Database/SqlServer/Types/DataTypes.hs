{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.DataTypes where

import Database.SqlServer.Types.Collations (collations, Collation)

import Text.PrettyPrint

import Test.QuickCheck
import Control.Monad 
import Data.DeriveTH


-- Size of arbitrary data (>= 1 && <= 8000)
newtype FixedRange = FixedRange Int

render_fixed_range :: FixedRange -> Doc
render_fixed_range (FixedRange n) = lparen <+> int n <+> rparen

instance Arbitrary FixedRange where
  arbitrary = liftM FixedRange (choose (1,8000))

-- See for example https://msdn.microsoft.com/en-us/library/ms176089.aspx
data Range = Sized FixedRange
           | Max

render_range :: Range -> Doc
render_range Max = text "(max)"
render_range (Sized r) = render_fixed_range r

derive makeArbitrary ''Range

-- Only valid for var binary max
data FileStream = FileStream

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


collation :: Type -> Maybe Collation
collation (Char _ mc)    = mc
collation (VarChar _ mc) = mc
collation (Text mc)      = mc
collation (NChar mc)     = mc
collation (NVarChar mc)  = mc
collation (NText mc)     = mc
collation _              = Nothing

render_data_type :: Type -> Doc
render_data_type BigInt = text "bigint"
render_data_type Bit = text "bit"
render_data_type Numeric = text "numeric"
render_data_type SmallInt = text "smallint"
render_data_type Decimal = text "decimal"
render_data_type SmallMoney = text "smallmoney"
render_data_type Int = text "int"
render_data_type TinyInt = text "tinyint"
render_data_type Money = text "money"
render_data_type Float = text "float"
render_data_type Real = text "real"
render_data_type Date = text "date"
render_data_type DateTimeOffset = text "datetimeoffset"
render_data_type DateTime2 = text "datetime2"
render_data_type SmallDateTime = text "smalldatetime"
render_data_type DateTime = text "datetime"
render_data_type Time = text "time"
render_data_type (Char fixedRange _)  = text "char" <+> render_fixed_range fixedRange
render_data_type (VarChar range _) = text "varchar" <+> render_range range
render_data_type (Text _) = text "text"
render_data_type (NChar _) = text "nchar"
render_data_type (NVarChar _) = text "nvarchar"
render_data_type (NText _) = text "ntext"
render_data_type (Binary fixedRange)  = text "binary" <+> render_fixed_range fixedRange
render_data_type (VarBinary range) = text "varbinary" <+> render_range range
render_data_type Image = text "image"
render_data_type Cursor = text "cursor"
render_data_type Timestamp = text "timestamp"
render_data_type HierarchyId = text "hierarchyid"
render_data_type UniqueIdentifier = text "uniqueidentifier"
render_data_type SqlVariant = text "sqlvariant"
render_data_type Xml = text "xml"
render_data_type Table = text "table"
render_data_type Geography = text "geography"
render_data_type Geometry = text "geometry"


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

