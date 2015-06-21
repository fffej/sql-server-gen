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

renderFixedRange :: FixedRange -> Doc
renderFixedRange (FixedRange n) = lparen <+> int n <+> rparen

instance Arbitrary FixedRange where
  arbitrary = liftM FixedRange (choose (1,8000))

-- See for example https://msdn.microsoft.com/en-us/library/ms176089.aspx
data Range = Sized FixedRange
           | Max

renderRange :: Range -> Doc
renderRange Max = text "(max)"
renderRange (Sized r) = renderFixedRange r

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

renderDataType :: Type -> Doc
renderDataType BigInt = text "bigint"
renderDataType Bit = text "bit"
renderDataType Numeric = text "numeric"
renderDataType SmallInt = text "smallint"
renderDataType Decimal = text "decimal"
renderDataType SmallMoney = text "smallmoney"
renderDataType Int = text "int"
renderDataType TinyInt = text "tinyint"
renderDataType Money = text "money"
renderDataType Float = text "float"
renderDataType Real = text "real"
renderDataType Date = text "date"
renderDataType DateTimeOffset = text "datetimeoffset"
renderDataType DateTime2 = text "datetime2"
renderDataType SmallDateTime = text "smalldatetime"
renderDataType DateTime = text "datetime"
renderDataType Time = text "time"
renderDataType (Char fixedRange _)  = text "char" <+> renderFixedRange fixedRange
renderDataType (VarChar range _) = text "varchar" <+> renderRange range
renderDataType (Text _) = text "text"
renderDataType (NChar _) = text "nchar"
renderDataType (NVarChar _) = text "nvarchar"
renderDataType (NText _) = text "ntext"
renderDataType (Binary fixedRange)  = text "binary" <+> renderFixedRange fixedRange
renderDataType (VarBinary range) = text "varbinary" <+> renderRange range
renderDataType Image = text "image"
renderDataType Cursor = text "cursor"
renderDataType Timestamp = text "timestamp"
renderDataType HierarchyId = text "hierarchyid"
renderDataType UniqueIdentifier = text "uniqueidentifier"
renderDataType SqlVariant = text "sqlvariant"
renderDataType Xml = text "xml"
renderDataType Table = text "table"
renderDataType Geography = text "geography"
renderDataType Geometry = text "geometry"


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

