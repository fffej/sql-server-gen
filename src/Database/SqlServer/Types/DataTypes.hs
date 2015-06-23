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

data VarBinaryStorage = SizedRange Range
                      | MaxNoFileStream
                      | MaxFileStream

derive makeArbitrary ''VarBinaryStorage

renderFileStream :: VarBinaryStorage -> Doc
renderFileStream MaxFileStream = text "FILESTREAM"
renderFileStream _             = empty

renderVarBinaryStorage :: VarBinaryStorage -> Doc
renderVarBinaryStorage (SizedRange r)   = renderRange r
renderVarBinaryStorage MaxFileStream    = text "(max)"
renderVarBinaryStorage MaxNoFileStream  = text "(max)"

{- A sparse column must be nullable and cannot have the ROWGUIDCOL, IDENTITY, or FILESTREAM properties.
   A sparse column cannot be of the following data types: text, ntext, image, geometry, geography, or user-defined type. -}
data SparseStorageOptions = Sparse
                    | SparseNull
                    | NotNull
                    | Null

renderSparse :: SparseStorageOptions -> Doc
renderSparse Sparse = text "SPARSE"
renderSparse _      = empty

renderNullConstraint :: SparseStorageOptions -> Doc
renderNullConstraint NotNull = text "NOT NULL"
renderNullConstraint Null    = text "NULL"
renderNullConstraint _       = empty

-- https://msdn.microsoft.com/en-us/library/ms187752.aspx
data Type = BigInt (Maybe SparseStorageOptions) 
          | Bit (Maybe SparseStorageOptions)
          | Numeric (Maybe SparseStorageOptions)
          | SmallInt (Maybe SparseStorageOptions)
          | Decimal (Maybe SparseStorageOptions)
          | SmallMoney (Maybe SparseStorageOptions)
          | Int (Maybe SparseStorageOptions)
          | TinyInt (Maybe SparseStorageOptions)
          | Money (Maybe SparseStorageOptions)
          | Float (Maybe SparseStorageOptions)
          | Real (Maybe SparseStorageOptions)
          | Date (Maybe SparseStorageOptions)
          | DateTimeOffset (Maybe SparseStorageOptions)
          | DateTime2 (Maybe SparseStorageOptions)
          | SmallDateTime (Maybe SparseStorageOptions)
          | DateTime (Maybe SparseStorageOptions)
          | Time (Maybe SparseStorageOptions)
          | Char FixedRange (Maybe Collation) (Maybe SparseStorageOptions)
          | VarChar Range (Maybe Collation) (Maybe SparseStorageOptions)
          | Text (Maybe Collation) (Maybe SparseStorageOptions)
          | NChar (Maybe Collation) (Maybe SparseStorageOptions)
          | NVarChar (Maybe Collation) (Maybe SparseStorageOptions)
          | NText (Maybe Collation) (Maybe SparseStorageOptions)
          | Binary FixedRange (Maybe SparseStorageOptions)
          | VarBinary VarBinaryStorage (Maybe SparseStorageOptions)
          | Image (Maybe SparseStorageOptions)
          | Timestamp (Maybe SparseStorageOptions)
          | HierarchyId (Maybe SparseStorageOptions)
          | UniqueIdentifier (Maybe SparseStorageOptions)
          | SqlVariant (Maybe SparseStorageOptions)
          | Xml (Maybe SparseStorageOptions)
          | Geography (Maybe SparseStorageOptions)
          | Geometry  (Maybe SparseStorageOptions)

derive makeArbitrary ''SparseStorageOptions
derive makeArbitrary ''Type

collation :: Type -> Maybe Collation
collation (Char _ mc _)    = mc
collation (VarChar _ mc _) = mc
collation (Text mc _)      = mc
collation (NChar mc _)     = mc
collation (NVarChar mc _)  = mc
collation (NText mc _)     = mc
collation s              = Nothing

storageOptions :: Type -> Maybe SparseStorageOptions
storageOptions (BigInt s) = s
storageOptions (Bit s) = s
storageOptions (Numeric s) = s
storageOptions (SmallInt s) = s
storageOptions (Decimal s) = s
storageOptions (SmallMoney s) = s
storageOptions (Int s) = s
storageOptions (TinyInt s) = s
storageOptions (Money s) = s
storageOptions (Float s) = s
storageOptions (Real s) = s
storageOptions (Date s) = s
storageOptions (DateTimeOffset s) = s
storageOptions (DateTime2 s) = s
storageOptions (SmallDateTime s) = s
storageOptions (DateTime s) = s
storageOptions (Time s) = s
storageOptions (Char _ _ s)  = s
storageOptions (VarChar _ _ s) = s
storageOptions (Text _ s) = s
storageOptions (NChar _ s) = s
storageOptions (NVarChar _ s) = s
storageOptions (NText _ s) = s 
storageOptions (Binary _ s)  = s 
storageOptions (VarBinary _ s) = s
storageOptions (Image s) = s
storageOptions (Timestamp s) = s 
storageOptions (HierarchyId s) = s
storageOptions (UniqueIdentifier s) = s
storageOptions (SqlVariant s) = s
storageOptions (Xml s) = s
storageOptions (Geography s) = s
storageOptions (Geometry s) = s


renderDataType :: Type -> Doc
renderDataType (BigInt _) = text "bigint"
renderDataType (Bit _) = text "bit"
renderDataType (Numeric _) = text "numeric"
renderDataType (SmallInt _) = text "smallint"
renderDataType (Decimal _) = text "decimal"
renderDataType (SmallMoney _) = text "smallmoney"
renderDataType (Int _) = text "int"
renderDataType (TinyInt _) = text "tinyint"
renderDataType (Money _) = text "money"
renderDataType (Float _) = text "float"
renderDataType (Real _) = text "real"
renderDataType (Date _) = text "date"
renderDataType (DateTimeOffset _) = text "datetimeoffset"
renderDataType (DateTime2 _) = text "datetime2"
renderDataType (SmallDateTime _) = text "smalldatetime"
renderDataType (DateTime _) = text "datetime"
renderDataType (Time _)= text "time"
renderDataType (Char fixedRange _ _)  = text "char" <+> renderFixedRange fixedRange
renderDataType (VarChar range _ _) = text "varchar" <+> renderRange range
renderDataType (Text _ _) = text "text"
renderDataType (NChar _ _) = text "nchar"
renderDataType (NVarChar _ _) = text "nvarchar"
renderDataType (NText _ _) = text "ntext"
renderDataType (Binary fixedRange _)  = text "binary" <+> renderFixedRange fixedRange
renderDataType (VarBinary range _) = text "varbinary" <+> renderVarBinaryStorage range
renderDataType (Image _) = text "image"
renderDataType (Timestamp _) = text "timestamp"
renderDataType (HierarchyId _) = text "hierarchyid"
renderDataType (UniqueIdentifier _) = text "uniqueidentifier"
renderDataType (SqlVariant _) = text "sql_variant"
renderDataType (Xml _) = text "xml"
renderDataType (Geography _) = text "geography"
renderDataType (Geometry _) =  text "geometry"

