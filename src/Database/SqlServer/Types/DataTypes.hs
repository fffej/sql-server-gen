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


data VarBinaryStorage = SizedRange Range
                      | MaxNoFileStream
                      | MaxFileStream

renderFileStream :: VarBinaryStorage -> Doc
renderFileStream MaxFileStream = text "FILESTREAM"
renderFileStream _             = empty

renderVarBinaryStorage :: VarBinaryStorage -> Doc
renderVarBinaryStorage (SizedRange r)   = renderRange r
renderVarBinaryStorage MaxFileStream    = text "(max)"
renderVarBinaryStorage MaxNoFileStream  = text "(max)"

{- A sparse column must be nullable and cannot have the ROWGUIDCOL, IDENTITY, or FILESTREAM properties.
   A sparse column cannot be of the following data types: text, ntext, image, geometry, geography, or user-defined type. -}
data StorageOptions = Sparse
                    | SparseNull
                    | StorageOptions NullStorageOptions

data NullStorageOptions = NotNull | Null

nullStorageFromStorageOptions :: StorageOptions -> Maybe NullStorageOptions
nullStorageFromStorageOptions (StorageOptions x) = Just x
nullStorageFromStorageOptions _                  = Nothing

renderSparse :: StorageOptions -> Doc
renderSparse Sparse = text "SPARSE"
renderSparse _      = empty

renderNullConstraint ::  NullStorageOptions -> Doc
renderNullConstraint x = renderNullStorageOptions x

renderNullStorageOptions :: NullStorageOptions -> Doc
renderNullStorageOptions NotNull = text "NOT NULL"
renderNullStorageOptions Null    = text "NULL"

-- https://msdn.microsoft.com/en-us/library/ms187752.aspx
data Type = BigInt (Maybe StorageOptions) 
          | Bit (Maybe StorageOptions)
          | Numeric (Maybe StorageOptions)
          | SmallInt (Maybe StorageOptions)
          | Decimal (Maybe StorageOptions)
          | SmallMoney (Maybe StorageOptions)
          | Int (Maybe StorageOptions)
          | TinyInt (Maybe StorageOptions)
          | Money (Maybe StorageOptions)
          | Float (Maybe StorageOptions)
          | Real (Maybe StorageOptions)
          | Date (Maybe StorageOptions)
          | DateTimeOffset (Maybe StorageOptions)
          | DateTime2 (Maybe StorageOptions)
          | SmallDateTime (Maybe StorageOptions)
          | DateTime (Maybe StorageOptions)
          | Time (Maybe StorageOptions)
          | Char FixedRange (Maybe Collation) (Maybe StorageOptions)
          | VarChar Range (Maybe Collation) (Maybe StorageOptions)
          | Text (Maybe Collation) (Maybe NullStorageOptions)
          | NChar (Maybe Collation) (Maybe StorageOptions)
          | NVarChar (Maybe Collation) (Maybe StorageOptions)
          | NText (Maybe Collation) (Maybe NullStorageOptions)
          | Binary FixedRange (Maybe StorageOptions)
          | VarBinary VarBinaryStorage (Maybe StorageOptions)
          | Image (Maybe NullStorageOptions)
          | Timestamp (Maybe StorageOptions)
          | HierarchyId (Maybe StorageOptions)
          | UniqueIdentifier (Maybe StorageOptions)
          | SqlVariant (Maybe StorageOptions)
          | Xml (Maybe StorageOptions)
          | Geography (Maybe NullStorageOptions)
          | Geometry  (Maybe NullStorageOptions)

derive makeArbitrary ''StorageOptions
derive makeArbitrary ''Type
derive makeArbitrary ''Range
derive makeArbitrary ''VarBinaryStorage
derive makeArbitrary ''NullStorageOptions

collation :: Type -> Maybe Collation
collation (Char _ mc _)    = mc
collation (VarChar _ mc _) = mc
collation (Text mc _)      = mc
collation (NChar mc _)     = mc
collation (NVarChar mc _)  = mc
collation (NText mc _)     = mc
collation s              = Nothing

nullOptions :: Type -> Maybe NullStorageOptions
nullOptions t = maybe Nothing nullStorageFromStorageOptions (storageOptions t)

storageOptions :: Type -> Maybe StorageOptions
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
storageOptions (NChar _ s) = s
storageOptions (NVarChar _ s) = s
storageOptions (Binary _ s)  = s 
storageOptions (VarBinary _ s) = s
storageOptions (Timestamp s) = s 
storageOptions (HierarchyId s) = s
storageOptions (UniqueIdentifier s) = s
storageOptions (SqlVariant s) = s
storageOptions (Xml s) = s
storageOptions (Text _ s) = Nothing
storageOptions (NText _ _) = Nothing
storageOptions (Image _) = Nothing
storageOptions (Geography s) = Nothing
storageOptions (Geometry s) = Nothing


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

