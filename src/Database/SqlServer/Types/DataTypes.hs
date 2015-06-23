{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.DataTypes where

import Database.SqlServer.Types.Collations (collations, Collation)

import Text.PrettyPrint

import Test.QuickCheck hiding (scale)
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

data NumericStorage = NumericStorage
                      {
                        precision :: Int
                      , scale :: Maybe Int 
                      }

renderNumericStorage :: NumericStorage -> Doc
renderNumericStorage ns = lparen <+> int (precision ns) <+> scale' <+> rparen
  where
    scale' = maybe empty (\x -> comma <+> int x) (scale ns)

{- The scale must be less than or equal to the precision -}
instance Arbitrary NumericStorage where                      
  arbitrary = do
    precision <- choose(1,38)
    scale     <- elements (Nothing : map Just [1..precision])
    return $ NumericStorage precision scale

data PrecisionStorage = PrecisionStorage Int

instance Arbitrary PrecisionStorage where
  arbitrary = do
    precision <- choose(1,53)
    return (PrecisionStorage precision)

renderPrecisionStorage :: PrecisionStorage -> Doc
renderPrecisionStorage (PrecisionStorage n) = lparen <+> int n <+> rparen
    
-- https://msdn.microsoft.com/en-us/library/ms187752.aspx
data Type = BigInt (Maybe StorageOptions) 
          | Bit (Maybe StorageOptions)
          | Numeric (Maybe StorageOptions) (Maybe NumericStorage)
          | SmallInt (Maybe StorageOptions) 
          | Decimal (Maybe StorageOptions) (Maybe NumericStorage)
          | SmallMoney (Maybe StorageOptions)
          | Int (Maybe StorageOptions)
          | TinyInt (Maybe StorageOptions)
          | Money (Maybe StorageOptions)
          | Float (Maybe StorageOptions) (Maybe PrecisionStorage)
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
          | Timestamp (Maybe NullStorageOptions)
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

numericStorageSize :: NumericStorage -> Int
numericStorageSize x 
  | precision x <= 9 = 5 * 8
  | precision x <= 19 = 9 * 8 
  | precision x <= 28 = 13 * 8
  | precision x <= 38 = 17 * 8
  | otherwise   = error $ "Failed to calculate numeric storage size (" ++ show (precision x) ++ ")"

precisionStorageSize :: PrecisionStorage -> Int
precisionStorageSize (PrecisionStorage x)
  | x <= 24 = 4 * 8
  | x<= 53 = 8 * 8
  | otherwise = error $ "Failed to calculate precision storage size (" ++ show x ++ ")"

-- storage size in bits
storageSize :: Type -> Int
storageSize (BigInt _) = 8 * 8
storageSize (Int _)  = 4 * 8
storageSize (SmallInt _) = 2 * 8
storageSize (TinyInt _) = 1 * 8
storageSize (Bit _)     = 1
storageSize (SmallMoney _) = 4 * 8
storageSize (Money _) = 8 * 8
storageSize (Numeric _ ns) = maybe (9 * 8) numericStorageSize ns -- default precision is 18
storageSize (Decimal _ ns) = maybe (9 * 8) numericStorageSize ns -- default precision is 18
storageSize (Float _ ps) = maybe (8 * 8) precisionStorageSize ps -- default precision is 53
storageSize (Real _) = 4 * 8


nullOptions :: Type -> Maybe NullStorageOptions
nullOptions t = maybe Nothing nullStorageFromStorageOptions (storageOptions t)

storageOptions :: Type -> Maybe StorageOptions
storageOptions (BigInt s) = s
storageOptions (Bit s) = s
storageOptions (Numeric s _) = s
storageOptions (SmallInt s) = s
storageOptions (Decimal s _) = s
storageOptions (SmallMoney s) = s
storageOptions (Int s) = s
storageOptions (TinyInt s) = s
storageOptions (Money s) = s
storageOptions (Float s _) = s
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
storageOptions (HierarchyId s) = s
storageOptions (UniqueIdentifier s) = s
storageOptions (SqlVariant s) = s
storageOptions (Xml s) = s
storageOptions (Timestamp _) = Nothing
storageOptions (Text _ _) = Nothing
storageOptions (NText _ _) = Nothing
storageOptions (Image _) = Nothing
storageOptions (Geography _) = Nothing
storageOptions (Geometry _) = Nothing


renderDataType :: Type -> Doc
renderDataType (BigInt _) = text "bigint"
renderDataType (Bit _) = text "bit"
renderDataType (Numeric _ ns) = text "numeric" <+> maybe empty renderNumericStorage ns
renderDataType (SmallInt _) = text "smallint"
renderDataType (Decimal _ ns) = text "decimal" <+> maybe empty renderNumericStorage ns
renderDataType (SmallMoney _) = text "smallmoney"
renderDataType (Int _) = text "int"
renderDataType (TinyInt _) = text "tinyint"
renderDataType (Money _) = text "money"
renderDataType (Float _ ps) = text "float" <+> maybe empty renderPrecisionStorage ps
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

