{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.DataTypes where

import Database.SqlServer.Types.Collations (Collation)

import Text.PrettyPrint

import Test.QuickCheck hiding (scale)
import Control.Monad 
import Data.DeriveTH


-- Size of arbitrary data (>= 1 && <= 8000)
newtype FixedRange = FixedRange Int

renderFixedRange :: FixedRange -> Doc
renderFixedRange (FixedRange n) = lparen <> int n <> rparen

fixedRangeStorage :: FixedRange -> Int
fixedRangeStorage (FixedRange n) = n * 8

instance Arbitrary FixedRange where
  arbitrary = liftM FixedRange (choose (1,8000))

newtype NFixedRange = NFixedRange Int

renderNFixedRange :: NFixedRange -> Doc
renderNFixedRange (NFixedRange n) = lparen <> int n <> rparen

nfixedRangeStorage :: NFixedRange -> Int
nfixedRangeStorage (NFixedRange n) = n * 8

instance Arbitrary NFixedRange where
  arbitrary = liftM NFixedRange (choose (1,4000))

-- See for example https://msdn.microsoft.com/en-us/library/ms176089.aspx
data Range = Sized FixedRange
           | Max

renderRange :: Range -> Doc
renderRange Max = text "(max)"
renderRange (Sized r) = renderFixedRange r

data NRange = NSized NFixedRange
            | NMax

renderNRange :: NRange -> Doc
renderNRange NMax = text "(max)"
renderNRange (NSized r) = renderNFixedRange r


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
renderNumericStorage ns = lparen <> int (precision ns) <> scale' <> rparen
  where
    scale' = maybe empty (\x -> comma <+> int x) (scale ns)

{- The scale must be less than or equal to the precision -}
instance Arbitrary NumericStorage where                      
  arbitrary = do
    p <- choose(1,38)
    s     <- elements (Nothing : map Just [1..p])
    return $ NumericStorage p s

data PrecisionStorage = PrecisionStorage Int

instance Arbitrary PrecisionStorage where
  arbitrary = do
    precision <- choose(1,53)
    return (PrecisionStorage precision)

renderPrecisionStorage :: PrecisionStorage -> Doc
renderPrecisionStorage (PrecisionStorage n) = lparen <> int n <> rparen

data FractionalSecondsPrecision = FractionalSecondsPrecision Int

instance Arbitrary FractionalSecondsPrecision where
  arbitrary = do
    precision <- choose (0,7)
    return (FractionalSecondsPrecision precision)

renderFractionalSecondsPrecision :: FractionalSecondsPrecision -> Doc
renderFractionalSecondsPrecision (FractionalSecondsPrecision n) = lparen <> int n <> rparen

-- https://msdn.microsoft.com/en-us/library/bb677335.aspx
datetime2StorageSize :: FractionalSecondsPrecision -> Int
datetime2StorageSize (FractionalSecondsPrecision p)
  | p < 3            = 6 * 8
  | p == 3 || p == 4 = 7 * 8
  | otherwise        = 8

dateTimeOffsetStorageSize :: FractionalSecondsPrecision -> Int
dateTimeOffsetStorageSize (FractionalSecondsPrecision n)
  | n < 3     = 8 * 8
  | n < 5     = 9 * 8
  | otherwise = 10 * 8

timeStorageSize :: FractionalSecondsPrecision -> Int
timeStorageSize (FractionalSecondsPrecision n)
  | n < 3 = 3 * 8
  | n < 5 = 4 * 8
  | otherwise = 5

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
          | DateTimeOffset (Maybe StorageOptions) (Maybe FractionalSecondsPrecision)
          | DateTime2 (Maybe StorageOptions) (Maybe FractionalSecondsPrecision)
          | SmallDateTime (Maybe StorageOptions)
          | DateTime (Maybe StorageOptions)
          | Time (Maybe StorageOptions) (Maybe FractionalSecondsPrecision)
          | Char (Maybe FixedRange) (Maybe Collation) (Maybe StorageOptions)
          | VarChar Range (Maybe Collation) (Maybe StorageOptions)
          | Text (Maybe Collation) (Maybe NullStorageOptions)
          | NChar (Maybe NFixedRange) (Maybe Collation) (Maybe StorageOptions)
          | NVarChar (Maybe NRange) (Maybe Collation) (Maybe StorageOptions)
          | NText (Maybe Collation) (Maybe NullStorageOptions)
          | Binary (Maybe FixedRange) (Maybe StorageOptions)
          | VarBinary (Maybe VarBinaryStorage) (Maybe StorageOptions)
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
derive makeArbitrary ''NRange
derive makeArbitrary ''VarBinaryStorage
derive makeArbitrary ''NullStorageOptions

collation :: Type -> Maybe Collation
collation (Char _ mc _)    = mc
collation (VarChar _ mc _) = mc
collation (Text mc _)      = mc
collation (NChar _ mc _)   = mc
collation (NVarChar _ mc _)= mc
collation (NText mc _)     = mc
collation _                = Nothing

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
-- Based on http://dba.stackexchange.com/questions/66471/script-to-estimate-row-sizes-for-any-table
-- and MSDN documentation
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
storageSize (Date _) = 3
storageSize (DateTime _ ) = 8 * 8
storageSize (DateTime2 _ p) = maybe (8 * 8) datetime2StorageSize p -- default is 8 bytes
storageSize (DateTimeOffset _ p) = maybe (10 * 8) dateTimeOffsetStorageSize p
storageSize (SmallDateTime _) = 4 * 8
storageSize (Time _ p) = maybe (5 * 8) timeStorageSize p
storageSize (Char fr _ _) = maybe 8 fixedRangeStorage fr
storageSize (NChar fr _ _) = maybe 8 nfixedRangeStorage fr
storageSize (Binary p _) = maybe (1 * 8) fixedRangeStorage p
storageSize (UniqueIdentifier _) = 16 * 8
storageSize (VarBinary _ _) = 0 -- assumption 
storageSize (VarChar _ _ _) = 0 -- assumption
storageSize (NVarChar _ _ _) = 0 -- assumption
storageSize (Text _ _) = 0 -- assumption
storageSize (NText _ _) = 0 -- assumption
storageSize (Image _) = 0 -- assumption
storageSize (Timestamp _) = 5 * 8
storageSize (HierarchyId _) = 0 -- assumption
storageSize (Geometry _) = 0
storageSize (Geography _) = 0
storageSize (SqlVariant _) = 0
storageSize (Xml _) = 0


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
storageOptions (DateTimeOffset s _) = s
storageOptions (DateTime2 s _) = s
storageOptions (SmallDateTime s) = s
storageOptions (DateTime s) = s
storageOptions (Time s _) = s
storageOptions (Char _ _ s)  = s
storageOptions (VarChar _ _ s) = s
storageOptions (NChar _ _ s) = s
storageOptions (NVarChar _ _ s) = s
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
renderDataType (DateTimeOffset _ p) = text "datetimeoffset" <+> maybe empty renderFractionalSecondsPrecision p
renderDataType (DateTime2 _ p) = text "datetime2" <+> maybe empty renderFractionalSecondsPrecision p
renderDataType (SmallDateTime _) = text "smalldatetime"
renderDataType (DateTime _) = text "datetime"
renderDataType (Time _ p)= text "time" <+> maybe empty renderFractionalSecondsPrecision p
renderDataType (Char fixedRange _ _)  = text "char" <+> maybe empty renderFixedRange fixedRange
renderDataType (VarChar range _ _) = text "varchar" <+> renderRange range
renderDataType (Text _ _) = text "text"
renderDataType (NChar p _ _) = text "nchar" <+> maybe empty renderNFixedRange p
renderDataType (NVarChar p _ _) = text "nvarchar" <+> maybe empty renderNRange p
renderDataType (NText _ _) = text "ntext"
renderDataType (Binary fixedRange _)  = text "binary" <+> maybe empty renderFixedRange fixedRange
renderDataType (VarBinary range _) = text "varbinary" <+> maybe empty renderVarBinaryStorage range
renderDataType (Image _) = text "image"
renderDataType (Timestamp _) = text "timestamp"
renderDataType (HierarchyId _) = text "hierarchyid"
renderDataType (UniqueIdentifier _) = text "uniqueidentifier"
renderDataType (SqlVariant _) = text "sql_variant"
renderDataType (Xml _) = text "xml"
renderDataType (Geography _) = text "geography"
renderDataType (Geometry _) =  text "geometry"

