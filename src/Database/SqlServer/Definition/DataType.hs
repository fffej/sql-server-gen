{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Definition.DataType
       (
         Type
       , renderDataType
       , collation
       , renderSparse
       , storageOptions
       , rowGuidOptions
       , storageSize
       , renderRowGuidConstraint
       , isRowGuidCol
       , nullOptions
       , renderNullConstraint
       , isTimestamp
       , isSupportedTypeForPartitionFunction
       , value
       ) where

import Database.SqlServer.Definition.Value 
import Database.SqlServer.Definition.Collation (Collation)

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
nfixedRangeStorage (NFixedRange n) = n * 8 * 2

instance Arbitrary NFixedRange where
  arbitrary = liftM NFixedRange (choose (1,4000))

-- See for example https://msdn.microsoft.com/en-us/library/ms176089.aspx
data Range = Sized FixedRange
           | Max

rangeSize :: Range -> Int
rangeSize Max = 8000
rangeSize (Sized (FixedRange n)) = n

rangeStorageSize :: Range -> Int
rangeStorageSize (Sized x) = fixedRangeStorage x
rangeStorageSize _         = 0 

renderRange :: Range -> Doc
renderRange Max = text "(max)"
renderRange (Sized r) = renderFixedRange r

data NRange = NSized NFixedRange
            | NMax

nRangeSize :: NRange -> Int
nRangeSize NMax = 4000
nRangeSize (NSized (NFixedRange n)) = n

nRangeStorageSize :: NRange -> Int
nRangeStorageSize (NSized x) = nfixedRangeStorage x
nRangeStorageSize _          = 0

renderNRange :: NRange -> Doc
renderNRange NMax = text "(max)"
renderNRange (NSized r) = renderNFixedRange r

data VarBinaryStorage = SizedRange Range
                      | MaxNoFileStream
                      | MaxFileStream

renderVarBinaryStorage :: VarBinaryStorage -> Doc
renderVarBinaryStorage (SizedRange r)   = renderRange r
renderVarBinaryStorage MaxFileStream    = text "(max)"
renderVarBinaryStorage MaxNoFileStream  = text "(max)"

varBinarySize :: VarBinaryStorage -> Int
varBinarySize (SizedRange r) = rangeStorageSize r
varBinarySize _              = 0 

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
renderNullConstraint = renderNullStorageOptions

renderNullStorageOptions :: NullStorageOptions -> Doc
renderNullStorageOptions NotNull = text "NOT NULL"
renderNullStorageOptions Null    = text "NULL"

-- Row Guid Col (cannot be sparse)
data UniqueIdentifierOptions = RowGuidCol (Maybe NullStorageOptions)
                             | UniqueIdentifierOptions (Maybe StorageOptions)

uniqueIdentifiertorageOptions :: UniqueIdentifierOptions -> Maybe StorageOptions
uniqueIdentifiertorageOptions (UniqueIdentifierOptions x) = x
uniqueIdentifiertorageOptions (RowGuidCol x) = fmap StorageOptions x

isRowGuidCol :: UniqueIdentifierOptions -> Bool
isRowGuidCol (RowGuidCol _) = True
isRowGuidCol _              = False

renderRowGuidConstraint :: Bool -> Doc
renderRowGuidConstraint True  = text "ROWGUIDCOL"
renderRowGuidConstraint False = empty

data NumericStorage = NumericStorage
                      {
                        precision :: Int
                      , scale :: Maybe Int 
                      }

{- The scale must be less than or equal to the precision -}
instance Arbitrary NumericStorage where                      
  arbitrary = do
    p <- choose(1,38)
    s <- elements (Nothing : map Just [1..p])
    return $ NumericStorage p s

renderNumericStorage :: NumericStorage -> Doc
renderNumericStorage ns = lparen <> int (precision ns) <> scale' <> rparen
  where
    scale' = maybe empty (\x -> comma <+> int x) (scale ns)

renderFractionalSecondsPrecision :: FractionalSecondsPrecision -> Doc
renderFractionalSecondsPrecision (FractionalSecondsPrecision n) = lparen <> int n <> rparen

-- https://msdn.microsoft.com/en-us/library/bb677335.aspx
datetime2StorageSize :: FractionalSecondsPrecision -> Int
datetime2StorageSize (FractionalSecondsPrecision p)
  | p < 3            = 6 * 8
  | p == 3 || p == 4 = 7 * 8
  | otherwise        = 8 * 8

dateTimeOffsetStorageSize :: FractionalSecondsPrecision -> Int
dateTimeOffsetStorageSize (FractionalSecondsPrecision n)
  | n < 3     = 8 * 8
  | n < 5     = 9 * 8
  | otherwise = 10 * 8

timeStorageSize :: FractionalSecondsPrecision -> Int
timeStorageSize (FractionalSecondsPrecision n)
  | n < 3     = 3 * 8
  | n < 5     = 4 * 8
  | otherwise = 5 * 8

data PrecisionStorage = PrecisionStorage Int

instance Arbitrary PrecisionStorage where
  arbitrary = do
    p <- choose(1,53)
    return (PrecisionStorage p)

renderPrecisionStorage :: PrecisionStorage -> Doc
renderPrecisionStorage (PrecisionStorage n) = lparen <> int n <> rparen

data FractionalSecondsPrecision = FractionalSecondsPrecision Int

instance Arbitrary FractionalSecondsPrecision where
  arbitrary = do
    p <- choose (0,7)
    return (FractionalSecondsPrecision p)

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
          | UniqueIdentifier (Maybe UniqueIdentifierOptions) 
          | SqlVariant (Maybe StorageOptions) 
          | Xml (Maybe StorageOptions) 
          | Geography (Maybe NullStorageOptions) 
          | Geometry  (Maybe NullStorageOptions) 

isTimestamp :: Type -> Bool
isTimestamp (Timestamp _) = True
isTimestamp _             = False

derive makeArbitrary ''StorageOptions
derive makeArbitrary ''Type
derive makeArbitrary ''Range
derive makeArbitrary ''NRange
derive makeArbitrary ''VarBinaryStorage
derive makeArbitrary ''NullStorageOptions
derive makeArbitrary ''UniqueIdentifierOptions

collation :: Type -> Maybe Collation
collation (Char _ mc _)     = mc
collation (VarChar _ mc _)  = mc
collation (Text mc _)         = mc
collation (NChar _ mc _)    = mc
collation (NVarChar _ mc _) = mc
collation (NText mc _)        = mc
collation _                   = Nothing

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
storageSize BigInt {} = 8 * 8
storageSize Int {}  = 4 * 8
storageSize SmallInt {} = 2 * 8
storageSize TinyInt {} = 1 * 8
storageSize Bit {}    = 1
storageSize SmallMoney {} = 4 * 8
storageSize Money {} = 8 * 8
storageSize (Numeric _ ns) = maybe (9 * 8) numericStorageSize ns -- default precision is 18
storageSize (Decimal _ ns) = maybe (9 * 8) numericStorageSize ns -- default precision is 18
storageSize (Float _ ps) = maybe (8 * 8) precisionStorageSize ps -- default precision is 53
storageSize Real {} = 4 * 8
storageSize Date {} = 3 * 8
storageSize DateTime {} = 8 * 8
storageSize (DateTime2 _ p) = maybe (8 * 8) datetime2StorageSize p -- default is 8 bytes
storageSize (DateTimeOffset _ p) = maybe (10 * 8) dateTimeOffsetStorageSize p
storageSize (SmallDateTime _) = 4 * 8
storageSize (Time _ p) = maybe (5 * 8) timeStorageSize p
storageSize (Char fr _ _) = maybe 8 fixedRangeStorage fr
storageSize (NChar fr _ _) = maybe 8 nfixedRangeStorage fr
storageSize (Binary p _) = maybe (1 * 8) fixedRangeStorage p
storageSize (UniqueIdentifier _) = 16 * 8
storageSize (VarBinary r _) = maybe (1 * 8) varBinarySize r
storageSize (VarChar r _ _) = rangeStorageSize r 
storageSize (NVarChar r _ _) = maybe (1 * 8) nRangeStorageSize r
storageSize Text {} = 0 -- assumption
storageSize NText {} = 0 -- assumption
storageSize Image {} = 0 -- assumption
storageSize Timestamp {} = 5 * 8
storageSize HierarchyId {} = 0 -- assumption
storageSize Geometry {} = 0
storageSize Geography {} = 0
storageSize SqlVariant {} = 0
storageSize Xml {} = 0


nullOptions :: Type -> Maybe NullStorageOptions
nullOptions t = maybe Nothing nullStorageFromStorageOptions (storageOptions t)

rowGuidOptions :: Type -> Bool
rowGuidOptions (UniqueIdentifier a) = maybe False isRowGuidCol a
rowGuidOptions _                      = False

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
storageOptions (UniqueIdentifier s) = maybe Nothing uniqueIdentifiertorageOptions s
storageOptions (SqlVariant s) = s
storageOptions (Xml s) = s
storageOptions (Timestamp _) = Nothing
storageOptions (Text _ _) = Nothing
storageOptions (NText _ _) = Nothing
storageOptions (Image _) = Nothing
storageOptions (Geography _) = Nothing
storageOptions (Geometry _) = Nothing

renderDataType :: Type -> Doc
renderDataType BigInt {} = text "bigint"
renderDataType Bit {} = text "bit"
renderDataType (Numeric _ ns) = text "numeric" <> maybe empty renderNumericStorage ns
renderDataType SmallInt {} = text "smallint"
renderDataType (Decimal _ ns) = text "decimal" <> maybe empty renderNumericStorage ns
renderDataType SmallMoney {}= text "smallmoney"
renderDataType Int {} = text "int"
renderDataType (TinyInt _) = text "tinyint"
renderDataType (Money _) = text "money"
renderDataType (Float _ ps ) = text "float" <> maybe empty renderPrecisionStorage ps
renderDataType (Real _) = text "real"
renderDataType (Date _) = text "date"
renderDataType (DateTimeOffset _ p) = text "datetimeoffset" <> maybe empty renderFractionalSecondsPrecision p
renderDataType (DateTime2 _ p) = text "datetime2" <> maybe empty renderFractionalSecondsPrecision p
renderDataType SmallDateTime {} = text "smalldatetime"
renderDataType DateTime {} = text "datetime"
renderDataType (Time _ p)= text "time" <> maybe empty renderFractionalSecondsPrecision p
renderDataType (Char fixedRange _ _)  = text "char" <> maybe empty renderFixedRange fixedRange
renderDataType (VarChar range _ _) = text "varchar" <> renderRange range
renderDataType (Text _ _) = text "text"
renderDataType (NChar p _ _) = text "nchar" <> maybe empty renderNFixedRange p
renderDataType (NVarChar p _ _) = text "nvarchar" <> maybe empty renderNRange p
renderDataType NText {} = text "ntext"
renderDataType (Binary fixedRange _)  = text "binary" <> maybe empty renderFixedRange fixedRange
renderDataType (VarBinary range _) = text "varbinary" <> maybe empty renderVarBinaryStorage range
renderDataType Image {} = text "image"
renderDataType Timestamp {} = text "timestamp"
renderDataType HierarchyId {} = text "hierarchyid"
renderDataType UniqueIdentifier {} = text "uniqueidentifier"
renderDataType SqlVariant {} = text "sql_variant"
renderDataType Xml {} = text "xml"
renderDataType Geography {} = text "geography"
renderDataType Geometry {} =  text "geometry"

{-
  All data types are valid for use as partitioning columns, except text, 
  ntext, image, xml, timestamp, varchar(max), nvarchar(max), varbinary(max), 
  alias data types, or CLR user-defined data types.
-}
isSupportedTypeForPartitionFunction :: Type -> Bool
isSupportedTypeForPartitionFunction Text {} = False
isSupportedTypeForPartitionFunction NText {} = False
isSupportedTypeForPartitionFunction Xml {} = False
isSupportedTypeForPartitionFunction Timestamp {} = False
isSupportedTypeForPartitionFunction Image {} = False
isSupportedTypeForPartitionFunction VarBinary {} = False
isSupportedTypeForPartitionFunction NVarChar {} = False
isSupportedTypeForPartitionFunction VarChar {} = False
isSupportedTypeForPartitionFunction Geometry {} = False
isSupportedTypeForPartitionFunction Geography {} = False
isSupportedTypeForPartitionFunction _ = True

value :: Type -> Maybe (Gen SQLValue)
value BigInt {} = Just arbitrarySQLInt64
value Bit {} = Just arbitrarySQLBit
value Numeric {} = Just arbitrarySQLNumeric
value SmallInt {} = Just arbitrarySQLInt16
value Decimal {} = Just arbitrarySQLNumeric
value SmallMoney {}= Just arbitrarySQLSmallMoney
value Int {} = Just arbitrarySQLInt32
value TinyInt {} = Just arbitrarySQLTinyInt
value Money {} = Just arbitrarySQLMoney
value Float {} = Just arbitrarySQLFloat
value Real {} = Just arbitrarySQLFloat
value Date {} = Just arbitrarySQLDate
value DateTimeOffset {} = Just arbitrarySQLDateTime
value DateTime2 {} = Just arbitrarySQLDateTime
value SmallDateTime {} = Just arbitrarySQLSmallDateTime
value DateTime {} = Just arbitrarySQLDateTime
value Time {} = Just arbitrarySQLTime
value (Char n _ _) = Just $ arbitrarySQLString (maybe 1 (\(FixedRange r) -> r) n)
value (VarChar n _ _) = Just $ arbitrarySQLString (rangeSize n)
value Text {} = Nothing
value (NChar n _ _) = Just $ arbitrarySQLString (maybe 1 (\(NFixedRange r) -> r) n)
value (NVarChar n _ _) = Just $ arbitrarySQLString (maybe 1 nRangeSize n)
value NText {} = Nothing
value Binary {}  = Just arbitrarySQLBinary
value VarBinary {} = Just arbitrarySQLBinary
value Image {} = Nothing
value Timestamp {} = Nothing
value HierarchyId {} = Just arbitrarySQLHierarchyID
value UniqueIdentifier {} = Just arbitrarySQLUniqueIdentifier
value SqlVariant {} = Just arbitrarySQLVariant
value Xml {} = Just arbitrarySQLXml
value Geography {} = Just arbitrarySQLGeography
value Geometry {} = Just arbitrarySQLGeometry

