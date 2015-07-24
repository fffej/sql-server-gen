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
       , renderValue
       , isSupportedTypeForPartitionFunction
       ) where

import Database.SqlServer.Definition.Value 
import Database.SqlServer.Definition.Collation (Collation)

import Text.PrettyPrint

import Test.QuickCheck hiding (scale)
import Control.Monad 
import Data.DeriveTH

import Data.Maybe

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

rangeStorageSize :: Range -> Int
rangeStorageSize (Sized x) = fixedRangeStorage x
rangeStorageSize _         = 0 

renderRange :: Range -> Doc
renderRange Max = text "(max)"
renderRange (Sized r) = renderFixedRange r

data NRange = NSized NFixedRange
            | NMax

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
data Type = BigInt (Maybe StorageOptions) SQLInt64
          | Bit (Maybe StorageOptions) SQLBit
          | Numeric (Maybe StorageOptions) (Maybe NumericStorage) SQLNumeric
          | SmallInt (Maybe StorageOptions) SQLInt16
          | Decimal (Maybe StorageOptions) (Maybe NumericStorage) SQLNumeric
          | SmallMoney (Maybe StorageOptions) SQLSmallMoney
          | Int (Maybe StorageOptions) SQLInt32
          | TinyInt (Maybe StorageOptions) SQLTinyInt
          | Money (Maybe StorageOptions) SQLMoney
          | Float (Maybe StorageOptions) (Maybe PrecisionStorage) SQLFloat
          | Real (Maybe StorageOptions) SQLFloat
          | Date (Maybe StorageOptions) SQLDate
          | DateTimeOffset (Maybe StorageOptions) (Maybe FractionalSecondsPrecision) SQLDateTime
          | DateTime2 (Maybe StorageOptions) (Maybe FractionalSecondsPrecision) SQLDateTime
          | SmallDateTime (Maybe StorageOptions) SQLSmallDateTime
          | DateTime (Maybe StorageOptions) SQLDateTime
          | Time (Maybe StorageOptions) (Maybe FractionalSecondsPrecision) SQLTime
          | Char (Maybe FixedRange) (Maybe Collation) (Maybe StorageOptions) SQLString
          | VarChar Range (Maybe Collation) (Maybe StorageOptions) SQLString
          | Text (Maybe Collation) (Maybe NullStorageOptions)
          | NChar (Maybe NFixedRange) (Maybe Collation) (Maybe StorageOptions) SQLString
          | NVarChar (Maybe NRange) (Maybe Collation) (Maybe StorageOptions) SQLString
          | NText (Maybe Collation) (Maybe NullStorageOptions)
          | Binary (Maybe FixedRange) (Maybe StorageOptions) SQLBinary
          | VarBinary (Maybe VarBinaryStorage) (Maybe StorageOptions) SQLBinary
          | Image (Maybe NullStorageOptions)
          | Timestamp (Maybe NullStorageOptions)
          | HierarchyId (Maybe StorageOptions) SQLHierarchyID
          | UniqueIdentifier (Maybe UniqueIdentifierOptions) SQLUniqueIdentifier
          | SqlVariant (Maybe StorageOptions) SQLVariant
          | Xml (Maybe StorageOptions) SQLXml
          | Geography (Maybe NullStorageOptions) SQLGeography
          | Geometry  (Maybe NullStorageOptions) SQLGeometry

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
collation (Char _ mc _ _)     = mc
collation (VarChar _ mc _ _)  = mc
collation (Text mc _)         = mc
collation (NChar _ mc _ _)    = mc
collation (NVarChar _ mc _ _) = mc
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
storageSize (BigInt _ _) = 8 * 8
storageSize (Int _ _)  = 4 * 8
storageSize (SmallInt _ _) = 2 * 8
storageSize (TinyInt _ _) = 1 * 8
storageSize (Bit _ _)     = 1
storageSize (SmallMoney _ _) = 4 * 8
storageSize (Money _ _) = 8 * 8
storageSize (Numeric _ ns _) = maybe (9 * 8) numericStorageSize ns -- default precision is 18
storageSize (Decimal _ ns _) = maybe (9 * 8) numericStorageSize ns -- default precision is 18
storageSize (Float _ ps _) = maybe (8 * 8) precisionStorageSize ps -- default precision is 53
storageSize (Real _ _) = 4 * 8
storageSize (Date _ _) = 3 * 8
storageSize (DateTime _ _) = 8 * 8
storageSize (DateTime2 _ p _) = maybe (8 * 8) datetime2StorageSize p -- default is 8 bytes
storageSize (DateTimeOffset _ p _) = maybe (10 * 8) dateTimeOffsetStorageSize p
storageSize (SmallDateTime _ _) = 4 * 8
storageSize (Time _ p _) = maybe (5 * 8) timeStorageSize p
storageSize (Char fr _ _ _) = maybe 8 fixedRangeStorage fr
storageSize (NChar fr _ _ _) = maybe 8 nfixedRangeStorage fr
storageSize (Binary p _ _) = maybe (1 * 8) fixedRangeStorage p
storageSize (UniqueIdentifier _ _) = 16 * 8
storageSize (VarBinary r _ _) = maybe (1 * 8) varBinarySize r
storageSize (VarChar r _ _ _) = rangeStorageSize r 
storageSize (NVarChar r _ _ _) = maybe (1 * 8) nRangeStorageSize r
storageSize (Text _ _) = 0 -- assumption
storageSize (NText _ _) = 0 -- assumption
storageSize (Image _) = 0 -- assumption
storageSize (Timestamp _) = 5 * 8
storageSize (HierarchyId _ _) = 0 -- assumption
storageSize (Geometry _ _) = 0
storageSize (Geography _ _) = 0
storageSize (SqlVariant _ _) = 0
storageSize (Xml _ _) = 0


nullOptions :: Type -> Maybe NullStorageOptions
nullOptions t = maybe Nothing nullStorageFromStorageOptions (storageOptions t)

rowGuidOptions :: Type -> Bool
rowGuidOptions (UniqueIdentifier a _) = maybe False isRowGuidCol a
rowGuidOptions _                      = False

storageOptions :: Type -> Maybe StorageOptions
storageOptions (BigInt s _) = s
storageOptions (Bit s _) = s
storageOptions (Numeric s _ _) = s
storageOptions (SmallInt s _) = s
storageOptions (Decimal s _ _) = s
storageOptions (SmallMoney s _) = s
storageOptions (Int s _) = s
storageOptions (TinyInt s _) = s
storageOptions (Money s _) = s
storageOptions (Float s _ _) = s
storageOptions (Real s _) = s
storageOptions (Date s _) = s
storageOptions (DateTimeOffset s _ _) = s
storageOptions (DateTime2 s _ _) = s
storageOptions (SmallDateTime s _) = s
storageOptions (DateTime s _) = s
storageOptions (Time s _ _) = s
storageOptions (Char _ _ s _)  = s
storageOptions (VarChar _ _ s _) = s
storageOptions (NChar _ _ s _) = s
storageOptions (NVarChar _ _ s _) = s
storageOptions (Binary _ s _)  = s 
storageOptions (VarBinary _ s _) = s
storageOptions (HierarchyId s _) = s
storageOptions (UniqueIdentifier s _) = maybe Nothing uniqueIdentifiertorageOptions s
storageOptions (SqlVariant s _) = s
storageOptions (Xml s _) = s
storageOptions (Timestamp _) = Nothing
storageOptions (Text _ _) = Nothing
storageOptions (NText _ _) = Nothing
storageOptions (Image _) = Nothing
storageOptions (Geography _ _) = Nothing
storageOptions (Geometry _ _) = Nothing

asInt :: NumericStorage -> (Int,Int)
asInt n = (precision n, maybe 18 id (scale n))

renderValue :: Type -> Maybe Doc
renderValue (Numeric _ n s) = Just $ renderNumeric (fmap asInt n) s 
renderValue (Decimal _ n s) = Just $ renderNumeric (fmap asInt n) s
renderValue (BigInt _ v) = Just $ (text . show) v
renderValue (Int _ v) = Just $ (text . show) v
renderValue (TinyInt _ v) = Just $ (text . show) v
renderValue (SmallInt _ v) = Just $ (text . show) v
renderValue (Bit _ b) = Just $ maybe (text "NULL") (\x -> int (if x then 1 else  0)) b
renderValue (SmallMoney _ s) = Just $ toDoc s
renderValue (Money _ s) = Just $ toDoc s
renderValue (Date _ d) = Just $ toDoc d
renderValue (Geography _ x) = Just $ toDoc x
renderValue (Geometry _ x) = Just $ toDoc x
renderValue (Binary _ _ x) = Just $ integer x
renderValue (VarBinary _ _ x) = Just $ integer x
renderValue (Char _ _ _ s) = Just $ toDoc s
renderValue (NChar _ _ _ s) = Just $ toDoc s
renderValue (VarChar _ _ _ s) = Just $ toDoc s
renderValue (NVarChar _ _ _ s) = Just $ toDoc s
renderValue (DateTime _ x) = Just $ toDoc x
renderValue (DateTime2 _ _ x) = Just $ toDoc x
renderValue (DateTimeOffset _ _ x) = Just $ toDoc x
renderValue (SmallDateTime _ s) = Just $ toDoc s 
renderValue (Time _ _ t) = Just $ toDoc t
renderValue (Float _ _ f) = Just $ toDoc f 
renderValue (Real _ f) = Just $ toDoc f
renderValue (HierarchyId _ x) = Just $ toDoc x
renderValue (UniqueIdentifier _ s) = Just $ toDoc s
renderValue (SqlVariant _ s) = Just $ toDoc s
renderValue (Xml _ s) = Just $ toDoc s
renderValue (Text _ _) = Nothing -- Text type invalid for local variables, function returns
renderValue (NText _ _) = Nothing -- NText type invalid for local variables, function returns
renderValue (Image _) = Nothing -- Image type invalid for local variable, function returns
renderValue (Timestamp _) = Nothing -- Timestamp invalid for local variable, function returns


renderDataType :: Type -> Doc
renderDataType (BigInt _ _) = text "bigint"
renderDataType (Bit _ _) = text "bit"
renderDataType (Numeric _ ns _) = text "numeric" <> maybe empty renderNumericStorage ns
renderDataType (SmallInt _ _) = text "smallint"
renderDataType (Decimal _ ns _) = text "decimal" <> maybe empty renderNumericStorage ns
renderDataType (SmallMoney _ _) = text "smallmoney"
renderDataType (Int _ _) = text "int"
renderDataType (TinyInt _ _) = text "tinyint"
renderDataType (Money _ _) = text "money"
renderDataType (Float _ ps _) = text "float" <> maybe empty renderPrecisionStorage ps
renderDataType (Real _ _) = text "real"
renderDataType (Date _ _) = text "date"
renderDataType (DateTimeOffset _ p _) = text "datetimeoffset" <> maybe empty renderFractionalSecondsPrecision p
renderDataType (DateTime2 _ p _) = text "datetime2" <> maybe empty renderFractionalSecondsPrecision p
renderDataType (SmallDateTime _ _) = text "smalldatetime"
renderDataType (DateTime _ _) = text "datetime"
renderDataType (Time _ p _)= text "time" <> maybe empty renderFractionalSecondsPrecision p
renderDataType (Char fixedRange _ _ _)  = text "char" <> maybe empty renderFixedRange fixedRange
renderDataType (VarChar range _ _ _) = text "varchar" <> renderRange range
renderDataType (Text _ _) = text "text"
renderDataType (NChar p _ _ _) = text "nchar" <> maybe empty renderNFixedRange p
renderDataType (NVarChar p _ _ _) = text "nvarchar" <> maybe empty renderNRange p
renderDataType (NText _ _) = text "ntext"
renderDataType (Binary fixedRange _ _)  = text "binary" <> maybe empty renderFixedRange fixedRange
renderDataType (VarBinary range _ _) = text "varbinary" <> maybe empty renderVarBinaryStorage range
renderDataType (Image _) = text "image"
renderDataType (Timestamp _) = text "timestamp"
renderDataType (HierarchyId _ _) = text "hierarchyid"
renderDataType (UniqueIdentifier _ _) = text "uniqueidentifier"
renderDataType (SqlVariant _ _) = text "sql_variant"
renderDataType (Xml _ _) = text "xml"
renderDataType (Geography _ _) = text "geography"
renderDataType (Geometry _ _) =  text "geometry"

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
isSupportedTypeForPartitionFunction x = isJust $ renderValue x
