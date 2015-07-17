{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.DataTypes where

import Database.SqlServer.Types.Collations (Collation)

import Text.PrettyPrint

import Test.QuickCheck hiding (scale)
import Control.Monad 
import Data.DeriveTH
import Data.Int
import Data.Word


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

renderFileStream :: VarBinaryStorage -> Doc
renderFileStream MaxFileStream = text "FILESTREAM"
renderFileStream _             = empty

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

uniqueIdentifierstorageOptions :: UniqueIdentifierOptions -> Maybe StorageOptions
uniqueIdentifierstorageOptions (UniqueIdentifierOptions x) = x
uniqueIdentifierstorageOptions (RowGuidCol x) = fmap StorageOptions x

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

renderNumericStorage :: NumericStorage -> Doc
renderNumericStorage ns = lparen <> int (precision ns) <> scale' <> rparen
  where
    scale' = maybe empty (\x -> comma <+> int x) (scale ns)

{- The scale must be less than or equal to the precision -}
instance Arbitrary NumericStorage where                      
  arbitrary = do
    p <- choose(1,38)
    s <- elements (Nothing : map Just [1..p])
    return $ NumericStorage p s

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

data SQLDate = SQLData
               {
                 year :: Int
               , month :: Int
               , day :: Int                 
               }

data SQLGeography = SQLGeography String

instance Arbitrary SQLGeography where
  arbitrary = liftM SQLGeography $ listOf $ elements ['a' .. 'z']
  
data SQLGeometry = SQLGeometry String

instance Arbitrary SQLGeometry where
  arbitrary = liftM SQLGeometry $ listOf $ elements ['a' .. 'z']

data SQLString = SQLString String

instance Arbitrary SQLString where
  arbitrary = liftM SQLString $ listOf $ elements ['a' .. 'z']

  
-- Note that SQL Server doesn't check the validity
-- of the dates, so I choose not to either!
instance Arbitrary SQLDate where
  arbitrary = do
    y <- choose (0,9999)
    m <- choose (1,12)
    d <- choose (1,31)
    return $ SQLData y m d

-- https://msdn.microsoft.com/en-us/library/ms187752.aspx
data Type = BigInt (Maybe StorageOptions) Int64
          | Bit (Maybe StorageOptions) (Maybe Bool)
          | Numeric (Maybe StorageOptions) (Maybe NumericStorage)
          | SmallInt (Maybe StorageOptions) Int16
          | Decimal (Maybe StorageOptions) (Maybe NumericStorage)
          | SmallMoney (Maybe StorageOptions) Int32
          | Int (Maybe StorageOptions) Int32
          | TinyInt (Maybe StorageOptions) Word8
          | Money (Maybe StorageOptions) Int64
          | Float (Maybe StorageOptions) (Maybe PrecisionStorage)
          | Real (Maybe StorageOptions)
          | Date (Maybe StorageOptions) SQLDate
          | DateTimeOffset (Maybe StorageOptions) (Maybe FractionalSecondsPrecision)
          | DateTime2 (Maybe StorageOptions) (Maybe FractionalSecondsPrecision)
          | SmallDateTime (Maybe StorageOptions)
          | DateTime (Maybe StorageOptions)
          | Time (Maybe StorageOptions) (Maybe FractionalSecondsPrecision)
          | Char (Maybe FixedRange) (Maybe Collation) (Maybe StorageOptions) SQLString
          | VarChar Range (Maybe Collation) (Maybe StorageOptions) SQLString
          | Text (Maybe Collation) (Maybe NullStorageOptions)
          | NChar (Maybe NFixedRange) (Maybe Collation) (Maybe StorageOptions) SQLString
          | NVarChar (Maybe NRange) (Maybe Collation) (Maybe StorageOptions) SQLString
          | NText (Maybe Collation) (Maybe NullStorageOptions)
          | Binary (Maybe FixedRange) (Maybe StorageOptions) Integer
          | VarBinary (Maybe VarBinaryStorage) (Maybe StorageOptions) Integer
          | Image (Maybe NullStorageOptions)
          | Timestamp (Maybe NullStorageOptions)
          | HierarchyId (Maybe StorageOptions)
          | UniqueIdentifier (Maybe UniqueIdentifierOptions)
          | SqlVariant (Maybe StorageOptions)
          | Xml (Maybe StorageOptions)
          | Geography (Maybe NullStorageOptions) SQLGeography
          | Geometry  (Maybe NullStorageOptions) SQLGeometry

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
storageSize (Numeric _ ns) = maybe (9 * 8) numericStorageSize ns -- default precision is 18
storageSize (Decimal _ ns) = maybe (9 * 8) numericStorageSize ns -- default precision is 18
storageSize (Float _ ps) = maybe (8 * 8) precisionStorageSize ps -- default precision is 53
storageSize (Real _) = 4 * 8
storageSize (Date _ _) = 3 * 8
storageSize (DateTime _ ) = 8 * 8
storageSize (DateTime2 _ p) = maybe (8 * 8) datetime2StorageSize p -- default is 8 bytes
storageSize (DateTimeOffset _ p) = maybe (10 * 8) dateTimeOffsetStorageSize p
storageSize (SmallDateTime _) = 4 * 8
storageSize (Time _ p) = maybe (5 * 8) timeStorageSize p
storageSize (Char fr _ _ _) = maybe 8 fixedRangeStorage fr
storageSize (NChar fr _ _ _) = maybe 8 nfixedRangeStorage fr
storageSize (Binary p _ _) = maybe (1 * 8) fixedRangeStorage p
storageSize (UniqueIdentifier _) = 16 * 8
storageSize (VarBinary r _ _) = maybe (1 * 8) varBinarySize r
storageSize (VarChar r _ _ _) = rangeStorageSize r 
storageSize (NVarChar r _ _ _) = maybe (1 * 8) nRangeStorageSize r
storageSize (Text _ _) = 0 -- assumption
storageSize (NText _ _) = 0 -- assumption
storageSize (Image _) = 0 -- assumption
storageSize (Timestamp _) = 5 * 8
storageSize (HierarchyId _) = 0 -- assumption
storageSize (Geometry _ _) = 0
storageSize (Geography _ _) = 0
storageSize (SqlVariant _) = 0
storageSize (Xml _) = 0


nullOptions :: Type -> Maybe NullStorageOptions
nullOptions t = maybe Nothing nullStorageFromStorageOptions (storageOptions t)

rowGuidOptions :: Type -> Bool
rowGuidOptions (UniqueIdentifier a)  = maybe False isRowGuidCol a
rowGuidOptions _                     = False

storageOptions :: Type -> Maybe StorageOptions
storageOptions (BigInt s _) = s
storageOptions (Bit s _) = s
storageOptions (Numeric s _) = s
storageOptions (SmallInt s _) = s
storageOptions (Decimal s _) = s
storageOptions (SmallMoney s _) = s
storageOptions (Int s _) = s
storageOptions (TinyInt s _) = s
storageOptions (Money s _) = s
storageOptions (Float s _) = s
storageOptions (Real s) = s
storageOptions (Date s _) = s
storageOptions (DateTimeOffset s _) = s
storageOptions (DateTime2 s _) = s
storageOptions (SmallDateTime s) = s
storageOptions (DateTime s) = s
storageOptions (Time s _) = s
storageOptions (Char _ _ s _)  = s
storageOptions (VarChar _ _ s _) = s
storageOptions (NChar _ _ s _) = s
storageOptions (NVarChar _ _ s _) = s
storageOptions (Binary _ s _)  = s 
storageOptions (VarBinary _ s _) = s
storageOptions (HierarchyId s) = s
storageOptions (UniqueIdentifier s) = maybe Nothing uniqueIdentifierstorageOptions s
storageOptions (SqlVariant s) = s
storageOptions (Xml s) = s
storageOptions (Timestamp _) = Nothing
storageOptions (Text _ _) = Nothing
storageOptions (NText _ _) = Nothing
storageOptions (Image _) = Nothing
storageOptions (Geography _ _) = Nothing
storageOptions (Geometry _ _) = Nothing


divideBy10000 :: Integer -> String
divideBy10000 n
  | length s < 5 = s
  | otherwise    = take (len - 4) s ++ "." ++ drop (len - 4) s
  where
    s = show n
    len = length s

-- I've made no effort to fix padding.
-- Conversion fails at runtime (urgh!)
renderSQLDate :: SQLDate -> Doc
renderSQLDate d = quotes (text yyyy <> text "-" <>
                          text mm <> text "-" <>
                          text dd)
  where
    yyyy = show (year d)
    mm = show (month d)
    dd = show (day d)

renderSQLString :: SQLString -> Doc
renderSQLString (SQLString s) = quotes $ text s

renderValue :: Type -> Doc
renderValue (BigInt _ v) = (text . show) v
renderValue (Int _ v) = (text . show) v
renderValue (TinyInt _ v) = (text . show) v
renderValue (SmallInt _ v) = (text . show) v
renderValue (Bit _ b) = maybe (text "NULL") (\x -> if x then int 1 else int 0) b
renderValue (SmallMoney _ s) = text (divideBy10000 $ fromIntegral s)
renderValue (Money _ s) = text (divideBy10000 $ fromIntegral s)
renderValue (Date _ d) = renderSQLDate d
renderValue (Geography _ (SQLGeography x)) = quotes (text x)
renderValue (Geometry _ (SQLGeometry x)) = quotes (text x)
renderValue (Binary _ _ x) = integer x
renderValue (VarBinary _ _ x) = integer x
renderValue (Char _ _ _ s) = renderSQLString s
renderValue (NChar _ _ _ s) = renderSQLString s
renderValue (VarChar _ _ _ s) = renderSQLString s
renderValue (NVarChar _ _ _ s) = renderSQLString s

renderDataType :: Type -> Doc
renderDataType (BigInt _ _) = text "bigint"
renderDataType (Bit _ _) = text "bit"
renderDataType (Numeric _ ns) = text "numeric" <> maybe empty renderNumericStorage ns
renderDataType (SmallInt _ _) = text "smallint"
renderDataType (Decimal _ ns) = text "decimal" <> maybe empty renderNumericStorage ns
renderDataType (SmallMoney _ _) = text "smallmoney"
renderDataType (Int _ _) = text "int"
renderDataType (TinyInt _ _) = text "tinyint"
renderDataType (Money _ _) = text "money"
renderDataType (Float _ ps) = text "float" <> maybe empty renderPrecisionStorage ps
renderDataType (Real _) = text "real"
renderDataType (Date _ _) = text "date"
renderDataType (DateTimeOffset _ p) = text "datetimeoffset" <> maybe empty renderFractionalSecondsPrecision p
renderDataType (DateTime2 _ p) = text "datetime2" <> maybe empty renderFractionalSecondsPrecision p
renderDataType (SmallDateTime _) = text "smalldatetime"
renderDataType (DateTime _) = text "datetime"
renderDataType (Time _ p)= text "time" <> maybe empty renderFractionalSecondsPrecision p
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
renderDataType (HierarchyId _) = text "hierarchyid"
renderDataType (UniqueIdentifier _) = text "uniqueidentifier"
renderDataType (SqlVariant _) = text "sql_variant"
renderDataType (Xml _) = text "xml"
renderDataType (Geography _ _) = text "geography"
renderDataType (Geometry _ _) =  text "geometry"

