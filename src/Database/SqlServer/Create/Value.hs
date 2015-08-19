module Database.SqlServer.Create.Value
       (
         SQLValue
       , SQLNumericOptions (..)
       , renderValue
       , arbitrarySQLBigInt -- I realize this is smelly
       , arbitrarySQLInt -- I can't think of a better way
       , arbitrarySQLSmallInt
       , arbitrarySQLTinyInt
       , arbitrarySQLBinary
       , arbitrarySQLBit
       , arbitrarySQLSmallMoney
       , arbitrarySQLMoney
       , arbitrarySQLDate
       , arbitrarySQLDateTime
       , arbitrarySQLSmallDateTime
       , arbitrarySQLTime
       , arbitrarySQLGeography
       , arbitrarySQLGeometry
       , arbitrarySQLString
       , arbitrarySQLFloat
       , arbitrarySQLHierarchyID
       , arbitrarySQLVariant
       , arbitrarySQLXml
       , arbitrarySQLUniqueIdentifier
       , arbitrarySQLNumeric
       ) where

import Database.SqlServer.Create.Identifier (ArbUUID)

import Text.PrettyPrint
import Test.QuickCheck hiding (scale)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.ISO8601
import Data.Time.LocalTime

import Control.Monad
import Data.Int
import Data.Word

data SQLNumericOptions = SQLNumericOptions
  {
    scale :: Int
  , precision :: Int
  } deriving (Eq)

data SQLValue = SQLBigInt Int64
              | SQLInt Int32
              | SQLSmallInt Int16
              | SQLTinyInt Word8
              | SQLBinary Integer
              | SQLBit (Maybe Bool)
              | SQLSmallMoney Int32
              | SQLMoney Int64
              | SQLDate Day
              | SQLDateTime UTCTime
              | SQLSmallDateTime UTCTime
              | SQLTime DiffTime
              | SQLGeography String
              | SQLGeometry String
              | SQLString String
              | SQLFloat Float
              | SQLHierarchyID String
              | SQLVariant Variant
              | SQLXml String
              | SQLUniqueIdentifier ArbUUID
              | SQLNumeric Integer SQLNumericOptions
                deriving (Eq)

data Variant = SQLVariantInt Integer
             | SQLVariantString String
               deriving (Eq)

variantToDoc :: Variant -> Doc
variantToDoc (SQLVariantString s) = quotes (text s)
variantToDoc (SQLVariantInt n) = integer n

arbitrarySQLBit :: Gen SQLValue
arbitrarySQLBit = liftM SQLBit arbitrary

arbitrarySQLBigInt :: Gen SQLValue
arbitrarySQLBigInt = liftM SQLBigInt arbitrary

arbitrarySQLInt :: Gen SQLValue
arbitrarySQLInt = liftM SQLInt arbitrary

arbitrarySQLSmallInt :: Gen SQLValue
arbitrarySQLSmallInt = liftM SQLSmallInt arbitrary

arbitrarySQLTinyInt :: Gen SQLValue
arbitrarySQLTinyInt = liftM SQLTinyInt arbitrary

arbitrarySQLBinary :: Gen SQLValue
arbitrarySQLBinary = liftM SQLBinary arbitrary

arbitrarySQLSmallMoney :: Gen SQLValue
arbitrarySQLSmallMoney = liftM SQLSmallMoney arbitrary

arbitrarySQLMoney :: Gen SQLValue
arbitrarySQLMoney = liftM SQLMoney arbitrary

arbitrarySQLDate :: Gen SQLValue
arbitrarySQLDate = do
    y <- choose (0, 9999)
    m <- choose (1, 12)
    d <- choose (1, 31)
    return $ SQLDate (fromGregorian y m d) -- clipping handled by time package

dateBetween :: Integer -> Integer -> Gen Day
dateBetween startYear endYear = do
  y <- choose (startYear, endYear)
  m <- choose (1, 12)
  d <- choose (1, 31)
  return (fromGregorian y m d)

arbitrarySQLDateTime :: Gen SQLValue
arbitrarySQLDateTime = do
  day <- dateBetween 1753 9999
  datetime <- choose (0, 86400)
  return (SQLDateTime (UTCTime day (secondsToDiffTime datetime)))

arbitrarySQLSmallDateTime :: Gen SQLValue
arbitrarySQLSmallDateTime = do
  day <- dateBetween 1900 2078
  datetime <- choose (0, 86400)
  return (SQLSmallDateTime (UTCTime day (secondsToDiffTime datetime)))

arbitrarySQLTime :: Gen SQLValue
arbitrarySQLTime = do
  time <- choose (0, 86400)
  return (SQLTime (secondsToDiffTime time))

arbitrarySQLGeography :: Gen SQLValue
arbitrarySQLGeography = do
  a <- choose (- 90, 90) :: Gen Float
  b <- choose (- 90, 90) :: Gen Float
  c <- choose (- 90, 90) :: Gen Float
  d <- choose (- 90, 90) :: Gen Float
  return $ SQLGeography ("LINESTRING(" ++
                         show a ++ " " ++
                         show b ++ "," ++
                         show c ++ " " ++
                         show d ++ ")")

-- Deliberate duplication so I can customize this
arbitrarySQLGeometry :: Gen SQLValue
arbitrarySQLGeometry = do
  a <- arbitrary :: Gen Float
  b <- arbitrary :: Gen Float
  c <- arbitrary :: Gen Float
  d <- arbitrary :: Gen Float
  return $ SQLGeometry ("LINESTRING(" ++
                        show a ++ " " ++
                        show b ++ "," ++
                        show c ++ " " ++
                        show d ++ ")")

arbitrarySQLString :: Int -> Gen SQLValue
arbitrarySQLString n = do
  c <- listOf $ elements ['a' .. 'z']
  return $ SQLString (take n c)

arbitrarySQLNumeric :: SQLNumericOptions -> Gen SQLValue
arbitrarySQLNumeric no = do
  let s = scale no
  xs <- replicateM s (choose (0, 9))
  b <- arbitrary
  let num = foldl (\ x y -> y + x * 10) 0 xs
  return (SQLNumeric (if b then num else (- num)) no)

arbitrarySQLFloat :: Gen SQLValue
arbitrarySQLFloat = liftM SQLFloat arbitrary

-- Just pick one of the examples from MSDN
arbitrarySQLHierarchyID :: Gen SQLValue
arbitrarySQLHierarchyID = liftM SQLHierarchyID $
                          elements [
                              "/"
                            , "/1/"
                            , "/0.3.-7/"
                            , "/1/3/"
                            , "/0.1/0.2/"
                            ]

arbitrarySQLUniqueIdentifier :: Gen SQLValue
arbitrarySQLUniqueIdentifier = liftM SQLUniqueIdentifier arbitrary

arbitrarySQLVariant :: Gen SQLValue
arbitrarySQLVariant = do
  x <- arbitrary
  y <- elements [SQLVariantString . show, SQLVariantInt]
  return $ SQLVariant (y x)

arbitrarySQLXml :: Gen SQLValue
arbitrarySQLXml = return $ SQLXml "some xml"

-- https://msdn.microsoft.com/en-us/library/ms190476.aspx
renderNumeric :: SQLNumericOptions -> Integer -> Doc
renderNumeric no n = double num
  where
    num = fromIntegral n / (10.0 ^ p)
    p = precision no

divideBy10000 :: Integer -> String
divideBy10000 n
  | length s < 5 = s
  | otherwise = take (len - 4) s ++ "." ++ drop (len - 4) s
  where
    s = show n
    len = length s

renderValue :: SQLValue -> Doc
renderValue (SQLDate d) = quotes (text $ showGregorian d)
renderValue (SQLString s) = quotes $ text s
renderValue (SQLGeography x) = quotes $ text x
renderValue (SQLGeometry x) = quotes $ text x
renderValue (SQLDateTime s) = quotes $ text (formatISO8601Millis s)
renderValue (SQLSmallDateTime s) = quotes $ text (formatISO8601Millis s)
renderValue (SQLTime t) = quotes $ text (show $ timeToTimeOfDay t)
renderValue (SQLFloat f) = float f
renderValue (SQLHierarchyID x) = quotes $ text x
renderValue (SQLUniqueIdentifier s) = (quotes . text . show) s
renderValue (SQLVariant s) = variantToDoc s
renderValue (SQLXml s) = quotes $ text s
renderValue (SQLSmallMoney s) = text (divideBy10000 $ fromIntegral s)
renderValue (SQLMoney s) = text (divideBy10000 $ fromIntegral s)
renderValue (SQLBit b) = maybe (text "NULL") (\ x -> int (if x then 1 else 0)) b
renderValue (SQLBinary s) = integer s
renderValue (SQLTinyInt s) = (text . show) s
renderValue (SQLSmallInt s) = (text . show) s
renderValue (SQLInt s) = (text . show) s
renderValue (SQLBigInt s) = (text . show) s
renderValue (SQLNumeric n no) = renderNumeric no n
