module Database.SqlServer.Definition.Value
       (
         SQLDate
       , SQLDateTime
       , SQLGeography
       , SQLGeometry
       , SQLVariant
       , SQLXml
       , SQLUniqueIdentifier
       , SQLString
       , SQLFloat
       , SQLTime
       , SQLNumeric
       , SQLSmallDateTime
       , SQLHierarchyID
       , SQLBit
       , SQLInt16
       , SQLInt32
       , SQLInt64
       , SQLTinyInt
       , renderSQLDate
       , renderSQLString
       , renderNumeric
       , renderSQLGeography
       , renderSQLGeometry
       , renderSQLDateTime
       , renderSQLSmallDateTime
       , renderSQLTime
       , renderSQLFloat
       , renderSQLHierarchyID
       , renderSQLUniqueIdentifier
       , renderSQLVariant
       , renderSQLXml
       ) where

import Database.SqlServer.Definition.Identifier (ArbUUID)

import Text.PrettyPrint
import Test.QuickCheck
import Control.Monad
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.ISO8601
import Data.Time.LocalTime

import Data.Int
import Data.Word

type SQLInt64 = Int64
type SQLInt32 = Int32
type SQLInt16 = Int16
type SQLBit = Maybe Bool
type SQLTinyInt = Word8



data SQLDate = SQLDate Day

instance Arbitrary SQLDate where
  arbitrary = do
    y <- choose (0,9999)
    m <- choose (1,12)
    d <- choose (1,31)
    return $ SQLDate (fromGregorian y m d) -- clipping handled by time package

data SQLDateTime = SQLDateTime UTCTime

dateBetween :: Integer -> Integer -> Gen Day
dateBetween startYear endYear = do
  y <- choose (startYear,endYear)
  m <- choose (1,12)
  d <- choose (1,31)
  return (fromGregorian y m d)

instance Arbitrary SQLDateTime where
  arbitrary = do
    day <- dateBetween 1753 9999
    datetime <- choose (0,86400)
    return (SQLDateTime (UTCTime day (secondsToDiffTime datetime)))

data SQLSmallDateTime = SQLSmallDateTime UTCTime

instance Arbitrary SQLSmallDateTime where
  arbitrary = do
    day <- dateBetween 1900 2078
    datetime <- choose (0,86400)
    return (SQLSmallDateTime (UTCTime day (secondsToDiffTime datetime)))


data SQLTime = SQLTime DiffTime

instance Arbitrary SQLTime where
  arbitrary = do
    time <- choose (0,86400)
    return (SQLTime (secondsToDiffTime time))

data SQLGeography = SQLGeography String

instance Arbitrary SQLGeography where
  arbitrary = do
    a <- choose (- 90, 90) :: Gen Float
    b <- choose (- 90, 90) :: Gen Float
    c <- choose (- 90, 90) :: Gen Float
    d <- choose (- 90, 90) :: Gen Float

    return $ SQLGeography ("LINESTRING(" ++ show a ++ " " ++ show b ++ "," ++ show c ++ " " ++ show d ++ ")")
  
data SQLGeometry = SQLGeometry String

-- Deliberate duplication so I can customize this
instance Arbitrary SQLGeometry where
  arbitrary = do
    a <- arbitrary :: Gen Float
    b <- arbitrary :: Gen Float
    c <- arbitrary :: Gen Float
    d <- arbitrary :: Gen Float
    return $ SQLGeometry ("LINESTRING(" ++ show a ++ " " ++ show b ++ "," ++ show c ++ " " ++ show d ++ ")")


data SQLString = SQLString String

instance Arbitrary SQLString where
  arbitrary = liftM SQLString $ listOf $ elements ['a' .. 'z']

data SQLNumeric = SQLNumeric Integer

instance Arbitrary SQLNumeric where
  arbitrary = liftM SQLNumeric arbitrary

data SQLFloat = SQLFloat Float

instance Arbitrary SQLFloat where
  arbitrary = liftM SQLFloat arbitrary

data SQLHierarchyID = SQLHierarchyID String

-- Just pick one of the examples from MSDN
instance Arbitrary SQLHierarchyID where
  arbitrary = liftM SQLHierarchyID $ elements ["/","/1/","/0.3.-7/","/1/3/","/0.1/0.2/"]

data SQLUniqueIdentifier = SQLUniqueIdentifier ArbUUID

instance Arbitrary SQLUniqueIdentifier where
  arbitrary = liftM SQLUniqueIdentifier arbitrary

data SQLVariant = SQLVariantInt Int
                | SQLVariantString String

instance Arbitrary SQLVariant where
  arbitrary = do
    x <- arbitrary
    y <- elements [SQLVariantString . show, SQLVariantInt]
    return $ y x

data SQLXml = SQLXml String

instance Arbitrary SQLXml where
  arbitrary = return $ SQLXml "some xml"

renderSQLDate :: SQLDate -> Doc
renderSQLDate (SQLDate d) = quotes (text $ showGregorian d)

renderSQLString :: SQLString -> Doc
renderSQLString (SQLString s) = quotes $ text s

-- https://msdn.microsoft.com/en-us/library/ms190476.aspx
renderNumeric :: Maybe (Int,Int) -> SQLNumeric -> Doc
renderNumeric Nothing s                = renderNumeric (Just (18,18)) s
renderNumeric (Just (p,s)) (SQLNumeric n) = text num
  where
    v = take p (show (abs n))
    len = length v
    num = take (len - s) v ++ "." ++ drop (len - s) v

renderSQLGeography :: SQLGeography -> Doc
renderSQLGeography (SQLGeography x) = quotes $ text x

renderSQLGeometry :: SQLGeometry -> Doc
renderSQLGeometry (SQLGeometry x) = quotes $ text x

renderSQLDateTime :: SQLDateTime -> Doc
renderSQLDateTime (SQLDateTime s) = quotes $ text (formatISO8601Millis s)

renderSQLSmallDateTime :: SQLSmallDateTime -> Doc
renderSQLSmallDateTime (SQLSmallDateTime s) = quotes $ text (formatISO8601Millis s)

renderSQLTime :: SQLTime -> Doc
renderSQLTime (SQLTime t) =  quotes $ text (show $ timeToTimeOfDay t)

renderSQLFloat :: SQLFloat -> Doc
renderSQLFloat (SQLFloat f) = float f

renderSQLHierarchyID :: SQLHierarchyID -> Doc
renderSQLHierarchyID (SQLHierarchyID x) = quotes $ text x

renderSQLUniqueIdentifier :: SQLUniqueIdentifier -> Doc
renderSQLUniqueIdentifier (SQLUniqueIdentifier s) = (quotes . text  . show) s

renderSQLVariant :: SQLVariant -> Doc
renderSQLVariant (SQLVariantInt s) = int s
renderSQLVariant (SQLVariantString s) = text s

renderSQLXml :: SQLXml -> Doc
renderSQLXml (SQLXml s) = quotes $ text s
