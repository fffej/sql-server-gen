{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.SqlServer.Definition.Value
       (
         SQLValue
       , toDoc
       ) where

import Database.SqlServer.Definition.Identifier (ArbUUID)

import Text.PrettyPrint
import Test.QuickCheck
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.ISO8601
import Data.Time.LocalTime

import Data.Int
import Data.Word

data SQLValue = SQLInt64 Int64
              | SQLInt32 Int32
              | SQLInt16 Int16
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
              | SQLNumeric Integer

instance Arbitrary SQLValue where
  arbitrary = undefined

data Variant = SQLVariantInt Integer
             | SQLVariantString String

variantToDoc :: Variant -> Doc
variantToDoc (SQLVariantString s) = quotes (text s)
variantToDoc (SQLVariantInt n) = integer n

{-
instance Arbitrary SQLDate where
  arbitrary = do
    y <- choose (0,9999)
    m <- choose (1,12)
    d <- choose (1,31)
    return $ SQLDate (fromGregorian y m d) -- clipping handled by time package
-}

{-
dateBetween :: Integer -> Integer -> Gen Day
dateBetween startYear endYear = do
  y <- choose (startYear,endYear)
  m <- choose (1,12)
  d <- choose (1,31)
  return (fromGregorian y m d)
-}
{-
instance Arbitrary SQLDateTime where
  arbitrary = do
    day <- dateBetween 1753 9999
    datetime <- choose (0,86400)
    return (SQLDateTime (UTCTime day (secondsToDiffTime datetime)))
-}

{-
instance Arbitrary SQLSmallDateTime where
  arbitrary = do
    day <- dateBetween 1900 2078
    datetime <- choose (0,86400)
    return (SQLSmallDateTime (UTCTime day (secondsToDiffTime datetime)))
-}

{-
instance Arbitrary SQLTime where
  arbitrary = do
    time <- choose (0,86400)
    return (SQLTime (secondsToDiffTime time))
-}

{-
instance Arbitrary SQLGeography where
  arbitrary = do
    a <- choose (- 90, 90) :: Gen Float
    b <- choose (- 90, 90) :: Gen Float
    c <- choose (- 90, 90) :: Gen Float
    d <- choose (- 90, 90) :: Gen Float

    return $ SQLGeography ("LINESTRING(" ++ show a ++ " " ++ show b ++ "," ++ show c ++ " " ++ show d ++ ")")
-}


-- Deliberate duplication so I can customize this
{-instance Arbitrary SQLGeometry where
  arbitrary = do
    a <- arbitrary :: Gen Float
    b <- arbitrary :: Gen Float
    c <- arbitrary :: Gen Float
    d <- arbitrary :: Gen Float
    return $ SQLGeometry ("LINESTRING(" ++ show a ++ " " ++ show b ++ "," ++ show c ++ " " ++ show d ++ ")")
-}
{-
instance Arbitrary SQLString where
  arbitrary = liftM SQLString $ listOf $ elements ['a' .. 'z']

instance Arbitrary SQLNumeric where
  arbitrary = liftM SQLNumeric arbitrary

instance Arbitrary SQLFloat where
  arbitrary = liftM SQLFloat arbitrary

-- Just pick one of the examples from MSDN
instance Arbitrary SQLHierarchyID where
  arbitrary = liftM SQLHierarchyID $ elements ["/","/1/","/0.3.-7/","/1/3/","/0.1/0.2/"]

instance Arbitrary SQLUniqueIdentifier where
  arbitrary = liftM SQLUniqueIdentifier arbitrary

instance Arbitrary SQLVariant where
  arbitrary = do
    x <- arbitrary
    y <- elements [SQLVariantString . show, SQLVariantInt]
    return $ y x

instance Arbitrary SQLXml where
  arbitrary = return $ SQLXml "some xml"
-}

-- https://msdn.microsoft.com/en-us/library/ms190476.aspx
{-renderNumeric :: Maybe (Int,Int) -> SQLNumeric -> Doc
renderNumeric Nothing s                = renderNumeric (Just (18,18)) s
renderNumeric (Just (p,s)) (SQLNumeric n) = text num
  where
    v = take p (show (abs n))
    len = length v
    num = take (len - s) v ++ "." ++ drop (len - s) v
-}
divideBy10000 :: Integer -> String
divideBy10000 n
  | length s < 5 = s
  | otherwise    = take (len - 4) s ++ "." ++ drop (len - 4) s
  where
    s = show n
    len = length s

toDoc :: SQLValue -> Doc
toDoc (SQLDate d) = quotes (text $ showGregorian d)
toDoc (SQLString s) = quotes $ text s
toDoc (SQLGeography x) = quotes $ text x
toDoc (SQLGeometry x) = quotes $ text x
toDoc (SQLDateTime s) = quotes $ text (formatISO8601Millis s)
toDoc (SQLSmallDateTime s) = quotes $ text (formatISO8601Millis s)
toDoc (SQLTime t) =  quotes $ text (show $ timeToTimeOfDay t)
toDoc (SQLFloat f) = float f
toDoc (SQLHierarchyID x) = quotes $ text x
toDoc (SQLUniqueIdentifier s) = (quotes . text  . show) s
toDoc (SQLVariant s) = variantToDoc s
toDoc (SQLXml s) = quotes $ text s
toDoc (SQLSmallMoney s) = text (divideBy10000 $ fromIntegral s)
toDoc (SQLMoney s) = text (divideBy10000 $ fromIntegral s)
toDoc (SQLBit b) = maybe (text "NULL") (\x -> int (if x then 1 else  0)) b
toDoc (SQLBinary s) = integer s
toDoc (SQLTinyInt s) = (text . show) s
toDoc (SQLInt16 s) = (text . show) s
toDoc (SQLInt32 s) = (text . show) s
toDoc (SQLInt64 s) = (text . show) s
toDoc (SQLNumeric s) = (text . show) s
