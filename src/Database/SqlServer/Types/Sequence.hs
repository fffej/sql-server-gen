{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Types.Sequence where

import Prelude hiding (cycle)

import Database.SqlServer.Types.Properties (NamedEntity,name)
import Database.SqlServer.Types.Identifiers (RegularIdentifier(..), renderRegularIdentifier)

import Text.PrettyPrint
import Test.QuickCheck
import Data.DeriveTH
import Control.Monad


data NumericType = TinyInt | SmallInt | Int | BigInt | Decimal | Numeric

renderNumericType :: NumericType -> Doc
renderNumericType TinyInt = text "AS tinyint"
renderNumericType SmallInt = text "AS smallint"
renderNumericType Int = text "AS int"
renderNumericType BigInt = text "AS bigint"
renderNumericType Decimal = text "AS decimal"
renderNumericType Numeric = text "AS numeric"

derive makeArbitrary ''NumericType

data SequenceDefinition = SequenceDefinition
                  {
                    sequenceName :: RegularIdentifier
                  , sequenceType :: Maybe NumericType
                  , startWith    :: Maybe Integer
                  , incrementBy  :: Maybe Integer
                  , minValue     :: Maybe (Maybe Integer)
                  , maxValue     :: Maybe (Maybe Integer)
                  , cycle        :: Maybe Bool
                  , cache        :: Maybe (Maybe Integer)
                  }

instance NamedEntity SequenceDefinition where
  name = sequenceName

renderMinValue :: Maybe Integer -> Doc
renderMinValue Nothing = text "NO MINVALUE"
renderMinValue (Just n) = text "MINVALUE" <+> integer n

renderMaxValue :: Maybe Integer -> Doc
renderMaxValue Nothing = text "NO MAXVALUE"
renderMaxValue (Just n) = text "MAXVALUE" <+> integer n

renderCacheValue :: Maybe Integer -> Doc
renderCacheValue Nothing = text "NO CACHE"
renderCacheValue (Just n) = text "CACHE" <+> integer n

renderSequenceDefinition :: SequenceDefinition -> Doc
renderSequenceDefinition s = text "CREATE SEQUENCE" <+> renderRegularIdentifier (sequenceName s) $+$
                            dataType $+$ startWith' $+$ incrementBy' $+$ minValue' $+$ maxValue' $+$
                            cycle' $+$ cache'
  where
    dataType = maybe empty renderNumericType (sequenceType s)
    startWith' = maybe empty (\x -> text "START WITH" <+> integer x) (startWith s)
    incrementBy' = maybe empty (\x -> text "INCREMENT BY" <+> integer x) (incrementBy s)
    minValue' = maybe empty renderMinValue (minValue s)
    maxValue' = maybe empty renderMaxValue (maxValue s)
    cycle'    = maybe empty (\x -> if x then text "CYCLE" else text "NO CYCLE") (cycle s)
    cache'    = maybe empty renderCacheValue (cache s)


numericBounds :: Maybe NumericType -> Maybe (Integer,Integer)
numericBounds  (Just TinyInt)  = Just (0,255)
numericBounds  (Just SmallInt) = Just (- 32768,32767)
numericBounds  (Just Int)      = Just (- 2147483648,214748367)
numericBounds  (Just BigInt)   = Just (- 9223372036854775808,9223372036854775807)
numericBounds  _               = Nothing

-- TODO get rid of duplication
arbitraryValue :: Maybe NumericType -> Gen (Maybe Integer)
arbitraryValue Nothing = arbitraryValue (Just Int)
arbitraryValue (Just TinyInt)  = oneof [liftM Just $ choose (0,255),return Nothing]
arbitraryValue (Just SmallInt) = oneof [liftM Just $ choose (- 32768,32767),return Nothing]
arbitraryValue (Just Int)      = oneof [liftM Just $ choose (- 2147483648,214748367),return Nothing]
arbitraryValue (Just BigInt)   = oneof [liftM Just $ choose (- 9223372036854775808,9223372036854775807),return Nothing]
arbitraryValue (Just Numeric)  = oneof [liftM Just $ arbitrary,return Nothing]
arbitraryValue (Just Decimal)  = oneof [liftM Just $ arbitrary,return Nothing]


arbitraryCacheValue :: Gen (Maybe Integer)
arbitraryCacheValue = frequency [(50,liftM Just $ choose (1,500)), (50,return Nothing)]

greaterThanMin :: Maybe Integer -> Maybe Integer -> Bool
greaterThanMin Nothing     _           = True
greaterThanMin (Just _)    Nothing     = True
greaterThanMin (Just min') (Just max') = max' > min'

lessThanMax :: Maybe Integer -> Maybe Integer -> Bool
lessThanMax Nothing     _           = True
lessThanMax (Just _)    Nothing     = True
lessThanMax (Just max') (Just min') = min' < max'

{-  The absolute value of the increment for sequence object 'VYOAkIDTbnNf65tp9h_I'
    must be less than or equal to the difference between the minimum and maximum
    value of the sequence object.
-}
validIncrementBy :: Maybe NumericType -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Bool
validIncrementBy _       _ _ (Just 0) = False -- 0 is never a valid increment
validIncrementBy Nothing x y z = validIncrementBy (Just Int) x y z -- default to int32
validIncrementBy (Just t) x y z = validIncrementBy' t x y z 

validIncrementBy' :: NumericType -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Bool
validIncrementBy' Decimal min' max' incr' = validIncrementBy' Numeric min' max' incr' -- can treat these the same
validIncrementBy' Numeric min' max' incr' = maybe True (\absDiff -> maybe True (\x -> abs x <= abs absDiff) incr') (liftM2 (-) max' min')
validIncrementBy' x       min' max' incr' = maybe True (\incr -> abs incr <= diff) incr'
  where
    Just (lower,upper) = numericBounds (Just x)
    min'' = maybe lower id min'
    max'' = maybe upper id max'
    diff  = abs (max'' - min'')

validSequenceName :: RegularIdentifier -> Bool
validSequenceName (RegularIdentifier (x:_)) = x /= '#'
validSequenceName _                         = error "new type declaration should fix this"

instance Arbitrary SequenceDefinition where
  arbitrary = do
    nm <- arbitrary `suchThat` validSequenceName
    dataType <- arbitrary
    minV <- arbitraryValue dataType
    maxV <- arbitraryValue dataType `suchThat` (greaterThanMin minV)
    start <- arbitraryValue dataType `suchThat` (\x -> greaterThanMin minV x && lessThanMax maxV x)
    increment <- arbitraryValue dataType `suchThat` (validIncrementBy dataType minV maxV)
    cyc <- arbitrary
    hasMinValue <- elements [Just, \_ -> Nothing]
    hasMaxValue <- elements [Just, \_ -> Nothing]
    hasChcValue <- elements [Just, \_ -> Nothing]    
    chc <- arbitraryCacheValue
    return $ SequenceDefinition {
        sequenceName = nm
      , sequenceType = dataType
      , startWith = start
      , incrementBy = increment
      , minValue = hasMinValue minV
      , maxValue = hasMaxValue maxV                  
      , cycle = cyc
      , cache = hasChcValue chc
      }

