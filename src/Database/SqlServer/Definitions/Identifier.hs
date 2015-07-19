{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Definitions.Identifier
       (
         RegularIdentifier
       , ArbUUID
       , ParameterIdentifier        
       , renderRegularIdentifier
       , renderParameterIdentifier
       , unwrap
       ) where

import Data.DeriveTH
import Test.QuickCheck
import Text.PrettyPrint
import Control.Monad
import Data.UUID
import Data.UUID.Util

newtype ArbUUID = ArbUUID { unpck :: UUID } deriving (Eq,Ord)

instance Show ArbUUID where
  show (ArbUUID x) = show x

newtype RegularIdentifier = RegularIdentifier ArbUUID deriving (Eq,Ord)

unwrap :: RegularIdentifier -> String
unwrap (RegularIdentifier x) = "UUID_" ++ xstr
  where
    xstr = filter (/= '-') (toString $ unpck x)

renderRegularIdentifier :: RegularIdentifier -> Doc
renderRegularIdentifier = text . unwrap 

newtype ParameterIdentifier = ParameterIdentifier RegularIdentifier

derive makeArbitrary ''ParameterIdentifier

renderParameterIdentifier :: ParameterIdentifier -> Doc
renderParameterIdentifier (ParameterIdentifier p) = text "@" <> renderRegularIdentifier p

instance Arbitrary ArbUUID where
  arbitrary = do
    tl <- choose(minBound,maxBound)
    tm <- choose(minBound,maxBound)
    th <- choose(minBound,maxBound)
    cshr <- choose(minBound,maxBound)
    csl <- choose(minBound,maxBound)
    n0 <- choose(minBound,maxBound)
    n1 <- choose(minBound,maxBound)
    n2 <- choose(minBound,maxBound)
    n3 <- choose(minBound,maxBound)
    n4 <- choose(minBound,maxBound)
    n5 <- choose(minBound,maxBound)
    let unpackedUUID = UnpackedUUID {
          time_low = tl
        , time_mid = tm
        , time_hi_and_version = th
        , clock_seq_hi_res = cshr
        , clock_seq_low = csl
        , node_0 = n0
        , node_1 = n1
        , node_2 = n2
        , node_3 = n3
        , node_4 = n4
        , node_5 = n5
        } 
    return $ ArbUUID (pack unpackedUUID)

instance Arbitrary RegularIdentifier where
  arbitrary = liftM RegularIdentifier arbitrary
