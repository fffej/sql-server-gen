{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.FullTextStopList where

import Database.SqlServer.Types.Identifiers
import Database.SqlServer.Types.Entity

import Test.QuickCheck
import Text.PrettyPrint
import Data.DeriveTH

data FullTextStopListDefinition = FullTextStopListDefinition
  {
    stoplistName :: RegularIdentifier
  , sourceStopList :: Maybe FullTextStopListDefinition
  -- TODO owners
  }

derive makeArbitrary ''FullTextStopListDefinition

instance Entity FullTextStopListDefinition where
  toDoc = undefined
