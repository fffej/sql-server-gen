{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Definition.View
       (
         View
       ) where

import Database.SqlServer.Definition.Identifier
import Database.SqlServer.Definition.Entity

import Test.QuickCheck

data View = View
  {
    viewName :: RegularIdentifier
  }

instance Arbitrary View where
  arbitrary = do
    n <- arbitrary
    return (View n)

instance Entity View where
  name = viewName
  toDoc = undefined
