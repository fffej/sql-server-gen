module Database.SqlServer.Create.SecurityPolicy
       (
         SecurityPolicy
       ) where

import Database.SqlServer.Create.Identifier
import Database.SqlServer.Create.Table
import Database.SqlServer.Create.Entity

import Test.QuickCheck


data SecurityPolicy = SecurityPolicy
  {
    policyName :: RegularIdentifier
  , tables :: [([Column], Table)]
  , state :: Maybe Bool
  , forReplication :: Bool
  }

instance Arbitrary SecurityPolicy where
  arbitrary = undefined

instance Entity SecurityPolicy where
  name = policyName
  render = undefined

instance Show SecurityPolicy where
  show = show . render