module Database.SqlServer.Generator where

import Database.SqlServer.Create.Identifier (RegularIdentifier)
import Database.SqlServer.Create.Table (Table)
import Database.SqlServer.Create.View (View)
import Database.SqlServer.Create.Sequence (Sequence)
import Database.SqlServer.Create.Procedure (Procedure)
import Database.SqlServer.Create.User (User, Role)
import Database.SqlServer.Create.FullTextCatalog (FullTextCatalog)
import Database.SqlServer.Create.FullTextStopList (FullTextStopList)
import Database.SqlServer.Create.Function (Function)
import Database.SqlServer.Create.Credential (Credential)
import Database.SqlServer.Create.MessageType (MessageType)
import Database.SqlServer.Create.BrokerPriority (BrokerPriority)
import Database.SqlServer.Create.PartitionFunction (PartitionFunction)
import Database.SqlServer.Create.Contract (Contract)
import Database.SqlServer.Create.Login (Login)
import Database.SqlServer.Create.Certificate (Certificate)
import Database.SqlServer.Create.Trigger (Trigger)
import Database.SqlServer.Create.SecurityPolicy (SecurityPolicy)
import Database.SqlServer.Create.Database (Database, RenderOptions, defaultRenderOptions)
import Database.SqlServer.Create.Entity

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

import System.Random

-- GHC doesn't flag warnings on things beginning _
_redundantImport :: (
  RegularIdentifier, Login, Certificate, Contract, PartitionFunction,
  MessageType, BrokerPriority, Credential, Function, FullTextStopList,
  FullTextCatalog, User, Role, Sequence, Table, View, Procedure,
  Trigger, SecurityPolicy, Database
  )
_redundantImport = undefined

asScript :: (Show a) => [a] -> String
asScript a = unlines $ map show a

data GenerateOptions = GenerateOptions
  {
    complexity :: Int
  , approximateObjectsPerType :: Int
  , seed :: Int
  , excludeTypes :: RenderOptions
  }

defaultGenerateOptions :: Int -> Int -> Int -> GenerateOptions
defaultGenerateOptions complexity' seed' approximateObjectsPerType' = GenerateOptions
  {
    complexity = complexity'
  , approximateObjectsPerType = approximateObjectsPerType'
  , seed = seed'
  , excludeTypes = defaultRenderOptions
  }

generateEntity :: (Arbitrary a, Entity a) => GenerateOptions -> a
generateEntity go = unGen arbitrary (mkQCGen (seed go)) (complexity go)

generateEntities :: (Arbitrary a, Entity a) => GenerateOptions -> [a]
generateEntities go = map (\ x -> generateEntity (go { seed = x })) (randoms g)
  where
    g = mkQCGen (seed go)
