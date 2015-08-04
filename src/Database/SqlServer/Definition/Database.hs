{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.SqlServer.Definition.Database where

import Database.SqlServer.Definition.Identifier (RegularIdentifier,renderRegularIdentifier)
import Database.SqlServer.Definition.Table (Table)
import Database.SqlServer.Definition.View (View)
import Database.SqlServer.Definition.Sequence (Sequence)
import Database.SqlServer.Definition.Procedure (Procedure)
import Database.SqlServer.Definition.User (User,Role)
import Database.SqlServer.Definition.FullTextCatalog (FullTextCatalog)
import Database.SqlServer.Definition.FullTextStopList (FullTextStopList)
import Database.SqlServer.Definition.Function (Function)
import Database.SqlServer.Definition.Credential (Credential)
import Database.SqlServer.Definition.MessageType (MessageType)
import Database.SqlServer.Definition.BrokerPriority (BrokerPriority)
import Database.SqlServer.Definition.PartitionFunction (PartitionFunction)
import Database.SqlServer.Definition.Entity


import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

import Text.PrettyPrint
import Data.DeriveTH

data MasterKey = MasterKey

derive makeArbitrary ''MasterKey

renderMasterKey :: MasterKey -> Doc
renderMasterKey _ = text "CREATE MASTER KEY ENCRYPTION BY PASSWORD = 'weKKjwehg252t!!'"
           $+$  text "GO"
                        
data DatabaseDefinition = DatabaseDefinition
                          {
                            databaseName :: RegularIdentifier
                          , tables :: [Table]
                          , views :: [View]
                          , sequences :: [Sequence]
                          , procedures :: [Procedure]
                          , functions :: [Function]
                          , users :: [User]
                          , roles :: [Role]
                          , fullTextCatalogs :: [FullTextCatalog]
                          , fullTextStopLists :: [FullTextStopList]
                          , credentials :: [Credential]
                          , messages :: [MessageType]
                          , brokerPriorities :: [BrokerPriority]
                          , partitionFunctions :: [PartitionFunction]
                          , masterKey :: MasterKey
                          }

instance Entity DatabaseDefinition where
  name = databaseName
  toDoc = renderDatabaseDefinition

renderNamedEntities :: Entity a => [a] -> Doc
renderNamedEntities xs = vcat (map toDoc xs)

renderDatabaseDefinition :: DatabaseDefinition -> Doc
renderDatabaseDefinition  dd = text "USE master" $+$
                               text "GO" $+$
                               text "CREATE DATABASE" <+> dbName $+$
                               text "GO" $+$
                               text "USE" <+> dbName $+$
                               renderMasterKey (masterKey dd) $+$
                               renderNamedEntities (tables dd) $+$
                               renderNamedEntities (views dd) $+$
                               renderNamedEntities (sequences dd) $+$
                               renderNamedEntities (procedures dd) $+$
                               renderNamedEntities (functions dd) $+$
                               renderNamedEntities (users dd) $+$
                               renderNamedEntities (roles dd) $+$
                               renderNamedEntities (fullTextCatalogs dd) $+$
                               renderNamedEntities (fullTextStopLists dd) $+$
                               renderNamedEntities (credentials dd) $+$
                               renderNamedEntities (messages dd) $+$
                               renderNamedEntities (brokerPriorities dd) $+$
                               renderNamedEntities (partitionFunctions dd) $+$
                               text "GO"
  where
    dbName = renderRegularIdentifier (databaseName dd)

derive makeArbitrary ''DatabaseDefinition

generateExamples :: (Show a) => Int -> Gen a -> IO [a]
generateExamples m a = generate (sequence [resize n a | n <- [0..m] ])

saveExamples :: (Show a) => FilePath -> [a] -> IO ()
saveExamples p xs = writeFile p (unlines $ map show xs)

instance Show DatabaseDefinition where
  show = render . renderDatabaseDefinition

seededDatabase :: Int -> Int -> DatabaseDefinition
seededDatabase seed = unGen arbitrary (mkQCGen seed) 
