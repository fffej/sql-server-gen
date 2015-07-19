{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.SqlServer.Definitions.Database where

import Database.SqlServer.Definitions.Identifier (RegularIdentifier,renderRegularIdentifier)
import Database.SqlServer.Definitions.Table (Table)
import Database.SqlServer.Definitions.Sequence (Sequence)
import Database.SqlServer.Definitions.Procedure (Procedure)
import Database.SqlServer.Definitions.Queue (Queue)
import Database.SqlServer.Definitions.Certificate (Certificate)
import Database.SqlServer.Definitions.Login (Login)
import Database.SqlServer.Definitions.User (User,Role)
import Database.SqlServer.Definitions.FullTextCatalog (FullTextCatalog)
import Database.SqlServer.Definitions.FullTextStopList (FullTextStopList)
import Database.SqlServer.Definitions.Function (Function)
import Database.SqlServer.Definitions.Credential (Credential)
import Database.SqlServer.Definitions.MessageType (MessageType)
import Database.SqlServer.Definitions.Entity

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

import Text.PrettyPrint
import Data.DeriveTH

data MasterKey = MasterKey

derive makeArbitrary ''MasterKey

instance Entity MasterKey where
  toDoc MasterKey = text "CREATE MASTER KEY ENCRYPTION BY PASSWORD = 'weKKjwehg252t!!'" $+$
                    text "GO"
                        
data DatabaseDefinition = DatabaseDefinition
                          {
                            databaseName :: RegularIdentifier
                          , tables :: [Table]
                          , sequences :: [Sequence]
                          , procedures :: [Procedure]
                          , functions :: [Function]
                          , queues :: [Queue]
                          , certificates :: [Certificate]
                          , users :: [User]
                          , roles :: [Role]
                          , logins :: [Login]
                          , fullTextCatalogs :: [FullTextCatalog]
                          , fullTextStopLists :: [FullTextStopList]
                          , credentials :: [Credential]
                          , messages :: [MessageType]
                          , masterKey :: MasterKey
                          }

renderNamedEntities :: Entity a => [a] -> Doc
renderNamedEntities xs = vcat (map toDoc xs)

renderDatabaseDefinition :: DatabaseDefinition -> Doc
renderDatabaseDefinition  dd = text "USE master" $+$
                               text "GO" $+$
                               text "CREATE DATABASE" <+> dbName $+$
                               text "GO" $+$
                               text "USE" <+> dbName $+$
                               toDoc (masterKey dd) $+$
                               renderNamedEntities (tables dd) $+$
                               renderNamedEntities (sequences dd) $+$
                               renderNamedEntities (procedures dd) $+$
                               renderNamedEntities (functions dd) $+$
                               renderNamedEntities (queues dd) $+$
                               renderNamedEntities (certificates dd) $+$
                               renderNamedEntities (users dd) $+$
                               renderNamedEntities (roles dd) $+$
                               renderNamedEntities (logins dd) $+$
                               renderNamedEntities (fullTextCatalogs dd) $+$
                               renderNamedEntities (fullTextStopLists dd) $+$
                               renderNamedEntities (credentials dd) $+$
                               renderNamedEntities (messages dd) $+$ 
                               text "GO"
  where
    dbName = renderRegularIdentifier (databaseName dd)

derive makeArbitrary ''DatabaseDefinition

dumpExamples :: Int -> FilePath -> IO ()
dumpExamples m p = do
  x <- generate (sequence [resize n (arbitrary :: Gen DatabaseDefinition) | n <- [0..m] ])
  writeFile p (unlines $ map show x)

instance Show DatabaseDefinition where
  show = render . renderDatabaseDefinition

seededDatabase :: Int -> Int -> DatabaseDefinition
seededDatabase seed = unGen arbitrary (mkQCGen seed) 
