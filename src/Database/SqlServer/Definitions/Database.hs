{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.SqlServer.Definitions.Database where

import Database.SqlServer.Definitions.Identifiers (RegularIdentifier,renderRegularIdentifier)
import Database.SqlServer.Definitions.Table (Table)
import Database.SqlServer.Definitions.Sequence (Sequence)
import Database.SqlServer.Definitions.Procedure (Procedure)
import Database.SqlServer.Definitions.Queue (QueueDefinition)
import Database.SqlServer.Definitions.Certificate (CertificateDefinition)
import Database.SqlServer.Definitions.Login (LoginDefinition)
import Database.SqlServer.Definitions.User (UserDefinition,RoleDefinition)
import Database.SqlServer.Definitions.FullTextCatalog (FullTextCatalogDefinition)
import Database.SqlServer.Definitions.FullTextStopList (FullTextStopListDefinition)
import Database.SqlServer.Definitions.Function (Function)
import Database.SqlServer.Definitions.Credential (CredentialDefinition)
import Database.SqlServer.Definitions.MessageType (MessageTypeDefinition)
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
                          , tableDefinitions :: [Table]
                          , sequenceDefinitions :: [Sequence]
                          , procedureDefinitions :: [Procedure]
                          , functionDefinitions :: [Function]
                          , queueDefinitions :: [QueueDefinition]
                          , certificateDefinitions :: [CertificateDefinition]
                          , userDefinitions :: [UserDefinition]
                          , roleDefinitions :: [RoleDefinition]
                          , loginDefinitions :: [LoginDefinition]
                          , fullTextCatalogDefinitions :: [FullTextCatalogDefinition]
                          , fullTextStopListDefinitions :: [FullTextStopListDefinition]
                          , credentials :: [CredentialDefinition]
                          , messageDefinitions :: [MessageTypeDefinition]
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
                               renderNamedEntities (tableDefinitions dd) $+$
                               renderNamedEntities (sequenceDefinitions dd) $+$
                               renderNamedEntities (procedureDefinitions dd) $+$
                               renderNamedEntities (functionDefinitions dd) $+$
                               renderNamedEntities (queueDefinitions dd) $+$
                               renderNamedEntities (certificateDefinitions dd) $+$
                               renderNamedEntities (userDefinitions dd) $+$
                               renderNamedEntities (roleDefinitions dd) $+$
                               renderNamedEntities (loginDefinitions dd) $+$
                               renderNamedEntities (fullTextCatalogDefinitions dd) $+$
                               renderNamedEntities (fullTextStopListDefinitions dd) $+$
                               renderNamedEntities (credentials dd) $+$
                               renderNamedEntities (messageDefinitions dd) $+$ 
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
