{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.SqlServer.Types.Database where

import Database.SqlServer.Types.Identifiers (RegularIdentifier,renderRegularIdentifier)
import Database.SqlServer.Types.Table (TableDefinition)
import Database.SqlServer.Types.Sequence (SequenceDefinition)
import Database.SqlServer.Types.Procedure (ProcedureDefinition)
import Database.SqlServer.Types.Queue (QueueDefinition)
import Database.SqlServer.Types.Certificate (CertificateDefinition)
import Database.SqlServer.Types.Login (LoginDefinition)
import Database.SqlServer.Types.User (UserDefinition)
import Database.SqlServer.Types.FullTextCatalog (FullTextCatalogDefinition)
import Database.SqlServer.Types.Entity

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
                          , tableDefinitions :: [TableDefinition]
                          , sequenceDefinitions :: [SequenceDefinition]
                          , procedureDefinitions :: [ProcedureDefinition]
                          , queueDefinitions :: [QueueDefinition]
                          , certificateDefinitions :: [CertificateDefinition]
                          , userDefinitions :: [UserDefinition]
                          , loginDefinitions :: [LoginDefinition]
                          , fullTextCatalogDefinitions :: [FullTextCatalogDefinition]
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
                               renderNamedEntities (queueDefinitions dd) $+$
                               renderNamedEntities (certificateDefinitions dd) $+$
                               renderNamedEntities (userDefinitions dd) $+$
                               renderNamedEntities (loginDefinitions dd) $+$
                               renderNamedEntities (fullTextCatalogDefinitions dd) $+$
                               text "GO"
  where
    dbName = renderRegularIdentifier (databaseName dd)

derive makeArbitrary ''DatabaseDefinition
    
dumpExamples :: Int -> FilePath -> IO ()
dumpExamples m p = do
  x <- generate (sequence [resize n (arbitrary :: Gen DatabaseDefinition) | n <- [0..m] ])
  writeFile p (unlines $ map (render . renderDatabaseDefinition) x)

instance Show DatabaseDefinition where
  show = render . renderDatabaseDefinition

seededDatabase :: Int -> Int -> DatabaseDefinition
seededDatabase seed = unGen arbitrary (mkQCGen seed) 
