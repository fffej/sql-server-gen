{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.Database where

import Database.SqlServer.Types.Identifiers (RegularIdentifier,renderRegularIdentifier)
import Database.SqlServer.Types.Table (TableDefinition,renderTableDefinition)
import Database.SqlServer.Types.Properties (validIdentifiers)
import Database.SqlServer.Types.Sequence (SequenceDefinition,renderSequenceDefinition)
import Database.SqlServer.Types.Queue
import Database.SqlServer.Types.Procedure

import Test.QuickCheck
import Control.Monad

import Text.PrettyPrint

import Data.DeriveTH

newtype TableDefinitions = TableDefinitions [TableDefinition]

newtype SequenceDefinitions = SequenceDefinitions [SequenceDefinition]

data DatabaseDefinition = DatabaseDefinition
                          {
                            databaseName :: RegularIdentifier
                          , tableDefinitions :: TableDefinitions
                          , sequenceDefinitions :: SequenceDefinitions
                          }

renderTableDefinitions :: TableDefinitions -> Doc
renderTableDefinitions (TableDefinitions xs) = vcat (map renderTableDefinition xs)

renderSequenceDefinitions :: SequenceDefinitions -> Doc
renderSequenceDefinitions (SequenceDefinitions xs) = vcat (map renderSequenceDefinition xs)


renderDatabaseDefinition :: DatabaseDefinition -> Doc
renderDatabaseDefinition  dd = text "USE master" $+$
                               text "GO" $+$
                               text "CREATE DATABASE" <+> dbName $+$
                               text "GO" $+$
                               text "USE" <+> dbName $+$
                               renderTableDefinitions (tableDefinitions dd) $+$
                               renderSequenceDefinitions (sequenceDefinitions dd)
  where
    dbName = renderRegularIdentifier (databaseName dd)

instance Arbitrary TableDefinitions where
  arbitrary = liftM TableDefinitions $ (listOf1 arbitrary `suchThat` validIdentifiers)

instance Arbitrary SequenceDefinitions where
  arbitrary = liftM SequenceDefinitions $ (listOf1 arbitrary `suchThat` validIdentifiers)

derive makeArbitrary ''DatabaseDefinition

dumpExamples :: Int -> FilePath -> IO ()
dumpExamples m p = do
  x <- generate (sequence [resize n (arbitrary :: Gen ProcedureDefinition) | n <- [0..m] ])
  writeFile p (unlines $ map (render . renderProcedureDefinition) x)
