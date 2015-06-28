{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.Database where

import Database.SqlServer.Types.Identifiers (RegularIdentifier,renderRegularIdentifier)
import Database.SqlServer.Types.Table (TableDefinition,renderTableDefinition,tableName)
import Database.SqlServer.Types.Properties (validIdentifiers)
import Database.SqlServer.Types.Sequence (SequenceDefinition,renderSequenceDefinition)
import Database.SqlServer.Types.Queue hiding (procedureName)
import Database.SqlServer.Types.Procedure

import Test.QuickCheck
import Control.Monad
import qualified Data.Set as S

import Text.PrettyPrint

import Data.DeriveTH

newtype TableDefinitions = TableDefinitions [TableDefinition]

newtype SequenceDefinitions = SequenceDefinitions [SequenceDefinition]

newtype ProcedureDefinitions = ProcedureDefinitions [ProcedureDefinition]

data DatabaseDefinition = DatabaseDefinition
                          {
                            databaseName :: RegularIdentifier
                          , tableDefinitions :: TableDefinitions
                          , sequenceDefinitions :: SequenceDefinitions
                          , procedureDefinitions :: ProcedureDefinitions
                          }

tableNames :: TableDefinitions -> S.Set RegularIdentifier
tableNames (TableDefinitions xs) = S.fromList (map tableName xs)

renderTableDefinitions :: TableDefinitions -> Doc
renderTableDefinitions (TableDefinitions xs) = vcat (map renderTableDefinition xs)

renderSequenceDefinitions :: SequenceDefinitions -> Doc
renderSequenceDefinitions (SequenceDefinitions xs) = vcat (map renderSequenceDefinition xs)

renderProcedureDefinitions :: ProcedureDefinitions -> Doc
renderProcedureDefinitions (ProcedureDefinitions xs) = vcat (map renderProcedureDefinition xs)

renderDatabaseDefinition :: DatabaseDefinition -> Doc
renderDatabaseDefinition  dd = text "USE master" $+$
                               text "GO" $+$
                               text "CREATE DATABASE" <+> dbName $+$
                               text "GO" $+$
                               text "USE" <+> dbName $+$
                               renderTableDefinitions (tableDefinitions dd) $+$
                               renderSequenceDefinitions (sequenceDefinitions dd) $+$
                               renderProcedureDefinitions (procedureDefinitions dd)
  where
    dbName = renderRegularIdentifier (databaseName dd)

instance Arbitrary TableDefinitions where
  arbitrary = liftM TableDefinitions $ (listOf1 arbitrary `suchThat` validIdentifiers)

instance Arbitrary SequenceDefinitions where
  arbitrary = liftM SequenceDefinitions $ (listOf1 arbitrary `suchThat` validIdentifiers)

makeArbitraryProcs :: S.Set RegularIdentifier -> Gen [ProcedureDefinition]
makeArbitraryProcs reserved = listOf arbitrary `suchThat` (\x -> not $ any (\a -> (procedureName a) `S.member` reserved) x)

instance Arbitrary DatabaseDefinition where
  arbitrary = do
    dbName <- arbitrary
    tables <- arbitrary
    sequences <- arbitrary
    procs <- liftM ProcedureDefinitions $ makeArbitraryProcs (tableNames tables)
    return $ DatabaseDefinition dbName tables sequences procs
   

dumpExamples :: Int -> FilePath -> IO ()
dumpExamples m p = do
  x <- generate (sequence [resize n (arbitrary :: Gen DatabaseDefinition) | n <- [0..m] ])
  writeFile p (unlines $ map (render . renderDatabaseDefinition) x)
