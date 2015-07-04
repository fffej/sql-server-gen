module Database.SqlServer.Types.Database where

import Database.SqlServer.Types.Identifiers (RegularIdentifier,renderRegularIdentifier)
import Database.SqlServer.Types.Table (TableDefinition,renderTableDefinition)
import Database.SqlServer.Types.Sequence (SequenceDefinition,renderSequenceDefinition)
import Database.SqlServer.Types.Procedure
import Database.SqlServer.Types.Queue

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Control.Monad

import Text.PrettyPrint

newtype TableDefinitions = TableDefinitions [TableDefinition]
newtype SequenceDefinitions = SequenceDefinitions [SequenceDefinition]
newtype ProcedureDefinitions = ProcedureDefinitions [ProcedureDefinition]
newtype QueueDefinitions = QueueDefinitions [QueueDefinition]

data DatabaseDefinition = DatabaseDefinition
                          {
                            databaseName :: RegularIdentifier
                          , tableDefinitions :: TableDefinitions
                          , sequenceDefinitions :: SequenceDefinitions
                          , procedureDefinitions :: ProcedureDefinitions
                          , queueDefinitions :: QueueDefinitions
                          }

renderTableDefinitions :: TableDefinitions -> Doc
renderTableDefinitions (TableDefinitions xs) = vcat (map renderTableDefinition (S.toList xs))

renderSequenceDefinitions :: SequenceDefinitions -> Doc
renderSequenceDefinitions (SequenceDefinitions xs) = vcat (map renderSequenceDefinition (S.toList xs))

renderProcedureDefinitions :: ProcedureDefinitions -> Doc
renderProcedureDefinitions (ProcedureDefinitions xs) = vcat (map renderProcedureDefinition (S.toList xs))

renderQueueDefinitions :: QueueDefinitions -> Doc
renderQueueDefinitions (QueueDefinitions xs) = vcat (map renderQueueDefinition (S.toList xs))

renderDatabaseDefinition :: DatabaseDefinition -> Doc
renderDatabaseDefinition  dd = text "USE master" $+$
                               text "GO" $+$
                               text "CREATE DATABASE" <+> dbName $+$
                               text "GO" $+$
                               text "USE" <+> dbName $+$
                               renderTableDefinitions (tableDefinitions dd) $+$
                               renderSequenceDefinitions (sequenceDefinitions dd) $+$
                               renderProcedureDefinitions (procedureDefinitions dd) $+$
                               renderQueueDefinitions (queueDefinitions dd)
  where
    dbName = renderRegularIdentifier (databaseName dd)

instance Arbitrary TableDefinitions where
  arbitrary = liftM (TableDefinitions . S.fromList) (listOf1 arbitrary)

-- TODO remove duplication
makeArbitraryProcs :: Gen (S.Set ProcedureDefinition)
makeArbitraryProcs = liftM S.fromList arbitrary

makeArbitrarySeqs :: Gen (S.Set SequenceDefinition)
makeArbitrarySeqs  = liftM S.fromList arbitrary

makeArbitraryQueues :: Gen (S.Set QueueDefinition)
makeArbitraryQueues = liftM S.fromList arbitrary

instance Arbitrary DatabaseDefinition where
  arbitrary = do
    dbName <- arbitrary
    tables <- arbitrary
    sequences <- liftM SequenceDefinitions makeArbitrarySeqs
    procs <- liftM ProcedureDefinitions makeArbitraryProcs
    queues <- liftM QueueDefinitions makeArbitraryQueues 
    return $ DatabaseDefinition
      {
        databaseName = dbName
      , tableDefinitions = tables
      , sequenceDefinitions = sequences
      , procedureDefinitions = procs
      , queueDefinitions = queues
      }
   
dumpExamples :: Int -> FilePath -> IO ()
dumpExamples m p = do
  x <- generate (sequence [resize n (arbitrary :: Gen DatabaseDefinition) | n <- [0..m] ])
  writeFile p (unlines $ map (render . renderDatabaseDefinition) x)

instance Show DatabaseDefinition where
  show = render . renderDatabaseDefinition

seededDatabase :: Int -> Int -> DatabaseDefinition
seededDatabase seed = unGen arbitrary (mkQCGen seed) 
