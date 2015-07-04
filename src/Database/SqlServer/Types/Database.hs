module Database.SqlServer.Types.Database where

import Database.SqlServer.Types.Identifiers (RegularIdentifier,renderRegularIdentifier)
import Database.SqlServer.Types.Table (TableDefinition,renderTableDefinition)
import Database.SqlServer.Types.Properties (NamedEntity,name)
import Database.SqlServer.Types.Sequence (SequenceDefinition,renderSequenceDefinition)
import Database.SqlServer.Types.Procedure
import Database.SqlServer.Types.Queue

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Control.Monad
import qualified Data.Set as S


import Text.PrettyPrint

newtype TableDefinitions = TableDefinitions (S.Set TableDefinition)
newtype SequenceDefinitions = SequenceDefinitions (S.Set SequenceDefinition)
newtype ProcedureDefinitions = ProcedureDefinitions (S.Set ProcedureDefinition)
newtype QueueDefinitions = QueueDefinitions (S.Set QueueDefinition)

data DatabaseDefinition = DatabaseDefinition
                          {
                            databaseName :: RegularIdentifier
                          , tableDefinitions :: TableDefinitions
                          , sequenceDefinitions :: SequenceDefinitions
                          , procedureDefinitions :: ProcedureDefinitions
                          , queueDefinitions :: QueueDefinitions
                          }

tableNames :: TableDefinitions -> S.Set RegularIdentifier
tableNames (TableDefinitions xs) = names xs

sequenceNames :: SequenceDefinitions -> S.Set RegularIdentifier
sequenceNames (SequenceDefinitions xs) = names xs

procedureNames :: ProcedureDefinitions -> S.Set RegularIdentifier
procedureNames (ProcedureDefinitions xs) = names xs

names :: NamedEntity a => S.Set a -> S.Set RegularIdentifier
names xs = S.map name xs

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
  arbitrary = liftM TableDefinitions $ (liftM S.fromList $ listOf1 arbitrary)

usesUnreservedNames :: NamedEntity a => S.Set RegularIdentifier -> [a] -> Bool
usesUnreservedNames reserved = \x -> not $ any (\a -> (name a) `S.member` reserved) x

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
    sequences <- liftM SequenceDefinitions $ makeArbitrarySeqs
    procs <- liftM ProcedureDefinitions $ makeArbitraryProcs
    queues <- liftM QueueDefinitions $ makeArbitraryQueues 
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
