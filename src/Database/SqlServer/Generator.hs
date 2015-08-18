module Database.SqlServer.Generator where

import Database.SqlServer.Create.Identifier (RegularIdentifier)
import Database.SqlServer.Create.Table (Table)
import Database.SqlServer.Create.View (View)
import Database.SqlServer.Create.Sequence (Sequence)
import Database.SqlServer.Create.Procedure (Procedure)
import Database.SqlServer.Create.User (User,Role)
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
import Database.SqlServer.Create.Entity

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

generateExamples :: (Show a) => Int -> Gen a -> IO [a]
generateExamples m a = generate (sequence [resize n a | n <- [0..m] ])

saveExamples :: (Show a) => FilePath -> [a] -> IO ()
saveExamples p xs = writeFile p (unlines $ map show xs)

data GenerateOptions = GenerateOptions
  {
    size :: Int
  , seed :: Int
  }

generateEntity :: (Arbitrary a, Entity a) => GenerateOptions -> a
generateEntity go = unGen arbitrary (mkQCGen (seed go)) (size go)
