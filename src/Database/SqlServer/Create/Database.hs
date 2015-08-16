module Database.SqlServer.Create.Database where

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

import Text.PrettyPrint hiding (render)

data MasterKey = MasterKey

instance Arbitrary MasterKey where
  arbitrary = return MasterKey

renderMasterKey :: MasterKey -> Doc
renderMasterKey _ = text "CREATE MASTER KEY ENCRYPTION BY PASSWORD = 'weKKjwehg252t!!'"
           $+$  text "GO"
                        
data Database = Database
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
  , logins :: [Login]
  , contracts :: [Contract]
  , certificates :: [Certificate]
  , masterKey :: MasterKey
}

instance Entity Database where
  name = databaseName
  render = renderDatabase

renderNamedEntities :: Entity a => [a] -> Doc
renderNamedEntities xs = vcat (map render xs)

-- Note that some parts aren't rendered to avoid bloat
renderDatabase :: Database -> Doc
renderDatabase dd = text "USE master" $+$
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
    dbName = renderName dd

instance Arbitrary Database where
  arbitrary = Database <$>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary
    
generateExamples :: (Show a) => Int -> Gen a -> IO [a]
generateExamples m a = generate (sequence [resize n a | n <- [0..m] ])

saveExamples :: (Show a) => FilePath -> [a] -> IO ()
saveExamples p xs = writeFile p (unlines $ map show xs)

instance Show Database where
  show = show . render

data GenerateOptions = GenerateOptions
  {
    size :: Int
  , seed :: Int
  }

generateEntity :: (Arbitrary a, Entity a) => GenerateOptions -> a
generateEntity go = unGen arbitrary (mkQCGen (seed go)) (size go)
