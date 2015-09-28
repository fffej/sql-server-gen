module Database.SqlServer.Create.Database where

import Database.SqlServer.Create.Identifier (RegularIdentifier)
import Database.SqlServer.Create.Table (Table)
import Database.SqlServer.Create.View (View)
import Database.SqlServer.Create.Sequence (Sequence)
import Database.SqlServer.Create.Procedure (Procedure)
import Database.SqlServer.Create.User (User, Role)
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
import Text.PrettyPrint hiding (render)

data MasterKey = MasterKey

instance Arbitrary MasterKey where
  arbitrary = return MasterKey

renderMasterKey :: MasterKey -> Doc
renderMasterKey _ =
  text "CREATE MASTER KEY ENCRYPTION BY PASSWORD = 'weKKjwehg252t!!'" $+$
  text "GO"

data RenderOptions = RenderOptions
  {
    showTables :: Bool
  , showViews :: Bool
  } deriving (Show)

defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions True True

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
  render = renderDatabase defaultRenderOptions

-- Todo rename to renderentities
renderNamedEntities :: Entity a => [a] -> Doc
renderNamedEntities xs = vcat (map render xs)

renderEntitiesIf :: Entity a => Bool -> [a] -> Doc
renderEntitiesIf False _ = empty
renderEntitiesIf True xs = renderNamedEntities xs

-- Note that some parts aren't rendered to avoid bloat
renderDatabase :: RenderOptions -> Database -> Doc
renderDatabase ro dd =
  text "USE master" $+$
  text "GO" $+$
  text "CREATE DATABASE" <+> dbName $+$
  text "GO" $+$
  text "USE" <+> dbName $+$
  renderMasterKey (masterKey dd) $+$
  renderEntitiesIf (showTables ro) (tables dd) $+$
  renderEntitiesIf (showViews ro) (views dd) $+$
  renderEntitiesIf True (sequences dd) $+$
  renderEntitiesIf True (procedures dd) $+$
  renderEntitiesIf True (functions dd) $+$
  renderEntitiesIf True (users dd) $+$
  renderEntitiesIf True (roles dd) $+$
  renderEntitiesIf True (fullTextCatalogs dd) $+$
  renderEntitiesIf True (fullTextStopLists dd) $+$
  renderEntitiesIf True (credentials dd) $+$
  renderEntitiesIf True (messages dd) $+$
  renderEntitiesIf True (brokerPriorities dd) $+$
  renderEntitiesIf True (partitionFunctions dd)
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

instance Show Database where
  show = show . render
