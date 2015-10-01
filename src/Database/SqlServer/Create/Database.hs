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
  , showSequences :: Bool
  , showProcedures :: Bool
  , showFunctions :: Bool
  , showUsers :: Bool
  , showRoles :: Bool
  , showFullTextCatalog :: Bool
  , showFullTextStopList :: Bool
  , showCredential :: Bool
  , showMessageType :: Bool
  , showBrokerPriority :: Bool
  , showPartitionFunction :: Bool
  } deriving (Show)

defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions
  True True True True True True True True True True True True True

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
  renderEntitiesIf (showSequences ro) (sequences dd) $+$
  renderEntitiesIf (showProcedures ro) (procedures dd) $+$
  renderEntitiesIf (showFunctions ro) (functions dd) $+$
  renderEntitiesIf (showUsers ro) (users dd) $+$
  renderEntitiesIf (showRoles ro) (roles dd) $+$
  renderEntitiesIf (showFullTextCatalog ro) (fullTextCatalogs dd) $+$
  renderEntitiesIf (showFullTextStopList ro) (fullTextStopLists dd) $+$
  renderEntitiesIf (showCredential ro) (credentials dd) $+$
  renderEntitiesIf (showMessageType ro) (messages dd) $+$
  renderEntitiesIf (showBrokerPriority ro) (brokerPriorities dd) $+$
  renderEntitiesIf (showPartitionFunction ro) (partitionFunctions dd)
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
