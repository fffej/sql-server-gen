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
  , objectsPerType :: Int
  } deriving (Show)

defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions
  True True True True True True True True True True True True True 10000000

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

renderDatabase :: RenderOptions -> Database -> Doc
renderDatabase ro dd =
  text "USE master" $+$
  text "GO" $+$
  text "CREATE DATABASE" <+> dbName $+$
  text "GO" $+$
  text "USE" <+> dbName $+$
  renderMasterKey (masterKey dd) $+$
  renderEntitiesIf (showTables ro) (take n $ tables dd) $+$
  renderEntitiesIf (showViews ro) (take n $ views dd) $+$
  renderEntitiesIf (showSequences ro) (take n $ sequences dd) $+$
  renderEntitiesIf (showProcedures ro) (take n $ procedures dd) $+$
  renderEntitiesIf (showFunctions ro) (take n $ functions dd) $+$
  renderEntitiesIf (showUsers ro) (take n $ users dd) $+$
  renderEntitiesIf (showRoles ro) (take n $ roles dd) $+$
  renderEntitiesIf (showFullTextCatalog ro) (take n $ fullTextCatalogs dd) $+$
  renderEntitiesIf (showFullTextStopList ro) (take n $ fullTextStopLists dd) $+$
  renderEntitiesIf (showCredential ro) (take n $ credentials dd) $+$
  renderEntitiesIf (showMessageType ro) (take n $ messages dd) $+$
  renderEntitiesIf (showBrokerPriority ro) (take n $ brokerPriorities dd) $+$
  renderEntitiesIf (showPartitionFunction ro) (take n $ partitionFunctions dd)
  where
    dbName = renderName dd
    n = objectsPerType ro

instance Arbitrary Database where
  arbitrary = Database <$>
    arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    infiniteListOf arbitrary <*>
    arbitrary

instance Show Database where
  show = show . render
