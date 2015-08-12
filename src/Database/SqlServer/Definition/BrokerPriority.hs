module Database.SqlServer.Definition.BrokerPriority
       (
         BrokerPriority
       ) where

import Database.SqlServer.Definition.Service (Service)
import Database.SqlServer.Definition.Contract (Contract)
import Database.SqlServer.Definition.Identifier hiding (unwrap)
import Database.SqlServer.Definition.Entity

import Test.QuickCheck
import Text.PrettyPrint

data PriorityLevel = PriorityLevel Int

instance Arbitrary PriorityLevel where
  arbitrary = do
    x <- choose (1,10)
    return (PriorityLevel x)

data BrokerPriority = BrokerPriority
  {
    priorityName :: RegularIdentifier
  , contractName :: Maybe Contract
  , localServiceName :: Maybe Service
  , remoteServiceName :: Maybe RegularIdentifier
  , priorityLevel :: Maybe PriorityLevel
  }

instance Arbitrary BrokerPriority where
  arbitrary = BrokerPriority <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

renderMaybeOrAny :: Maybe RegularIdentifier -> Doc
renderMaybeOrAny = maybe (text "ANY") (quotes . renderRegularIdentifier)

renderName' :: Entity a => Maybe a -> Doc
renderName' = maybe (text "ANY") renderName

renderPriorityLevel :: Maybe PriorityLevel -> Doc
renderPriorityLevel = maybe (text "DEFAULT") (\(PriorityLevel z) -> int z)

renderOptions :: BrokerPriority -> Doc
renderOptions b = vcat $ punctuate comma
  [
    text "CONTRACT_NAME =" <+> renderName' (contractName b)
  , text "LOCAL_SERVICE_NAME =" <+> renderName' (localServiceName b)
  , text "REMOTE_SERVICE_NAME =" <+> renderMaybeOrAny (remoteServiceName b)
  , text "PRIORITY_LEVEL =" <+> renderPriorityLevel (priorityLevel b)
  ]

renderPrerequisites :: Entity a => Maybe a -> Doc
renderPrerequisites = maybe empty toDoc

instance Entity BrokerPriority where
  name = priorityName
  toDoc b = renderPrerequisites (contractName b) $+$
            renderPrerequisites (localServiceName b) $+$
            text "GO" $+$
            text "CREATE BROKER PRIORITY" <+> renderName b $+$
            text "FOR CONVERSATION" $+$
            text "SET" <+> parens (renderOptions b) <> text ";" $+$
            text "GO\n"

instance Show BrokerPriority where
  show = show . toDoc
