module Database.SqlServer.Create.Contract
       (
         Contract
       ) where

import Database.SqlServer.Create.User (User)
import Database.SqlServer.Create.MessageType (MessageType)
import Database.SqlServer.Create.Identifier hiding (unwrap)
import Database.SqlServer.Create.Entity

import Test.QuickCheck
import Text.PrettyPrint hiding (render)

data SentByConstraint = Initiator | Target | Any

instance Arbitrary SentByConstraint where
  arbitrary = elements [Initiator, Target, Any]

isInitiatorOrAny :: SentByConstraint -> Bool
isInitiatorOrAny Initiator = True
isInitiatorOrAny Target = False
isInitiatorOrAny Any = True

data Constraint = Constraint MessageType SentByConstraint

instance Arbitrary Constraint where
  arbitrary = Constraint <$> arbitrary <*> arbitrary

messageType :: Constraint -> MessageType
messageType (Constraint m _) = m

sentBy :: Constraint -> SentByConstraint
sentBy (Constraint _ s) = s

data Contract = Contract
  {
    contractName :: RegularIdentifier
  , authorization :: Maybe User
  , messageTypes :: [Constraint]
  }

renderSentBy :: SentByConstraint -> Doc
renderSentBy Initiator = text "SENT BY INITIATOR"
renderSentBy Target = text "SENT BY TARGET"
renderSentBy Any = text "SENT BY ANY"

renderMessageType :: Constraint -> Doc
renderMessageType (Constraint mt sbc) = renderName mt <+> renderSentBy sbc

renderAuthorization :: User -> Doc
renderAuthorization n = text "AUTHORIZATION" <+> renderName n

renderPrerequisites :: Contract -> Doc
renderPrerequisites c = maybe empty render (authorization c) $+$
                        text "GO" $+$
                        vcat (punctuate (text "\nGO\n") $
                              map (render . messageType) (messageTypes c)) $+$
                        text "GO\n"

-- The service must have at least one message SENT BY INITIATOR or ANY.
instance Arbitrary Contract where
  arbitrary = do
    n <- arbitrary
    mts <- listOf1 arbitrary `suchThat` any (isInitiatorOrAny . sentBy)
    auth <- arbitrary
    return $ Contract n auth mts

instance Entity Contract where
  name = contractName
  render m = renderPrerequisites m $+$
            text "CREATE CONTRACT" <+> renderName m $+$
            maybe empty renderAuthorization (authorization m) $+$
            mt $+$
            text "GO\n"
    where
      mt = parens
           (vcat $ punctuate comma (map renderMessageType (messageTypes m)))

instance Show Contract where
  show = show . render
