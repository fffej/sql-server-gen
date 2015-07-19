{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Definition.Contract
       (
         Contract
       ) where

import Database.SqlServer.Definition.User (User)
import Database.SqlServer.Definition.MessageType (MessageType)
import Database.SqlServer.Definition.Identifier hiding (unwrap)
import Database.SqlServer.Definition.Entity

import Test.QuickCheck
import Data.DeriveTH
import Text.PrettyPrint

data SentByConstraint = Initiator | Target | Any

isInitiatorOrAny :: SentByConstraint -> Bool
isInitiatorOrAny Initiator = True
isInitiatorOrAny Target = False
isInitiatorOrAny Any = True

derive makeArbitrary ''SentByConstraint

data Constraint = Constraint MessageType SentByConstraint

messageType :: Constraint -> MessageType
messageType (Constraint m _) = m

sentBy :: Constraint -> SentByConstraint
sentBy (Constraint _ s) = s

derive makeArbitrary ''Constraint

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
renderPrerequisites c = maybe empty toDoc (authorization c) $+$
                        text "GO" $+$
                        vcat (punctuate (text "\nGO\n") $ map (toDoc . messageType) (messageTypes c)) $+$
                        text "GO\n"

-- The service  must have at least one message SENT BY INITIATOR or ANY.
instance Arbitrary Contract where
  arbitrary = do
    n <- arbitrary
    mts <- listOf1 arbitrary `suchThat` any (isInitiatorOrAny . sentBy)
    auth <- arbitrary
    return $ Contract n auth mts 

instance Entity Contract where
  name = contractName
  toDoc m = renderPrerequisites m $+$
            text "CREATE CONTRACT" <+> renderName m $+$
            maybe empty renderAuthorization (authorization m) $+$
            parens (vcat $ punctuate comma (map renderMessageType (messageTypes m))) 

instance Show Contract where
  show = show . toDoc
            
