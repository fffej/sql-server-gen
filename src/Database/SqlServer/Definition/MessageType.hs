module Database.SqlServer.Definition.MessageType
       (
         MessageType
       ) where

import Database.SqlServer.Definition.Identifier hiding (unwrap)
import Database.SqlServer.Definition.User (User,Role)
import Database.SqlServer.Definition.Entity

import Test.QuickCheck
import Text.PrettyPrint hiding (render)

data Validation = None
                | Empty
                | WellFormedXml -- TODO valid XML

instance Arbitrary Validation where
  arbitrary = elements [None,Empty,WellFormedXml]

data MessageType = MessageType
  {
    messageTypeName :: RegularIdentifier
  , authorization :: Maybe (Either User Role)
  , validation :: Maybe Validation
  }

instance Arbitrary MessageType where
  arbitrary = MessageType <$> arbitrary <*> arbitrary <*> arbitrary

-- Must be able to eliminate the duplication here
renderPreRequisites :: Either User Role -> Doc
renderPreRequisites (Left x)  = render x $+$ text "GO"
renderPreRequisites (Right x) = render x $+$ text "GO"

renderAuthorization :: Either User Role -> Doc
renderAuthorization (Left x)  = text "AUTHORIZATION" <+> renderName x
renderAuthorization (Right x) = text "AUTHORIZATION" <+> renderName x

renderValidation :: Validation -> Doc
renderValidation None = text "VALIDATION = NONE"
renderValidation Empty = text "VALIDATION = EMPTY"
renderValidation WellFormedXml = text "VALIDATION = WELL_FORMED_XML"

instance Entity MessageType where
  name = messageTypeName
  render m = maybe empty renderPreRequisites (authorization m) $+$
            text "CREATE MESSAGE TYPE" <+> renderName m $+$
            maybe empty renderAuthorization (authorization m) $+$
            maybe empty renderValidation (validation m) $+$
            text "GO\n"
            
