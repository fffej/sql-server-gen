module Database.SqlServer.Definition.Service
       (
         Service
       ) where

import Database.SqlServer.Definition.Queue (Queue)
import Database.SqlServer.Definition.Contract (Contract)
import Database.SqlServer.Definition.Identifier hiding (unwrap)
import Database.SqlServer.Definition.Entity

import Test.QuickCheck
import Text.PrettyPrint

-- An entity of type service cannot be owned by
-- a role, a group, or by principals mapped to
-- certificates or asymmetric keys.
data Service = Service
    {
      serviceName :: RegularIdentifier
    , queue :: Queue
    , contracts :: [Contract]
    }

-- TODO Owner
instance Arbitrary Service where
  arbitrary = Service <$> arbitrary <*> arbitrary <*> arbitrary

renderContracts :: [Contract] -> Doc
renderContracts [] = empty
renderContracts xs = parens (vcat $ punctuate comma (map renderName xs)) $+$
                     text "GO\n"

renderPreRequisites :: Service -> Doc
renderPreRequisites s = toDoc (queue s) $+$
                        vcat (punctuate (text "GO") (map toDoc (contracts s))) $+$
                        text "GO\n"

instance Entity Service where
  name = serviceName
  toDoc s = renderPreRequisites s $+$
            text "CREATE SERVICE" <+> renderName s $+$
            text "ON QUEUE" <+> renderName (queue s) $+$
            renderContracts (contracts s) $+$
            text "GO\n" 

instance Show Service where
  show = show . toDoc
