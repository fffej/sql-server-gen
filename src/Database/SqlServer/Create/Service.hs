module Database.SqlServer.Create.Service
       (
         Service
       ) where

import Database.SqlServer.Create.Queue (Queue)
import Database.SqlServer.Create.Contract (Contract)
import Database.SqlServer.Create.Identifier hiding (unwrap)
import Database.SqlServer.Create.Entity

import Test.QuickCheck
import Text.PrettyPrint hiding (render)

{-
 An entity of type service cannot be owned by
 a role, a group, or by principals mapped to
 certificates or asymmetric keys.
-}
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
renderPreRequisites s =
  render (queue s) $+$
  vcat (map render (contracts s))

instance Entity Service where
  name = serviceName
  render s = renderPreRequisites s $+$
            text "CREATE SERVICE" <+> renderName s $+$
            text "ON QUEUE" <+> renderName (queue s) $+$
            renderContracts (contracts s) $+$
            text "GO\n"

instance Show Service where
  show = show . render
