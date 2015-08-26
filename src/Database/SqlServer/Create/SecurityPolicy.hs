module Database.SqlServer.Create.SecurityPolicy
       (
         SecurityPolicy
       ) where

import Database.SqlServer.Create.Identifier
import Database.SqlServer.Create.Table
import Database.SqlServer.Create.Entity

import Test.QuickCheck
import Text.PrettyPrint hiding (render)

data SecuredObject = SecuredObject
  {
    table :: Table
  , arguments :: [Column]
  }

instance Arbitrary SecuredObject where
  arbitrary = do
    t <- arbitrary
    c <- sublistOf (unpack $ columns t) `suchThat` (not . null)
    return (SecuredObject t c)

data SecurityPolicy = SecurityPolicy
  {
    policyName :: RegularIdentifier
  , securedObjects :: [SecuredObject]
  , state :: Maybe Bool
  , forReplication :: Bool
  }

instance Arbitrary SecurityPolicy where
  arbitrary = SecurityPolicy <$>
              arbitrary <*>
              arbitrary <*>
              arbitrary <*>
              arbitrary

renderFilterPredicates :: [SecuredObject] -> [Doc]
renderFilterPredicates = map renderFilterPredicate

renderFilterPredicate :: SecuredObject -> Doc
renderFilterPredicate so =
  text "ADD FILTER PREDICATE"

renderState :: Bool -> Doc
renderState True = text "WITH (STATE = ON)"
renderState False = text "WITH (STATE = OFF)"
          

instance Entity SecurityPolicy where
  name = policyName
  render s =
    text "CREATE SECURITY POLICY" <+> (renderName s) $+$
    vcat (renderFilterPredicates (securedObjects s)) $+$
    maybe empty renderState (state s) $+$ 
    if (not $ forReplication s) then text "NOT FOR REPLICATION" else empty $+$
    text "GO\n"

instance Show SecurityPolicy where
  show = show . render