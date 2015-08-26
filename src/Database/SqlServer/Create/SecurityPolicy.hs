module Database.SqlServer.Create.SecurityPolicy
       (
         SecurityPolicy
       ) where

import Database.SqlServer.Create.Identifier
import Database.SqlServer.Create.Table
import Database.SqlServer.Create.DataType
import Database.SqlServer.Create.Entity

import Test.QuickCheck
import Text.PrettyPrint hiding (render)

-- TODO generate these
exampleSecurityFunctionName :: String
exampleSecurityFunctionName = "dbo.rowLevelPredicate"

exampleSecurityFunction :: Doc 
exampleSecurityFunction =
  text "CREATE FUNCTION dbo.rowLevelPredicate (@userCode as sysname)" $+$
  text "RETURNS TABLE" $+$
  text "WITH SCHEMABINDING" $+$
  text "AS" $+$
  text "RETURN SELECT 1 AS rowLevelPredicateResult" $+$
  text "WHERE @userCode = USER_NAME();" $+$
  text "GO\n"

data SecuredObject = SecuredObject
  {
    table :: Table
  , parameter :: Column
  }

instance Arbitrary SecuredObject where
  arbitrary = do
    t <- arbitrary `suchThat` atLeastOneColumnIsValid
    c <- elements (unpack $ columns t) `suchThat` columnTypeIsCompatible
    return (SecuredObject t c)

atLeastOneColumnIsValid :: Table -> Bool
atLeastOneColumnIsValid t = not $ null (filter columnTypeIsCompatible (unpack $ columns t))

-- Need to explore what this means in more detail
columnTypeIsCompatible :: Column -> Bool
columnTypeIsCompatible c = isSupportedTypeForPartitionFunction (dataType c) &&
                           not (isSqlVariant (dataType c))

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
              listOf1 arbitrary <*>
              arbitrary <*>
              arbitrary

renderFilterPredicates :: [SecuredObject] -> [Doc]
renderFilterPredicates = punctuate comma . map renderFilterPredicate

renderFilterPredicate :: SecuredObject -> Doc
renderFilterPredicate so =
  text "ADD FILTER PREDICATE" <+> text exampleSecurityFunctionName <>
  parens ((renderRegularIdentifier . columnName) (parameter so)) $+$
  text "ON dbo." <> renderName (table so)
  
renderState :: Bool -> Doc
renderState True = text "WITH (STATE = ON)"
renderState False = text "WITH (STATE = OFF)"        

renderPrerequisites :: SecurityPolicy -> Doc
renderPrerequisites s = vcat $ map renderTable (securedObjects s)

renderTable :: SecuredObject -> Doc
renderTable = render . table

instance Entity SecurityPolicy where
  name = policyName
  render s =
    renderPrerequisites s $+$
    text "CREATE SECURITY POLICY" <+> (renderName s) $+$
    vcat (renderFilterPredicates (securedObjects s)) $+$
    maybe empty renderState (state s) $+$ 
    if (not $ forReplication s) then text "NOT FOR REPLICATION" else empty $+$
    text "GO\n"

instance Show SecurityPolicy where
  show = show . render