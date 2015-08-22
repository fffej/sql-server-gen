module Database.SqlServer.Create.Trigger
       (
         Trigger
       ) where

import Prelude hiding (cycle)

import Database.SqlServer.Create.Identifier (RegularIdentifier)
import Database.SqlServer.Create.Entity
import Database.SqlServer.Create.Table
import Database.SqlServer.Create.View
import Text.PrettyPrint hiding (render)
import Test.QuickCheck
import qualified Data.Set as S

data Option = Insert
            | Update
            | Delete
              deriving (Eq, Ord)

instance Arbitrary Option where
  arbitrary = elements [Insert, Update, Delete]

data Options = Options (S.Set Option)

instance Arbitrary Options where
  arbitrary = Options <$>
              (S.fromList <$> sublistOf [Insert, Update, Delete])

renderOptions :: Options -> Doc
renderOptions (Options s) 
  | S.null s = empty
  | otherwise = text "AFTER" <+>
                vcat (punctuate comma (map display $ S.toList s))
  where
    display Insert = text "INSERT"
    display Update = text "UPDATE"
    display Delete = text "DELETE"

data ExecuteAs = Caller
               | Self
               | Owner

renderExecuteAs :: ExecuteAs -> Doc
renderExecuteAs Caller = text "CALLER"
renderExecuteAs Self = text "SELF"
renderExecuteAs Owner = text "OWNER"

instance Arbitrary ExecuteAs where
  arbitrary = elements [Caller, Self, Owner]

data DmlTriggerOption = Encryption
                      | TableExecuteAs ExecuteAs

instance Arbitrary DmlTriggerOption where
  arbitrary = oneof [return Encryption, TableExecuteAs <$> arbitrary]

-- Currently only considering table/view triggers
data Trigger = Trigger
  {
    triggerName :: RegularIdentifier 
  , on :: Either Table View
  , dmlTriggerOption :: Maybe DmlTriggerOption
  , options :: Maybe Options
  , notForReplication :: Bool
  }

instance Arbitrary Trigger where
  arbitrary = Trigger <$>
              arbitrary <*>
              arbitrary <*>
              arbitrary <*>
              arbitrary <*>
              arbitrary

renderDmlTriggerOption :: DmlTriggerOption -> Doc
renderDmlTriggerOption Encryption = text "WITH ENCRYPTION"
renderDmlTriggerOption (TableExecuteAs x) = 
  text "WITH EXECUTE AS" <+> renderExecuteAs x

instance Entity Trigger where
  name = triggerName
  render t =
    either render render (on t) $+$
    text "CREATE TRIGGER" <+> renderName t $+$
    text "ON" <+> either renderName renderName (on t) $+$
    maybe empty renderDmlTriggerOption (dmlTriggerOption t) $+$
    maybe empty renderOptions (options t) $+$
    (if notForReplication t then text "NOT FOR REPLICATION" else empty) $+$
    text "AS" <+> text "SELECT 1;" $+$
    text "GO"
             
instance Show Trigger where
  show = show . render
