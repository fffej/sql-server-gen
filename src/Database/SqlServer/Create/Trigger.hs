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
import Data.Either (isRight)

data Option = Insert
            | Update
            | Delete
              deriving (Eq, Ord)

data OptionModifier = For
                    | After
                    | InsteadOf

instance Arbitrary OptionModifier where
  arbitrary = elements [For, After, InsteadOf]

instance Arbitrary Option where
  arbitrary = elements [Insert, Update, Delete]

data Options = Options OptionModifier (S.Set Option)

instance Arbitrary Options where
  arbitrary = Options <$>
              arbitrary <*>
              (S.fromList <$> sublistOf [Insert, Update, Delete])

renderOptions :: Options -> Doc
renderOptions (Options m s) 
  | S.null s = empty
  | otherwise = modifier m <+>
                vcat (punctuate comma (map display $ S.toList s))
  where
    display Insert = text "INSERT"
    display Update = text "UPDATE"
    display Delete = text "DELETE"
    modifier For = text "FOR"
    modifier After = text "AFTER"
    modifier InsteadOf = text "INSTEAD OF"

isInsteadOf :: Options -> Bool
isInsteadOf (Options InsteadOf _) = True
isInsteadOf _ = False

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
-- Only "INSTEAD OF" triggers are valid on views
-- Views can not have the CHECK_OPTION
data Trigger = Trigger
  {
    triggerName :: RegularIdentifier 
  , on :: Either Table View
  , dmlTriggerOption :: Maybe DmlTriggerOption
  , options :: Options
  , notForReplication :: Bool
  }

instance Arbitrary Trigger where
  arbitrary = (Trigger <$>
              arbitrary <*>
              arbitrary `suchThat` validViewOrTable <*>
              arbitrary <*>
              arbitrary <*>
              arbitrary) `suchThat` validTrigger

validViewOrTable :: Either Table View -> Bool
validViewOrTable (Left _) = True
validViewOrTable (Right v) = not (withCheckOption v)

validTrigger :: Trigger -> Bool
validTrigger t = if isRight (on t) -- A view
                 then isInsteadOf (options t)
                 else True

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
    renderOptions (options t) $+$
    (if notForReplication t then text "NOT FOR REPLICATION" else empty) $+$
    text "AS" <+> text "SELECT 1;" $+$
    text "GO\n"
             
instance Show Trigger where
  show = show . render
