module Database.SqlServer.Types.Collations where

import Test.QuickCheck
import Control.Monad

import Text.PrettyPrint

newtype Collation = Collation String

instance Arbitrary Collation where
  arbitrary = elements collations

renderCollation :: Collation -> Doc
renderCollation (Collation x) = text "COLLATE" <+> text x

collations :: [Collation]
collations = map Collation  ["SQL_Latin1_General_CP1_CI_AS"]
