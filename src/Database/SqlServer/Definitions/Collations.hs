module Database.SqlServer.Definitions.Collations
       (
         Collation
       , renderCollation
       ) where

import Test.QuickCheck
import Text.PrettyPrint

newtype Collation = Collation String

instance Arbitrary Collation where
  arbitrary = elements collations

renderCollation :: Collation -> Doc
renderCollation (Collation x) = text "COLLATE" <+> text x

collations :: [Collation]
collations = map Collation  ["SQL_Latin1_General_CP1_CI_AS"]
