module Database.SqlServer.Types.FullTextStopList where

import Database.SqlServer.Types.Identifiers
import Database.SqlServer.Types.Entity

import Test.QuickCheck
import Text.PrettyPrint

data FullTextStopListDefinition = FullTextStopListDefinition
  {
    stoplistName :: RegularIdentifier
  , sourceStopList :: Maybe (Maybe FullTextStopListDefinition)
  }

instance Arbitrary FullTextStopListDefinition where
  arbitrary = do
    x <- arbitrary
    y <- frequency [(50, return Nothing), (50,arbitrary)]
    return (FullTextStopListDefinition x y)

instance Entity FullTextStopListDefinition where
  toDoc f = maybe empty toDoc (maybe Nothing id (sourceStopList f)) $+$
            text "CREATE FULLTEXT STOPLIST" <+>
            renderRegularIdentifier (stoplistName f) <+>
            maybe (text ";") (\q -> text "FROM" <+>
                               maybe (text "SYSTEM STOPLIST;\n") (\x -> renderRegularIdentifier (stoplistName x) <> text ";\n") q <>
                               text "GO\n") (sourceStopList f)
