module Database.SqlServer.Definitions.FullTextStopList where

import Database.SqlServer.Definitions.Identifiers
import Database.SqlServer.Definitions.Entity

import Test.QuickCheck
import Text.PrettyPrint
import Control.Monad

data FullTextStopList = FullTextStopList
  {
    stoplistName :: RegularIdentifier
  , sourceStopList :: Maybe (Maybe FullTextStopList)
  }

instance Arbitrary FullTextStopList where
  arbitrary = do
    x <- arbitrary
    y <- frequency [(50, return Nothing), (50,arbitrary)]
    return (FullTextStopList x y)

instance Entity FullTextStopList where
  toDoc f = maybe empty toDoc (join (sourceStopList f)) $+$
            text "CREATE FULLTEXT STOPLIST" <+>
            renderRegularIdentifier (stoplistName f) <+>
            maybe (text ";") (\q -> text "FROM" <+>
                               maybe (text "SYSTEM STOPLIST;\n") (\x -> renderRegularIdentifier (stoplistName x) <> text ";\n") q <>
                               text "GO\n") (sourceStopList f)
