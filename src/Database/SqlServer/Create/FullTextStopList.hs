module Database.SqlServer.Create.FullTextStopList
       (
         FullTextStopList
       ) where

import Database.SqlServer.Create.Identifier
import Database.SqlServer.Create.Entity

import Test.QuickCheck
import Text.PrettyPrint hiding (render)
import Control.Monad

data FullTextStopList = FullTextStopList
  {
    stoplistName :: RegularIdentifier
  , sourceStopList :: Maybe (Maybe FullTextStopList)
  }

instance Arbitrary FullTextStopList where
  arbitrary = do
    x <- arbitrary
    y <- frequency [(50, return Nothing), (50, arbitrary)]
    return (FullTextStopList x y)

instance Entity FullTextStopList where
  name = stoplistName
  render f = maybe empty render (join (sourceStopList f)) $+$
            text "CREATE FULLTEXT STOPLIST" <+>
            renderName f <+>
            maybe (text ";") (\ q -> text "FROM" <+>
                                     systop q <>
                                     text "GO\n") (sourceStopList f)
    where
      systop q =
        maybe
        (text "SYSTEM STOPLIST;\n")
        (\ x -> renderRegularIdentifier (stoplistName x) <> text ";\n") q
