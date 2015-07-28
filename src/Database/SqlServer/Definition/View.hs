{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Definition.View
       (
         View
       ) where

import Database.SqlServer.Definition.Identifier
import Database.SqlServer.Definition.Entity
import Database.SqlServer.Definition.Table

import Test.QuickCheck
import Text.PrettyPrint

data View = View
  {
    viewName :: RegularIdentifier
  , tables :: [Table]
  , withCheckOption :: Bool
  }

unionSelectStatement :: View -> Doc
unionSelectStatement v = vcat $ punctuate (text " UNION ALL") (map selectN tabs)
  where
    tabs = tables v

selectN :: Table -> Doc
selectN t = text "SELECT" <+> hcat (punctuate comma cols) <+> text "FROM" <+> renderName t
  where
    cols = map (renderRegularIdentifier . columnName) $ unpack (columnDefinitions t)

renderPrerequisites :: View -> Doc
renderPrerequisites = vcat . map toDoc . tables

instance Arbitrary View where
  arbitrary = do
    n <- arbitrary
    t <- arbitrary
    co <- arbitrary
    return (View n [t] co)

instance Entity View where
  name = viewName
  toDoc v = renderPrerequisites v $+$
            text "CREATE VIEW" <+> renderName v $+$
            text "AS" $+$
            unionSelectStatement v $+$
            (if (withCheckOption v) then text "WITH CHECK OPTION" else empty) $+$
            text "GO\n"

instance Show View where
  show = show . toDoc
