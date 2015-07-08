{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.FullTextCatalog where

import Database.SqlServer.Types.Identifiers
import Database.SqlServer.Types.Entity

import Test.QuickCheck
import Text.PrettyPrint
import Data.DeriveTH

data FullTextCatalogDefinition = FullTextCatalogDefinition
  {
    catalogName :: RegularIdentifier
  , filegroup :: Maybe RegularIdentifier
  , accentSensitive :: Maybe Bool
  , asDefault :: Bool
  -- TODO ignore users
  }

derive makeArbitrary ''FullTextCatalogDefinition

renderFileGroup :: RegularIdentifier -> Doc
renderFileGroup n = text "ON FILEGROUP" <+> (renderRegularIdentifier n)

renderOptions :: Bool -> Doc
renderOptions True  = text "WITH ACCENT_SENSITIVITY = ON"
renderOptions False = text "WITH ACCENT_SENSITIVITY = OFF"

instance Entity FullTextCatalogDefinition where
  toDoc ftc = text "CREATE FULLTEXT CATALOG" <+>
              renderRegularIdentifier (catalogName ftc) $+$
              maybe empty renderFileGroup (filegroup ftc) $+$
              maybe empty renderOptions (accentSensitive ftc) $+$
              if (asDefault ftc) then text "AS DEFAULT" else empty $+$
              text "GO\n"
