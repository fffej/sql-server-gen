{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Definitions.FullTextCatalog where

import Database.SqlServer.Definitions.Identifiers
import Database.SqlServer.Definitions.Entity

import Test.QuickCheck
import Text.PrettyPrint
import Data.DeriveTH

data FullTextCatalog = FullTextCatalog
  {
    catalogName :: RegularIdentifier
  , filegroup :: Maybe RegularIdentifier
  , accentSensitive :: Maybe Bool
  , asDefault :: Bool
  -- TODO ignore users
  }

derive makeArbitrary ''FullTextCatalog

renderFileGroup :: RegularIdentifier -> Doc
renderFileGroup n = text "ON FILEGROUP" <+> renderRegularIdentifier n

renderOptions :: Bool -> Doc
renderOptions True  = text "WITH ACCENT_SENSITIVITY = ON"
renderOptions False = text "WITH ACCENT_SENSITIVITY = OFF"

instance Entity FullTextCatalog where
  toDoc ftc = text "CREATE FULLTEXT CATALOG" <+>
              renderRegularIdentifier (catalogName ftc) $+$
              maybe empty renderFileGroup (filegroup ftc) $+$
              maybe empty renderOptions (accentSensitive ftc) $+$
              if asDefault ftc then text "AS DEFAULT" else empty $+$
              text "GO\n"
