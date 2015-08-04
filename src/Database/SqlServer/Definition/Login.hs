{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Definition.Login where

import Database.SqlServer.Definition.Identifier
import Database.SqlServer.Definition.Entity

import Data.DeriveTH
import Test.QuickCheck
import Text.PrettyPrint

data Login = Login
   {
     loginName :: RegularIdentifier
   , password :: RegularIdentifier
   , mustChange :: Bool 
   }

derive makeArbitrary ''Login

renderPassword :: RegularIdentifier -> Doc
renderPassword s = text "WITH PASSWORD = " <>
                   quotes (renderRegularIdentifier s)

renderMustChange :: Bool -> Doc
renderMustChange False = empty
renderMustChange True = text "MUST_CHANGE" <> comma <> text "CHECK_EXPIRATION=ON"

instance Entity Login where
  name = loginName
  toDoc a = text "CREATE LOGIN" <+> renderName a $+$
            renderPassword (password a)  <+> renderMustChange (mustChange a)
            
instance Show Login where
  show = show . toDoc 
