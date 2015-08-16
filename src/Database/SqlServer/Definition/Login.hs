module Database.SqlServer.Definition.Login where

import Database.SqlServer.Definition.Identifier
import Database.SqlServer.Definition.Entity

import Control.Monad
import Test.QuickCheck
import Text.PrettyPrint hiding (render)

data Login = Login
   {
     loginName :: RegularIdentifier
   , password :: RegularIdentifier
   , mustChange :: Bool 
   }

instance Arbitrary Login where
  arbitrary = liftM3 Login arbitrary arbitrary arbitrary

renderPassword :: RegularIdentifier -> Doc
renderPassword s = text "WITH PASSWORD = " <>
                   quotes (renderRegularIdentifier s)

renderMustChange :: Bool -> Doc
renderMustChange False = empty
renderMustChange True = text "MUST_CHANGE" <> comma <> text "CHECK_EXPIRATION=ON"

instance Entity Login where
  name = loginName
  render a = text "CREATE LOGIN" <+> renderName a $+$
            renderPassword (password a)  <+> renderMustChange (mustChange a)
            
instance Show Login where
  show = show . render 
