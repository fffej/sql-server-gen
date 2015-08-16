module Database.SqlServer.Alter.Alter
       (
         Alter,
         render,
       ) where

import Text.PrettyPrint hiding (render)

class Alter a where
  render :: a -> Doc
