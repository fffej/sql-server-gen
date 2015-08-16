module Database.SqlServer.Alter.Alter
       (
         Alter,
         render,
         apply
       ) where

import Text.PrettyPrint hiding (render)
import Database.SqlServer.Create.Database

class Alter a where
  render :: a -> Doc
  apply :: Database -> a -> Database
