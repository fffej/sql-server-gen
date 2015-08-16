module Database.SqlServer.Definition.Alter
       (
         Alter,
         render,
       ) where

import Database.SqlServer.Definition.Identifier

import Text.PrettyPrint hiding (render)

class Alter a where
  render :: a -> Doc
