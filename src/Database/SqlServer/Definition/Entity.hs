module Database.SqlServer.Definition.Entity
       (
         Entity,
         toDoc,
         name,
         renderName
       ) where

import Database.SqlServer.Definition.Identifier

import Text.PrettyPrint

class Entity a where
  toDoc :: a -> Doc
  name :: a -> RegularIdentifier
  renderName :: a -> Doc
  renderName = renderRegularIdentifier . name
