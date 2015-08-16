module Database.SqlServer.Create.Entity
       (
         Entity,
         render,
         name,
         renderName
       ) where

import Database.SqlServer.Create.Identifier

import Text.PrettyPrint hiding (render)

class Entity a where
  render :: a -> Doc
  name :: a -> RegularIdentifier
  renderName :: a -> Doc
  renderName = renderRegularIdentifier . name
