module Database.SqlServer.Definition.Entity
       (
         Entity,
         toDoc,
         name,
         renderName
       ) where

import Database.SqlServer.Definition.Identifier
import Database.SqlServer.Definition.Options

import Text.PrettyPrint
import Test.QuickCheck
import Control.Monad.Reader

class Entity a where
  toDoc :: a -> Doc
  name :: a -> RegularIdentifier
  renderName :: a -> Doc
  renderName = renderRegularIdentifier . name
