module Database.SqlServer.Definitions.Entity
       (
         Entity,
         toDoc
       ) where

import Text.PrettyPrint

class Entity a where
  toDoc :: a -> Doc
  

