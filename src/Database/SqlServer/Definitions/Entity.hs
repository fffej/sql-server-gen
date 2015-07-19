module Database.SqlServer.Definitions.Entity where

import Text.PrettyPrint

class Entity a where
  toDoc :: a -> Doc
  

