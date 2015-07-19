module Database.SqlServer.Types.Entity where

import Text.PrettyPrint

class Entity a where
  toDoc :: a -> Doc
  

