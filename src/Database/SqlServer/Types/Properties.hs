module Database.SqlServer.Types.Properties where

import Database.SqlServer.Types.Identifiers (RegularIdentifier)

class NamedEntity a where
  name :: a -> RegularIdentifier

