module Database.SqlServer.Types.Properties where

import Database.SqlServer.Types.Identifiers (RegularIdentifier, unwrap)
import Database.SqlServer.Types.Reserved (isReserved)

class NamedEntity a where
  name :: a -> RegularIdentifier

reserved :: NamedEntity a => a -> Bool
reserved a = isReserved $ unwrap (name a)

unReserved :: NamedEntity a => a -> Bool
unReserved = not . reserved

validIdentifiers :: NamedEntity a => [a] -> Bool
validIdentifiers xs = all unReserved xs


  
