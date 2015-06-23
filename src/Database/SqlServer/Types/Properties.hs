{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.Properties where

import Database.SqlServer.Types.Identifiers
import Database.SqlServer.Types.Reserved

import Data.List (nub)

class NamedEntity a where
  name :: a -> RegularIdentifier

uniqueNames :: NamedEntity a => [a] -> Bool
uniqueNames xs = length xs == length (nub $ map name xs)

reserved :: NamedEntity a => a -> Bool
reserved a = isReserved $ unwrap (name a)

unReserved :: NamedEntity a => a -> Bool
unReserved = not . reserved

validIdentifiers :: NamedEntity a => [a] -> Bool
validIdentifiers xs = all unReserved xs && uniqueNames xs
