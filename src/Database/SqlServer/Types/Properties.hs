{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.Properties where

import Database.SqlServer.Types.Identifiers

import Data.List (nub)

class NamedEntity a where
  name :: a -> RegularIdentifier

uniqueNames :: NamedEntity a => [a] -> Bool
uniqueNames xs = length xs == length (nub $ map name xs)
