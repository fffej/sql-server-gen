{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs

import Database.SqlServer.Types.Database

data Arguments = Arguments
    {
      seed :: Int
    , size :: Int
    } deriving (Show,Data,Typeable)

msg :: [String]
msg =  ["More details on the github repo at " ++
        " https://github.com/fffej/sql-server-gen"]

defaultArgs :: Arguments
defaultArgs = Arguments 
    {
      seed = def &= help "Seed for random number generator"
    , size = def &= help "Size of database (optional)"
    } &= summary "SQL Server Schema Generator"
      &= help "Generate arbitrary SQL Server databases"
      &= details msg

main :: IO ()
main = do
  a <- cmdArgs defaultArgs
  print $ seededDatabase (seed a) (size a) 
  return ()
