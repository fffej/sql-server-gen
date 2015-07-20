{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs

import Database.SqlServer.Definition.Database

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
      seed = def &= help "Seed for random number generator" &= name "s"
    , size = 100 &= help "Size of database (optional)" &= opt (500 :: Int) &= name "n"
    } &= summary "SQL Server Schema Generator"
      &= help "Generate arbitrary SQL Server databases"
      &= details msg

main :: IO ()
main = do
  a <- cmdArgs defaultArgs
  print $ seededDatabase (seed a) (size a) 
  return ()
