{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs

import qualified Database.SqlServer.Generator as D
import Database.SqlServer.Create.Database
import Data.List.Split

data Arguments = Arguments
    {
      seed :: Int
    , size :: Int
    , excludeTypes :: String
    } deriving (Show,Data,Typeable)

msg :: [String]
msg =  ["More details on the github repo at " ++
        " https://github.com/fffej/sql-server-gen"]

defaultArgs :: Arguments
defaultArgs = Arguments
    {
      seed = def &= help "Seed for random number generator" &= name "s"
    , size = 100 &= help "Size of database (optional)" &= opt (500 :: Int) &= name "n"
    , excludeTypes = "*" &= help "List of object types to exclude comma seperated" &= name "e"
    } &= summary "SQL Server Schema Generator"
      &= help "Generate arbitrary SQL Server databases"
      &= details msg

convert :: Arguments -> D.GenerateOptions
convert a = D.GenerateOptions
    {
      D.seed = seed a
    , D.size = size a
    , D.excludeTypes = parseRenderOptions $ excludeTypes a
    }

parseRenderOptions :: String -> RenderOptions
parseRenderOptions xs = foldl setFlag defaultRenderOptions (splitOn "," xs)

setFlag :: RenderOptions -> String -> RenderOptions
setFlag ro s =
  case s of
    "T" -> ro {showTables = False}
    "V" -> ro {showViews = False}
    "S" -> ro {showSequences = False}
    "P" -> ro {showProcedures = False}
    "F" -> ro {showFunctions = False}
    "U" -> ro {showUsers = False}
    "R" -> ro {showRoles = False}
    "FTC" -> ro {showFullTextCatalog = False}
    "FTS" -> ro {showFullTextStopList = False}
    "CRED" -> ro {showCredential = False}
    "M" -> ro {showMessageType = False}
    "B" -> ro {showBrokerPriority = False}
    "PF" -> ro {showPartitionFunction = False}
    _   -> ro

header :: Arguments -> String
header a = unlines 
  [
    "-- This code was generated by sql-server-gen"
  , "-- Arguments used: seed=" ++ show (seed a) ++ " size=" ++ show (size a)
  ]

main :: IO ()
main = do
  a <- cmdArgs defaultArgs
  let renderOptions = parseRenderOptions (excludeTypes a)
  putStrLn (header a)
  print $ (renderDatabase renderOptions (D.generateEntity (convert a)))
  return ()
