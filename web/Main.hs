{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Aeson hiding (json)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Text.PrettyPrint (render)


import Database.SqlServer.Types.Database;

data DatabaseAsJson = DatabaseAsJson
   {
     seed   :: Int
   , size   :: Int
   , createStatement :: String
   } deriving Generic

instance ToJSON DatabaseAsJson

generateDB :: Int -> Int -> DatabaseAsJson
generateDB seed' size' = DatabaseAsJson
    {
      seed = seed'
    , size = size'
    , createStatement = (render . renderDatabaseDefinition) db
    }
  where
    db = unGen (arbitrary :: Gen DatabaseDefinition) (mkQCGen seed') size'

main :: IO ()
main = scotty 8888 $ do
  get "/database/:seed" $ do
    s <- param "seed"
    z <- param "size" `rescue` (const $ return 101)
    json (generateDB s z)
                    
