{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Aeson hiding (json)
import GHC.Generics
import Data.Hashable
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Text.PrettyPrint (render)
import Control.Monad.IO.Class
import Data.Text


import Database.SqlServer.Types.Database;

data DatabaseAsJson = DatabaseAsJson
   {
     dbName :: String
   , seed   :: String
   , createStatement :: String
   } deriving Generic

instance ToJSON DatabaseAsJson

generateDB :: String -> String -> IO String
generateDB n s = do
  let seed = hash s    
      db = unGen (arbitrary :: Gen DatabaseDefinition) (mkQCGen seed) seed
  return $ (render . renderDatabaseDefinition) db

main :: IO ()
main = scotty 8888 $ do
  get "/database/:database/:seed" $ do
    n <- param "database"
    s   <- param "seed"
    db <- liftIO $ generateDB n s
    text db
                    
