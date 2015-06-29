{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Monoid (mconcat)

main :: IO ()
main = scotty 8888 $ do
  get "/database/:database/:hash" $ do
    dbName <- param "database"
    hash   <- param "hash"
    html $ mconcat [
      "<h1>Database: ",
      dbName,
      "</h1>",
      "<h1>Hash: ",
      hash,
      "</h1>"
      ]
                    
