module Database.SqlServer.Definition.Options
       (
         Options
       ) where

data Options = Options
  {
    useCertificates :: Bool
  , useLogins :: Bool
  } deriving (Show)
  
