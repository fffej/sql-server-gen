module Database.SqlServer.Definition.Options
       (
         Options
       ) where

import Control.Monad.Reader
import Test.QuickCheck

data Options = Options
  {
    useCertificates :: Bool
  , useLogins :: Bool
  } deriving (Show)
  
type EntityGen a = ReaderT Options Gen 

