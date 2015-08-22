module Database.SqlServer.Create.Trigger
       (
         Trigger
       ) where

import Prelude hiding (cycle)

import Database.SqlServer.Create.Identifier
  (RegularIdentifier, renderRegularIdentifier)
import Database.SqlServer.Create.Entity
import Database.SqlServer.Create.Table
import Text.PrettyPrint
import Test.QuickCheck
import Control.Monad
import Data.Maybe (fromMaybe)

data Trigger = Trigger
