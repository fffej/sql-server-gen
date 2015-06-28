module Database.SqlServer.Types.Identifiers where

import Database.SqlServer.Types.Reserved (isReserved)

import Test.QuickCheck
import Control.Monad (liftM2)
import Text.PrettyPrint

import Data.Char (toUpper)

-- https://msdn.microsoft.com/en-us/subscriptions/downloads/ms175874
newtype RegularIdentifier = RegularIdentifier
                            {
                              unwrap :: String
                            } deriving (Ord)

instance Eq RegularIdentifier where
  a == b = (map toUpper $ unwrap a) == (map toUpper $ unwrap b)

renderRegularIdentifier :: RegularIdentifier -> Doc
renderRegularIdentifier (RegularIdentifier x) = text x

-- Removed hash; it's magic
firstChars :: String
firstChars = ['a'..'z'] ++ ['A'..'Z'] ++ "_"

subsequentChars :: String
subsequentChars = firstChars ++ ['0'..'9']

-- The MSDN documentation lies
-- The object or column name starting with '#IDZf39RtgmgnZgRrcVHc3d4nFsY9#UirNPVM6nICl8lubBsJfXjnV_yIPpagTXeYjN_2JrQwkUnPsBJ7OTqUtRn1E4xe65djU8WB#u7rM#MPdHVmi3QYk2gontf' is too long. The maximum length is 116 characters.
maximumLengthOfRegularIdentifier :: Int
maximumLengthOfRegularIdentifier = 116

validLength :: String -> Bool
validLength x = length x < maximumLengthOfRegularIdentifier

validIdentifier :: String -> Bool
validIdentifier x = validLength x && not (isReserved x)

-- https://msdn.microsoft.com/en-us/library/ms189822.aspx
newtype Keyword = Keyword String

instance Arbitrary RegularIdentifier where
  arbitrary = do
    y <- liftM2
         (:)
         (elements firstChars)
         (listOf (elements subsequentChars)) `suchThat` validIdentifier
    return (RegularIdentifier y)
