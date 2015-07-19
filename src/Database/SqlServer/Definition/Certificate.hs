module Database.SqlServer.Definition.Certificate
       (
         Certificate
       , certificateName
       ) where

import Database.SqlServer.Definition.Identifier
import Database.SqlServer.Definition.Entity

import Text.PrettyPrint
import Data.Time.Calendar
import Test.QuickCheck
import Control.Monad

data Certificate = Certificate
  {
    certificateName :: RegularIdentifier
  , activeForBeginDialog :: Maybe (Maybe Bool)
  , expiryDate :: Maybe Day -- must be at least tomorrow
  , startDate :: Maybe Day -- must be at least 1st Jan 1970
  , subject :: RegularIdentifier
  , encryptPassword :: RegularIdentifier
  }

renderEncryptionByPassword :: RegularIdentifier -> Doc
renderEncryptionByPassword s = text "ENCRYPTION BY PASSWORD = '" <> renderRegularIdentifier s <> text "'"

renderSubject :: RegularIdentifier -> Doc
renderSubject s = text "WITH SUBJECT = '" <> renderRegularIdentifier s <> text "'"

renderExpiryDate :: Day -> Doc
renderExpiryDate d = text "EXPIRY_DATE = '" <> text (filter (/= '-') (show d))  <> text "'"

renderStartDate :: Day -> Doc
renderStartDate d = text "START_DATE = '" <> text (filter (/= '-') (show d)) <> text "'"

instance Arbitrary Certificate where
  arbitrary = do
    certName <- arbitrary
    afbd <- arbitrary
    eDay <- liftM3 fromGregorian (elements [2016..3000]) (choose(1,12)) (choose(1,31))
    x <- choose(- 1000,- 1)
    ex <- elements [Just eDay, Nothing]
    str <- elements [Just (addDays x eDay), Nothing]
    ep <- arbitrary
    sub <- arbitrary
    return Certificate {
        certificateName = certName
      , activeForBeginDialog = afbd
      , startDate = str
      , expiryDate = ex
      , encryptPassword = ep
      , subject = sub
      }

instance Entity Certificate where
  name = certificateName
  toDoc c = text "CREATE CERTIFICATE" <+> renderName c $+$
            renderEncryptionByPassword (encryptPassword c) $+$
            hcat (punctuate comma $ filter (/= empty)
                  [ renderSubject (subject c)
                  , maybe empty renderExpiryDate (expiryDate c)
                  , maybe empty renderStartDate (startDate c)])
  
