{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Definitions.Credential
       (
         Credential
       ) where

import Database.SqlServer.Definitions.Identifier hiding (unwrap)
import Database.SqlServer.Definitions.Entity

import Text.PrettyPrint
import Test.QuickCheck
import Data.DeriveTH

newtype Identity = Identity String

instance Arbitrary Identity where
  arbitrary = do
    i <- listOf1 $ elements ['a' .. 'z']
    return (Identity i)

renderIdentity :: Identity -> Doc
renderIdentity (Identity s) = text s

newtype Secret = Secret String

instance Arbitrary Secret where
  arbitrary = do
    s <- listOf1 $ elements (['a' .. 'z'] ++ ['0' .. '9'])
    return (Secret s)

renderSecret :: Secret -> Doc
renderSecret (Secret s)= comma <> text "SECRET =" <+> quotes (text s)

data Credential = CredentialDefintion
   {
     credentialName :: RegularIdentifier
   , identity :: Identity
   , secret :: Maybe Secret
   }

derive makeArbitrary ''Credential

instance Entity Credential where
  toDoc s = text "CREATE CREDENTIAL" <+> renderRegularIdentifier (credentialName s) <+>
            text "WITH IDENTITY =" <+> quotes (renderIdentity (identity s)) <+>
            maybe empty renderSecret (secret s) $+$
            text "GO"
