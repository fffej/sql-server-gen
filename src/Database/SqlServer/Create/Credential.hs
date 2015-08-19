module Database.SqlServer.Create.Credential
       (
         Credential
       ) where

import Database.SqlServer.Create.Identifier hiding (unwrap)
import Database.SqlServer.Create.Entity

import Text.PrettyPrint
import Test.QuickCheck

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
renderSecret (Secret s) = comma <> text "SECRET =" <+> quotes (text s)

data Credential = Credential
   {
     credentialName :: RegularIdentifier
   , identity :: Identity
   , secret :: Maybe Secret
   }

instance Arbitrary Credential where
  arbitrary = Credential <$> arbitrary <*> arbitrary <*> arbitrary

instance Entity Credential where
  name = credentialName
  render s = text "CREATE CREDENTIAL" <+> renderName s <+>
            text "WITH IDENTITY =" <+> quotes (renderIdentity (identity s)) <+>
            maybe empty renderSecret (secret s) $+$
            text "GO"
