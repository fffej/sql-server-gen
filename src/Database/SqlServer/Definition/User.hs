module Database.SqlServer.Definition.User
       (
         User
       , Role
       ) where

import Database.SqlServer.Definition.Identifier
import Database.SqlServer.Definition.Entity
import Database.SqlServer.Definition.Certificate
import Database.SqlServer.Definition.Login

import Test.QuickCheck
import Text.PrettyPrint

data ForFrom = For | From

-- TODO asymmetric key
data User = CreateUserWithoutLogin RegularIdentifier
          | CreateUserWithCertificate RegularIdentifier ForFrom Certificate
          | CreateUserWithLogin RegularIdentifier ForFrom Login

instance Arbitrary User where
  arbitrary = oneof
    [
      CreateUserWithoutLogin <$> arbitrary
    , CreateUserWithCertificate <$> arbitrary <*> arbitrary <*> arbitrary
    , CreateUserWithLogin <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary ForFrom where
  arbitrary = elements [For,From]

renderForFrom :: ForFrom -> Doc
renderForFrom For = text "FOR"
renderForFrom From = text "FROM"

renderCertificate :: Certificate -> Doc
renderCertificate c = text "CERTIFICATE" <+>
                      renderRegularIdentifier (certificateName c)

renderLogin :: Login -> Doc
renderLogin l = text "LOGIN" <+>
                renderRegularIdentifier (loginName l)

renderUserName :: User -> Doc
renderUserName (CreateUserWithoutLogin x) = renderRegularIdentifier x
renderUserName (CreateUserWithCertificate x _ _) = renderRegularIdentifier x
renderUserName (CreateUserWithLogin x _ _) = renderRegularIdentifier x

instance Entity User where
  name (CreateUserWithoutLogin x) = x
  name (CreateUserWithLogin x _ _) = x
  name (CreateUserWithCertificate x _ _) = x
  toDoc (CreateUserWithoutLogin x) = text "CREATE USER" <+>
                                     renderRegularIdentifier x <+>
                                     text "WITHOUT LOGIN"
  toDoc (CreateUserWithCertificate nm ff cert) = toDoc cert $+$
                                                 text "GO" $+$
                                                 text "CREATE USER" <+>
                                                 renderRegularIdentifier nm <+>
                                                 renderForFrom ff <+>
                                                 renderCertificate cert
  toDoc (CreateUserWithLogin nm ff lg) = toDoc lg $+$
                                         text "GO" $+$
                                         text "CREATE USER" <+>
                                         renderRegularIdentifier nm <+>
                                         renderForFrom ff <+>
                                         renderLogin lg

instance Show User where
  show = show . toDoc

data Role = Role
    {
      roleName :: RegularIdentifier
    , authorization :: Maybe User
    }

instance Arbitrary Role where
  arbitrary = Role <$> arbitrary <*> arbitrary

renderAuthorization :: User -> Doc
renderAuthorization ud = text "AUTHORIZATION" <+> renderUserName ud

instance Entity Role where
  name = roleName
  toDoc rd = maybe empty toDoc (authorization rd) $+$ text "GO" $+$
             text "CREATE ROLE" <+> renderName rd <+> 
             maybe empty renderAuthorization (authorization rd) 
             
instance Show Role where
  show = show . toDoc 
