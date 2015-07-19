{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Definitions.User where

import Database.SqlServer.Definitions.Identifiers
import Database.SqlServer.Definitions.Entity
import Database.SqlServer.Definitions.Certificate
import Database.SqlServer.Definitions.Login

import Test.QuickCheck
import Text.PrettyPrint
import Data.DeriveTH

data ForFrom = For | From

-- TODO asymmetric key
data User = CreateUserWithoutLogin RegularIdentifier
                    | CreateUserWithCertificate RegularIdentifier ForFrom Certificate
                    | CreateUserWithLogin RegularIdentifier ForFrom Login


derive makeArbitrary ''ForFrom
derive makeArbitrary ''User

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

data Role = Role
    {
      roleName :: RegularIdentifier
    , authorization :: Maybe User
    }

derive makeArbitrary ''Role

renderAuthorization :: User -> Doc
renderAuthorization ud = text "AUTHORIZATION" <+> renderUserName ud

instance Entity Role where
  toDoc rd = maybe empty toDoc (authorization rd) $+$ text "GO" $+$
             text "CREATE ROLE" <+> renderRegularIdentifier (roleName rd) <+>
             maybe empty renderAuthorization (authorization rd) 
             
instance Show Role where
  show = show . toDoc 
