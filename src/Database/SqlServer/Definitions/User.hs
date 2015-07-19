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
data UserDefinition = CreateUserWithoutLogin RegularIdentifier
                    | CreateUserWithCertificate RegularIdentifier ForFrom CertificateDefinition
                    | CreateUserWithLogin RegularIdentifier ForFrom LoginDefinition


derive makeArbitrary ''ForFrom
derive makeArbitrary ''UserDefinition

renderForFrom :: ForFrom -> Doc
renderForFrom For = text "FOR"
renderForFrom From = text "FROM"

renderCertificate :: CertificateDefinition -> Doc
renderCertificate c = text "CERTIFICATE" <+>
                      renderRegularIdentifier (certificateName c)

renderLogin :: LoginDefinition -> Doc
renderLogin l = text "LOGIN" <+>
                renderRegularIdentifier (loginName l)

renderUserName :: UserDefinition -> Doc
renderUserName (CreateUserWithoutLogin x) = renderRegularIdentifier x
renderUserName (CreateUserWithCertificate x _ _) = renderRegularIdentifier x
renderUserName (CreateUserWithLogin x _ _) = renderRegularIdentifier x

instance Entity UserDefinition where
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

data RoleDefinition = RoleDefinition
    {
      roleName :: RegularIdentifier
    , authorization :: Maybe UserDefinition
    }

derive makeArbitrary ''RoleDefinition

renderAuthorization :: UserDefinition -> Doc
renderAuthorization ud = text "AUTHORIZATION" <+> (renderUserName ud)

instance Entity RoleDefinition where
  toDoc rd = maybe empty toDoc (authorization rd) $+$ text "GO" $+$
             text "CREATE ROLE" <+> (renderRegularIdentifier $ roleName rd) <+>
             maybe empty renderAuthorization (authorization rd) 
             
instance Show RoleDefinition where
  show = show . toDoc 
