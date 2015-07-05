{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.User where

import Database.SqlServer.Types.Identifiers
import Database.SqlServer.Types.Entity
import Database.SqlServer.Types.Certificate
import Database.SqlServer.Types.Login

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

