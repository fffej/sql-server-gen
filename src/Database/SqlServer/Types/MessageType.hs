{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Types.MessageType where

import Database.SqlServer.Types.Identifiers hiding (unwrap)
import Database.SqlServer.Types.User (UserDefinition,RoleDefinition,roleName,renderUserName)
import Database.SqlServer.Types.Entity

import Test.QuickCheck
import Data.DeriveTH
import Text.PrettyPrint

data Validation = None
                | Empty
                | WellFormedXml -- TODO valid XML

derive makeArbitrary ''Validation

data MessageTypeDefinition = MessageTypeDefinition
  {
    messageTypeName :: RegularIdentifier
  , authorization :: Maybe (Either UserDefinition RoleDefinition)
  , validation :: Maybe Validation
  }

derive makeArbitrary ''MessageTypeDefinition

-- Must be able to eliminate the duplication here
renderPreRequisites :: Either UserDefinition RoleDefinition -> Doc
renderPreRequisites (Left x)  = toDoc x $+$ text "GO"
renderPreRequisites (Right x) = toDoc x $+$ text "GO"

renderAuthorization :: Either UserDefinition RoleDefinition -> Doc
renderAuthorization (Left x)  = text "AUTHORIZATION" <+> renderUserName x
renderAuthorization (Right x) = text "AUTHORIZATION" <+> renderRegularIdentifier (roleName x) 

renderValidation :: Validation -> Doc
renderValidation None = text "VALIDATION = NONE"
renderValidation Empty = text "VALIDATION = EMPTY"
renderValidation WellFormedXml = text "VALIDATION = WELL_FORMED_XML"

instance Entity MessageTypeDefinition where
  toDoc m = maybe empty renderPreRequisites (authorization m) $+$
            text "CREATE MESSAGE TYPE" <+> (renderRegularIdentifier (messageTypeName m)) $+$
            maybe empty renderAuthorization (authorization m) $+$
            maybe empty renderValidation (validation m) 
            
