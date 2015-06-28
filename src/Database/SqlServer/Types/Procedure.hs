{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Types.Procedure where

import Database.SqlServer.Types.Identifiers hiding (unwrap)
import Database.SqlServer.Types.DataTypes
import Database.SqlServer.Types.Properties

import Test.QuickCheck
import Data.DeriveTH
import Data.Word (Word16)
import Text.PrettyPrint
import Data.Maybe (isJust)
import Control.Monad

import qualified Data.Set as S

data ExecuteAsClause = Owner
                     | Self

derive makeArbitrary ''ExecuteAsClause

data ProcedureOption = Encryption
                     | Recompile
                     | ExecuteAs ExecuteAsClause

derive makeArbitrary ''ProcedureOption

type ProcedureOptions = S.Set ProcedureOption

newtype ParameterIdentifier = ParameterIdentifier { unwrapP :: RegularIdentifier }

derive makeArbitrary ''ParameterIdentifier

renderParameterIdentifier :: ParameterIdentifier -> Doc
renderParameterIdentifier (ParameterIdentifier p) = text "@" <> renderRegularIdentifier p

data Parameter = Parameter
  {
    parameterName :: ParameterIdentifier
  , dataType      :: Type
  , isOutput      :: Bool
  }

instance NamedEntity Parameter where
  name = unwrapP . parameterName

derive makeArbitrary ''Parameter

renderOut :: Bool -> Doc
renderOut True = text "OUTPUT"
renderOut False = empty

renderParameter :: Parameter -> Doc
renderParameter p = renderParameterIdentifier (parameterName p) <+> renderDataType (dataType p) <+> renderOut (isOutput p)

newtype Parameters = Parameters { unwrap :: [Parameter] } 

instance Arbitrary Parameters where
  arbitrary = liftM Parameters (listOf arbitrary `suchThat` validIdentifiers)

data ProcedureDefinition = ProcedureDefinition
  {
    procedureName :: RegularIdentifier
  , parameters    :: Parameters
  }

instance NamedEntity ProcedureDefinition where
  name = procedureName

derive makeArbitrary ''ProcedureDefinition

-- Generating arbitrary SQL is perhaps a bit complicated.
statementBody :: String
statementBody = "select 1\n"

renderProcedureDefinition :: ProcedureDefinition -> Doc
renderProcedureDefinition p = text "CREATE PROCEDURE" <+> renderRegularIdentifier (procedureName p) $+$
                              hcat (punctuate comma (map renderParameter (unwrap $ parameters p))) <+> text "AS" $+$
                              text statementBody $+$
                              text "GO"
                              
                                                    
