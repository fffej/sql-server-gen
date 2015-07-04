{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Types.Procedure where

import Database.SqlServer.Types.Identifiers hiding (unwrap)
import Database.SqlServer.Types.DataTypes
import Database.SqlServer.Types.Renderable

import Test.QuickCheck
import Data.DeriveTH
import Text.PrettyPrint

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

derive makeArbitrary ''Parameter

renderOut :: Bool -> Doc
renderOut True = text "OUTPUT"
renderOut False = empty

renderParameter :: Parameter -> Doc
renderParameter p = renderParameterIdentifier (parameterName p) <+> renderDataType (dataType p) <+> renderOut (isOutput p)


data ProcedureDefinition = ProcedureDefinition
  {
    procedureName :: RegularIdentifier
  , parameters    :: [Parameter]
  }

derive makeArbitrary ''ProcedureDefinition

-- Generating arbitrary SQL is perhaps a bit complicated.
statementBody :: String
statementBody = "select 1\n"

instance RenderableEntity ProcedureDefinition where
  toDoc p = text "CREATE PROCEDURE" <+> renderRegularIdentifier (procedureName p) $+$
                              hcat (punctuate comma (map renderParameter (parameters p))) <+> text "AS" $+$
                              text statementBody $+$
                              text "GO"
                              
