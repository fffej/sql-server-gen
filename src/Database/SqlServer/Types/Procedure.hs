{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Types.Procedure where

import Database.SqlServer.Types.Identifiers hiding (unwrap)
import Database.SqlServer.Types.DataTypes
import Database.SqlServer.Types.Properties

import Test.QuickCheck
import Data.DeriveTH
import Text.PrettyPrint
import Control.Monad
import Data.Ord

import qualified Data.Set as S

data ExecuteAsClause = Owner
                     | Self

derive makeArbitrary ''ExecuteAsClause

data ProcedureOption = Encryption
                     | Recompile
                     | ExecuteAs ExecuteAsClause

derive makeArbitrary ''ProcedureOption

type ProcedureOptions = S.Set ProcedureOption

newtype ParameterIdentifier = ParameterIdentifier { unwrapP :: RegularIdentifier } deriving (Ord,Eq)

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

instance Ord Parameter where
  compare = comparing parameterName

instance Eq Parameter where
  a == b = parameterName a == parameterName b

derive makeArbitrary ''Parameter

renderOut :: Bool -> Doc
renderOut True = text "OUTPUT"
renderOut False = empty

renderParameter :: Parameter -> Doc
renderParameter p = renderParameterIdentifier (parameterName p) <+> renderDataType (dataType p) <+> renderOut (isOutput p)

newtype Parameters = Parameters { unwrap :: S.Set Parameter } 

instance Arbitrary Parameters where
  arbitrary = do
    p <- listOf arbitrary `suchThat` validIdentifiers
    return $ Parameters (S.fromList p)

data ProcedureDefinition = ProcedureDefinition
  {
    procedureName :: RegularIdentifier
  , parameters    :: Parameters
  }

instance Eq ProcedureDefinition where
  a == b = procedureName a == procedureName b

instance Ord ProcedureDefinition where
  compare = comparing procedureName

instance NamedEntity ProcedureDefinition where
  name = procedureName

derive makeArbitrary ''ProcedureDefinition

-- Generating arbitrary SQL is perhaps a bit complicated.
statementBody :: String
statementBody = "select 1\n"

renderProcedureDefinition :: ProcedureDefinition -> Doc
renderProcedureDefinition p = text "CREATE PROCEDURE" <+> renderRegularIdentifier (procedureName p) $+$
                              hcat (punctuate comma (map renderParameter (S.toList $ unwrap $ parameters p))) <+> text "AS" $+$
                              text statementBody $+$
                              text "GO"
                              
                                                    
