{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Types.Function where

import Database.SqlServer.Types.Identifiers hiding (unwrap)
import Database.SqlServer.Types.DataTypes
import Database.SqlServer.Types.Entity

import Test.QuickCheck
import Data.DeriveTH
import Text.PrettyPrint
import Data.Maybe (isJust,fromJust)
import Control.Monad

data NullOption = ReturnsNullOnNullInput
                | CalledOnNullInput

derive makeArbitrary ''NullOption

data FunctionOption = FunctionOption
    {
      encryption :: Maybe Bool
    , schemaBinding :: Maybe Bool
    , nullOption :: Maybe NullOption
    }

derive makeArbitrary ''FunctionOption

newtype InputType = InputType Type

-- A time stamp can not be passed in as an argument to a function
instance Arbitrary InputType where
  arbitrary = liftM InputType $ arbitrary `suchThat` (not . isTimestamp)

renderInputDataType :: InputType -> Doc
renderInputDataType (InputType t) = renderDataType t

data Parameter = Parameter
  {
    parameterName :: ParameterIdentifier
  , dataType      :: InputType
  }

renderParameter :: Parameter -> Doc
renderParameter p = renderParameterIdentifier (parameterName p) <+> renderInputDataType (dataType p) 

derive makeArbitrary ''Parameter

newtype ReturnType = ReturnType Type

instance Arbitrary ReturnType where
  arbitrary = liftM ReturnType $ arbitrary `suchThat` (liftM isJust renderValue)

renderReturnType :: ReturnType -> Doc
renderReturnType (ReturnType t) = renderDataType t

-- Safe because of instance of Arbitrary above
renderReturnValue :: ReturnType -> Doc
renderReturnValue (ReturnType t) = fromJust $ renderValue t

data ScalarFunctionDefinition = ScalarFunctionDefinition
   {
     scalarFunctionName :: RegularIdentifier
   , parameters :: [Parameter]
   , returnType :: ReturnType
   , functionBody :: String
   , functionOption :: FunctionOption
   }

derive makeArbitrary ''ScalarFunctionDefinition

data FunctionDefinition = ScalarFunction ScalarFunctionDefinition

derive makeArbitrary ''FunctionDefinition

instance Entity FunctionDefinition where
  toDoc (ScalarFunction f) = text "CREATE FUNCTION" <+> renderRegularIdentifier (scalarFunctionName f) <+>
                             (parens $ hcat (punctuate comma (map renderParameter (parameters f)))) $+$
                             text "RETURNS" <+> renderReturnType (returnType f) <+> text "AS" $+$
                             text "BEGIN" $+$
                             text "RETURN" <+> renderReturnValue (returnType f) $+$
                             text "END" $+$ text "GO\n"

instance Show FunctionDefinition where
  show f = show (toDoc f)
