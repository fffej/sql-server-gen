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
import Data.Maybe (isJust)
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

data Parameter = Parameter
  {
    parameterName :: ParameterIdentifier
  , dataType      :: Type
  }

derive makeArbitrary ''Parameter

newtype ReturnType = ReturnType Type

instance Arbitrary ReturnType where
  arbitrary = liftM ReturnType $ arbitrary `suchThat` (liftM isJust renderValue)

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
