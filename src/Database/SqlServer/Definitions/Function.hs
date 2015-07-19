{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Definitions.Function
       (
         Function
       ) where

import Database.SqlServer.Definitions.Identifier hiding (unwrap)
import Database.SqlServer.Definitions.DataType
import Database.SqlServer.Definitions.Entity

import Test.QuickCheck
import Data.DeriveTH
import Text.PrettyPrint
import Data.Maybe (isJust,fromJust)
import Control.Monad

data NullOption = ReturnsNullOnNullInput
                | CalledOnNullInput

derive makeArbitrary ''NullOption

renderNullOption :: NullOption -> Doc
renderNullOption ReturnsNullOnNullInput = text "RETURNS NULL ON NULL INPUT"
renderNullOption CalledOnNullInput = text "CALLED ON NULL INPUT"

data FunctionOption = FunctionOption
    {
      encryption :: Bool
    , schemaBinding :: Bool
    , nullOption :: Maybe NullOption
    }

derive makeArbitrary ''FunctionOption

areThereAnyOptionsSet :: FunctionOption -> Bool
areThereAnyOptionsSet f = encryption f || schemaBinding f || isJust (nullOption f)

renderFunctionOptions :: FunctionOption -> Doc
renderFunctionOptions f
  | not (areThereAnyOptionsSet f) = empty
  | otherwise = text "WITH" <+>
                vcat (punctuate comma
                  (filter (/= empty) [ if encryption f then text "ENCRYPTION" else empty
                                     , if schemaBinding f then text "SCHEMABINDING" else empty
                                     , maybe empty renderNullOption (nullOption f) ]))

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
  arbitrary = liftM ReturnType $ arbitrary `suchThat` liftM isJust renderValue

renderReturnType :: ReturnType -> Doc
renderReturnType (ReturnType t) = renderDataType t

-- Safe because of instance of Arbitrary above
renderReturnValue :: ReturnType -> Doc
renderReturnValue (ReturnType t) = fromJust $ renderValue t

data ScalarFunction = ScalarFunction
   {
     scalarFunctionName :: RegularIdentifier
   , parameters :: [Parameter]
   , returnType :: ReturnType
   , functionOption :: FunctionOption
   }

derive makeArbitrary ''ScalarFunction

data Function = ScalarFunctionC ScalarFunction

derive makeArbitrary ''Function

instance Entity Function where
  toDoc (ScalarFunctionC f) = text "CREATE FUNCTION" <+> renderRegularIdentifier (scalarFunctionName f) <+>
                              parens (hcat (punctuate comma (map renderParameter (parameters f)))) $+$
                              text "RETURNS" <+> renderReturnType (returnType f) $+$
                              renderFunctionOptions (functionOption f) $+$
                              text "AS" $+$
                              text "BEGIN" $+$
                              text "RETURN" <+> renderReturnValue (returnType f) $+$
                              text "END" $+$ text "GO\n"

instance Show Function where
  show f = show (toDoc f)
