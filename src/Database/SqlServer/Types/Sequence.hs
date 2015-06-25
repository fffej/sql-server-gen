module Database.SqlServer.Types.Sequence where

import Database.SqlServer.Types.Properties (NamedEntity,name,validIdentifiers)
import Database.SqlServer.Types.Identifiers (RegularIdentifier, renderRegularIdentifier)

import Text.PrettyPrint
import Test.QuickCheck

data BuiltInIntegerType = SequenceTinyInt
                        | SequenceSmallInt
                        | SequenceInt
                        | SequenceBitInt
                        | SequenceDecimal
                        | SequenceNumeric

data SequenceDefinition = SequenceDefinition
                {
                  sequenceName :: RegularIdentifier
                , integerType  :: BuiltInIntegerType
                , startWith    :: Maybe Integer
                , incrementBy  :: Maybe Integer
                , minValue     :: Maybe Integer
                , maxValue     :: Maybe Integer
                , cycle        :: Maybe Bool
                , cache        :: Maybe Integer
                }

instance NamedEntity SequenceDefinition where
  name = sequenceName

instance Arbitrary SequenceDefinition where
  arbitrary = undefined

renderSequence :: SequenceDefinition -> Doc
renderSequence = undefined
