{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Database.SqlServer.Definition.PartitionFunction
       (
         PartitionFunction
       ) where

import Prelude hiding (Left,Right)

import Database.SqlServer.Definition.Identifier hiding (unwrap)
import Database.SqlServer.Definition.DataType
import Database.SqlServer.Definition.Entity

import Data.Maybe (fromJust)
import Data.DeriveTH
import Text.PrettyPrint
import Test.QuickCheck

{-
  All data types are valid for use as partitioning columns, except text, 
  ntext, image, xml, timestamp, varchar(max), nvarchar(max), varbinary(max), 
  alias data types, or CLR user-defined data types.
-}
newtype InputParameterType = InputParameterType { unwrap :: Type }

instance Arbitrary InputParameterType where
  arbitrary = do
    x <- arbitrary `suchThat` isSupportedTypeForPartitionFunction
    return (InputParameterType x)

data Range = Left | Right

derive makeArbitrary ''Range

renderRange :: Range -> Doc
renderRange Left = text "LEFT"
renderRange Right = text "RIGHT"

-- TODO boundaryValues need to all be the same type....
data PartitionFunction = PartitionFunction
  {
    partitionFunctionName :: RegularIdentifier
  , inputType :: InputParameterType
  , range :: Range
  , boundaryValues :: [Type]
  }

renderValues :: [Type] -> Doc
renderValues xs = parens (vcat (punctuate comma $ map (fromJust . renderValue) xs))

derive makeArbitrary ''PartitionFunction

instance Entity PartitionFunction where
  name = partitionFunctionName
  toDoc a = text "CREATE PARTITION FUNCTION" <+> renderName a <+> 
            parens (renderDataType (unwrap $ inputType a)) $+$
            text "AS RANGE" <+> renderRange (range a) <+> text "FOR VALUES" <+>
            renderValues (boundaryValues a)
  
instance Show PartitionFunction where
  show = show . toDoc