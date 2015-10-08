module Database.SqlServer.Create.PartitionFunction
       (
         PartitionFunction
       ) where

import Prelude hiding (Left, Right)

import Database.SqlServer.Create.Identifier hiding (unwrap)
import Database.SqlServer.Create.DataType
import Database.SqlServer.Create.Value
import Database.SqlServer.Create.Entity

import Data.List (nub)
import Text.PrettyPrint hiding (render)
import Test.QuickCheck

import Data.Maybe (fromJust)

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

instance Arbitrary Range where
  arbitrary = elements [Left, Right]

renderRange :: Range -> Doc
renderRange Left = text "LEFT"
renderRange Right = text "RIGHT"

-- TODO boundaryValues need to all be the same type....
data PartitionFunction = PartitionFunction
  {
    partitionFunctionName :: RegularIdentifier
  , inputType :: InputParameterType
  , range :: Range
  , boundaryValues :: [SQLValue]
  }

renderValues :: [SQLValue] -> Doc
renderValues xs = parens (vcat (punctuate comma $ map renderValue xs))

instance Arbitrary PartitionFunction where
  arbitrary = do
    n <- arbitrary
    t <- arbitrary
    r <- arbitrary
    let b = fromJust $ value (unwrap t) -- otherwise an error in my code
    bv <- listOf b
    return $ PartitionFunction n t r (nub bv)

instance Entity PartitionFunction where
  name = partitionFunctionName
  render a = text "CREATE PARTITION FUNCTION" <+> renderName a <+>
            parens (renderDataType (unwrap $ inputType a)) $+$
            text "AS RANGE" <+> renderRange (range a) <+> text "FOR VALUES" <+>
            renderValues (boundaryValues a) $+$
            text "GO\n"

instance Show PartitionFunction where
  show = show . render
