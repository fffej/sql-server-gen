{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Definitions.Table where

import Database.SqlServer.Definitions.Identifiers (RegularIdentifier, renderRegularIdentifier)
import Database.SqlServer.Definitions.DataTypes (
  Type,
  renderDataType,
  collation,
  renderSparse,
  storageOptions,
  renderNullConstraint,
  nullOptions,
  storageSize,
  renderRowGuidConstraint,
  rowGuidOptions,
  isTimestamp
  )
  
import Database.SqlServer.Definitions.Collations (renderCollation)
import Database.SqlServer.Definitions.Entity

import Test.QuickCheck
import Text.PrettyPrint

import Data.DeriveTH

data ColumnDefinition = ColumnDefinition
                        {
                          columnName :: RegularIdentifier
                        , dataType   :: Type
                        }

newtype ColumnDefinitions = ColumnDefinitions [ColumnDefinition]

data Table = Table
             {
               tableName    :: RegularIdentifier
             , columnDefinitions :: ColumnDefinitions
             }

columnConstraintsSatisfied :: [ColumnDefinition] -> Bool
columnConstraintsSatisfied xs = length (filter columnIsTimestamp xs) <= 1 && 
                                totalColumnSizeBytes <= 8060 &&
                                length (filter (rowGuidOptions . dataType) xs) <= 1 
  where
    totalColumnSizeBits = (length xs * 8) + 32 + sum (map (storageSize . dataType) xs)
    totalColumnSizeBytes = totalColumnSizeBits `div` 8 + (if totalColumnSizeBits `rem` 8 /= 0 then 8 else 0)
    columnIsTimestamp = isTimestamp . dataType
    
instance Arbitrary Table where
  arbitrary = do
    cols <- arbitrary 
    nm <- arbitrary
    return $ Table nm cols

derive makeArbitrary ''ColumnDefinition

instance Arbitrary ColumnDefinitions where
  arbitrary = do
    cols <- listOf1 arbitrary `suchThat` columnConstraintsSatisfied
    return $ ColumnDefinitions cols

renderColumnDefinitions :: ColumnDefinitions -> Doc
renderColumnDefinitions (ColumnDefinitions xs) = vcat (punctuate comma cols)
  where
    cols = map renderColumnDefinition xs

renderColumnDefinition :: ColumnDefinition -> Doc
renderColumnDefinition c = columnName' <+> columnType' <+> collation' <+>
                           sparse <+> nullConstraint <+> rowGuidConstraint
  where
    columnName'       = renderRegularIdentifier (columnName c)
    columnType'       = renderDataType $ dataType c
    collation'        = maybe empty renderCollation (collation (dataType c))
    sparse            = maybe empty renderSparse (storageOptions (dataType c))
    nullConstraint    = maybe empty renderNullConstraint (nullOptions (dataType c))
    rowGuidConstraint = renderRowGuidConstraint (rowGuidOptions (dataType c))

instance Entity Table where
  toDoc t = text "CREATE TABLE" <+> tableName' $$
            parens (renderColumnDefinitions (columnDefinitions t)) $+$
            text "GO"
    where
      tableName' = renderRegularIdentifier (tableName t)

