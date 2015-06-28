{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.Table where

import Database.SqlServer.Types.Properties (NamedEntity,name,validIdentifiers)
import Database.SqlServer.Types.Identifiers (RegularIdentifier, renderRegularIdentifier)
import Database.SqlServer.Types.DataTypes (
  Type(..),
  renderDataType,
  collation,
  renderSparse,
  storageOptions,
  renderNullConstraint,
  nullOptions,
  storageSize,
  renderRowGuidConstraint,
  rowGuidOptions,
  isRowGuidCol
  )
  
import Database.SqlServer.Types.Collations (renderCollation)

import Test.QuickCheck
import Control.Monad
import Text.PrettyPrint

import Data.DeriveTH

newtype ColumnDefinitions = ColumnDefinitions [ColumnDefinition]

-- https://msdn.microsoft.com/en-us/library/ms174979.aspx
data TableDefinition = TableDefinition
             {
               tableName    :: RegularIdentifier
             , columnDefinitions :: ColumnDefinitions
             }

instance NamedEntity TableDefinition where
  name = tableName
  
data ColumnDefinition = ColumnDefinition
                        {
                          columnName :: RegularIdentifier
                        , dataType   :: Type
                        }

{-

Creating or altering table 'diDsDhXF3In' failed because the minimum row size would be 12387, including 14 bytes of internal overhead. This exceeds the maximum allowable table row size of 8060 bytes.

Calculating the internal overhead for column size looks a little impossible (alignment issues).

-}
columnConstraintsSatisfied :: [ColumnDefinition] -> Bool
columnConstraintsSatisfied xs = allValidIdentifiers &&
                                length (filter isTimeStamp xs) <= 1 && -- only a single time stamp column is allowed
                                totalColumnSizeBytes <= 8060 &&
                                length (filter oneGuidCol xs) <= 1 -- only one row guid col allowed
                                
  where
    totalColumnSizeBits = 32 + (sum $ map (storageSize . dataType) xs)
    totalColumnSizeBytes = totalColumnSizeBits `div` 8 + (if totalColumnSizeBits `rem` 8 /= 0 then 1 else 0)
    allValidIdentifiers = validIdentifiers xs
    isTimeStamp c = case dataType c of
      (Timestamp _) -> True
      _             -> False
    oneGuidCol c = case dataType c of
      (UniqueIdentifier s) -> maybe False isRowGuidCol s
      _                    -> False


instance NamedEntity ColumnDefinition where
  name = columnName

instance Arbitrary ColumnDefinitions where
  arbitrary = liftM ColumnDefinitions $ (listOf1 arbitrary `suchThat` columnConstraintsSatisfied)

derive makeArbitrary ''TableDefinition

derive makeArbitrary ''ColumnDefinition

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

renderTableDefinition :: TableDefinition -> Doc
renderTableDefinition t = text "CREATE TABLE" <+> tableName' $$
                (parens $ renderColumnDefinitions (columnDefinitions t)) $+$
                text "GO"
  where
    tableName' = renderRegularIdentifier (tableName t)


instance Show TableDefinition where
  show = render . renderTableDefinition

