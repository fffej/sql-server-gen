{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.Table where

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
import Text.PrettyPrint

import Data.DeriveTH


-- https://msdn.microsoft.com/en-us/library/ms174979.aspx
data TableDefinition = TableDefinition
             {
               tableName    :: RegularIdentifier
             , columnDefinitions :: [ColumnDefinition]
             }

data ColumnDefinition = ColumnDefinition
                        {
                          columnName :: RegularIdentifier
                        , dataType   :: Type
                        }

columnConstraintsSatisfied :: [ColumnDefinition] -> Bool
columnConstraintsSatisfied xs = length (filter isTimeStamp xs) <= 1 && 
                                totalColumnSizeBytes <= 8060 &&
                                length (filter oneGuidCol xs) <= 1 
                                
  where
    totalColumnSizeBits = 32 + sum (map (storageSize . dataType) xs)
    totalColumnSizeBytes = totalColumnSizeBits `div` 8 + (if totalColumnSizeBits `rem` 8 /= 0 then 1 else 0)
    isTimeStamp c = case dataType c of
      (Timestamp _) -> True
      _             -> False
    oneGuidCol c = case dataType c of
      (UniqueIdentifier s) -> maybe False isRowGuidCol s
      _                    -> False

derive makeArbitrary ''TableDefinition

derive makeArbitrary ''ColumnDefinition

renderColumnDefinitions :: [ColumnDefinition] -> Doc
renderColumnDefinitions xs = vcat (punctuate comma cols)
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
                parens (renderColumnDefinitions (columnDefinitions t)) $+$
                text "GO"
  where
    tableName' = renderRegularIdentifier (tableName t)


instance Show TableDefinition where
  show = render . renderTableDefinition

