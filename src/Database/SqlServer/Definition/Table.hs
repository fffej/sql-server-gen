{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Definition.Table
       (
         Table
       ) where

import Database.SqlServer.Definition.Identifier (RegularIdentifier, renderRegularIdentifier)
import Database.SqlServer.Definition.DataType (
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
  isTimestamp,
  isTypeForIndex
  )
  
import Database.SqlServer.Definition.Collation (renderCollation)
import Database.SqlServer.Definition.Entity

import Data.DeriveTH
import Test.QuickCheck
import Text.PrettyPrint
import Control.Monad

data ColumnDefinition = ColumnDefinition
  {
    columnName :: RegularIdentifier
  , dataType   :: Type
  }

instance Arbitrary ColumnDefinition where
  arbitrary = do
    n <- arbitrary
    t <- arbitrary
    return $ ColumnDefinition n t    

newtype ColumnDefinitions = ColumnDefinitions [ColumnDefinition]

data IndexType = PrimaryKey | Unique

data SortOrder = Ascending | Descending

newtype FillFactor = FillFactor Int

instance Arbitrary FillFactor where
  arbitrary = liftM FillFactor $ choose (1,100)

derive makeArbitrary ''IndexType

derive makeArbitrary ''SortOrder

renderIndexType :: IndexType -> Doc
renderIndexType PrimaryKey = text "PRIMARY KEY"
renderIndexType Unique = text "UNIQUE"

renderSortOrder :: SortOrder -> Doc
renderSortOrder Ascending = text "ASC"
renderSortOrder Descending = text "DESC"

renderFillFactor :: FillFactor -> Doc
renderFillFactor (FillFactor f) = text "WITH FILLFACTOR =" <+> int f

data TableConstraint = TableConstraint
  {
    constraintName :: RegularIdentifier
  , indexType :: IndexType
  , column :: RegularIdentifier
  , sortOrder :: Maybe SortOrder
  , fillFactor :: Maybe FillFactor
  }

generateTableConstraint :: ColumnDefinitions -> Gen (Maybe TableConstraint)
generateTableConstraint (ColumnDefinitions cd) = case filter (isTypeForIndex . dataType) cd of
  [] -> return Nothing
  xs -> do
    n <- arbitrary
    c <- columnName <$> elements xs
    it <- arbitrary
    so <- arbitrary
    ff <- arbitrary
    frequency
      [
        (0, return Nothing),
        (100, return $ Just (TableConstraint
           {
             constraintName = n
           , indexType = it
           , column = c
           , sortOrder = so
           , fillFactor = ff
           }))
      ]
  

renderTableConstraint :: TableConstraint -> Doc
renderTableConstraint t = comma <+> text "CONSTRAINT" <+>
                          renderRegularIdentifier (constraintName t) <+>
                          renderIndexType (indexType t) <+>
                          parens (
                            renderRegularIdentifier (column t) <+>
                            maybe empty renderSortOrder (sortOrder t)) <+>
                          maybe empty renderFillFactor (fillFactor t)

data Table = Table
  {
    tableName    :: RegularIdentifier
  , columnDefinitions :: ColumnDefinitions
  , tableConstraint :: Maybe TableConstraint
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
    f <- generateTableConstraint cols
    return $ Table nm cols f

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
  name = tableName
  toDoc t = text "CREATE TABLE" <+> renderName t $$
            parens ((renderColumnDefinitions (columnDefinitions t)) <>
            maybe empty renderTableConstraint (tableConstraint t)) $+$
            text "GO"

instance Show Table where
  show = show . toDoc
