{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.Table where

import Database.SqlServer.Types.Identifiers
import Database.SqlServer.Types.DataTypes
import Database.SqlServer.Types.Collations (collations, Collation, renderCollation)

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Data.List (nub,intersperse)

import Text.PrettyPrint

import Data.DeriveTH

newtype ColumnDefinitions = ColumnDefinitions [ColumnDefinition]

-- https://msdn.microsoft.com/en-us/library/ms174979.aspx
data TableDefinition = TableDefinition
             {
               tableName    :: RegularIdentifier
             , columnDefinitions :: ColumnDefinitions
             }

uniqueNames :: [ColumnDefinition] -> Bool
uniqueNames xs = length xs == length (nub $ map columnName xs)

data StorageOptions = Sparse
                    | SparseNull
                    | NotNull
                    | Null

renderSparse :: StorageOptions -> Doc
renderSparse Sparse = text "SPARSE"
renderSparse _      = empty

renderNullConstraint :: StorageOptions -> Doc
renderNullConstraint NotNull = text "NOT NULL"
renderNullConstraint Null    = text "NULL"
renderNullConstraint _       = empty

data ColumnDefinition = ColumnDefinition
                        {
                          columnName :: RegularIdentifier
                        , dataType   :: Type
                        , storageOptions :: Maybe StorageOptions
                        }

instance Arbitrary ColumnDefinitions where
  arbitrary = liftM ColumnDefinitions $ listOf1 arbitrary   

derive makeArbitrary ''TableDefinition

derive makeArbitrary ''ColumnDefinition

derive makeArbitrary ''StorageOptions

renderColumnDefinitions :: ColumnDefinitions -> Doc
renderColumnDefinitions (ColumnDefinitions xs) = vcat (punctuate comma cols)
  where
    cols = map renderColumnDefinition xs

renderColumnDefinition :: ColumnDefinition -> Doc
renderColumnDefinition c = columnName' <+> columnType' <+> collation' <+>
                           sparse <+> nullConstraint
  where
    columnName'     = text (show $ columnName c)
    columnType'     = renderDataType $ dataType c
    collation'      = maybe empty renderCollation (collation (dataType c))
    sparse          = maybe empty renderSparse (storageOptions c)
    nullConstraint  = maybe empty renderNullConstraint (storageOptions c)

instance Show TableDefinition where
  show t = render doc 
    where
      doc = text "CREATE TABLE" <+> tableName' $$
            (parens $ renderColumnDefinitions (columnDefinitions t))
      tableName' = text (show $ tableName t)


