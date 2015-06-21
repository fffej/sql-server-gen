{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.Grammar where

import Database.SqlServer.Types.Identifiers
import Database.SqlServer.Types.DataTypes
import Database.SqlServer.Types.Collations (collations, Collation, render_collation)

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
               table_name    :: RegularIdentifier
             , column_definitions :: ColumnDefinitions
             }

uniqueNames :: [ColumnDefinition] -> Bool
uniqueNames xs = length xs == length (nub $ map column_name xs)

data StorageOptions = Sparse
                    | SparseNull
                    | NotNull
                    | Null

render_sparse :: StorageOptions -> Doc
render_sparse Sparse = text "SPARSE"
render_sparse _      = empty

render_null_constraint :: StorageOptions -> Doc
render_null_constraint NotNull = text "NOT NULL"
render_null_constraint Null    = text "NULL"
render_null_constraint _       = empty

data ColumnDefinition = ColumnDefinition
                        {
                          column_name :: RegularIdentifier
                        , data_type   :: Type
                        , storage_options :: Maybe StorageOptions
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
renderColumnDefinition c = columnName <+> columnType <+> collationD <+>
                           sparse <+> nullConstraint
  where
    columnName     = text (show $ column_name c)
    columnType     = render_data_type $ data_type c
    collationD     = maybe empty render_collation (collation (data_type c))
    sparse         = maybe empty render_sparse (storage_options c)
    nullConstraint = maybe empty render_null_constraint (storage_options c)

instance Show TableDefinition where
  show t = render doc 
    where
      doc = text "CREATE TABLE" <+> tableName $$
           (parens $ renderColumnDefinitions (column_definitions t))
      tableName = text (show $ table_name t)


