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

render_sparse :: StorageOptions -> String
render_sparse Sparse = "SPARSE"
render_sparse _      = ""

render_null_constraint :: StorageOptions -> String
render_null_constraint NotNull = "NOT NULL"
render_null_constraint Null    = "NULL"
render_null_constraint _       = ""

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

instance Show ColumnDefinitions where
  show (ColumnDefinitions xs) = concat $ intersperse ",\n" $ map show xs

instance Show ColumnDefinition where
  show c = render (renderColumnDefinition c)

renderColumnDefinition :: ColumnDefinition -> Doc
renderColumnDefinition c = columnName <+> columnType <+> collationD <+>
                           sparse <+> nullConstraint
  where
    columnName     = text (show $ column_name c)
    columnType     = text (render_data_type $ data_type c)
    collationD     = maybe empty render_collation (collation (data_type c))
    sparse         = maybe empty (text . render_sparse) (storage_options c)
    nullConstraint = maybe empty (text . render_null_constraint)  (storage_options c)

instance Show TableDefinition where
  show t = render doc 
    where
      doc = text "CREATE TABLE" <+> tableName <+>
           (parens $ text (show (column_definitions t)))
      tableName = text (show $ table_name t)


