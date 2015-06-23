{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.SqlServer.Types.Table where

import Database.SqlServer.Types.Properties (NamedEntity,name,validIdentifiers)
import Database.SqlServer.Types.Identifiers (RegularIdentifier, renderRegularIdentifier)
import Database.SqlServer.Types.DataTypes (Type, renderDataType, collation, renderSparse, storageOptions, renderNullConstraint)
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

instance NamedEntity TableDefinition where
  name = tableName
  
data ColumnDefinition = ColumnDefinition
                        {
                          columnName :: RegularIdentifier
                        , dataType   :: Type
                        }

instance NamedEntity ColumnDefinition where
  name = columnName

instance Arbitrary ColumnDefinitions where
  arbitrary = liftM ColumnDefinitions $ (listOf1 arbitrary `suchThat` validIdentifiers)

derive makeArbitrary ''TableDefinition

derive makeArbitrary ''ColumnDefinition

renderColumnDefinitions :: ColumnDefinitions -> Doc
renderColumnDefinitions (ColumnDefinitions xs) = vcat (punctuate comma cols)
  where
    cols = map renderColumnDefinition xs

renderColumnDefinition :: ColumnDefinition -> Doc
renderColumnDefinition c = columnName' <+> columnType' <+> collation' <+>
                           sparse <+> nullConstraint
  where
    columnName'     = renderRegularIdentifier (columnName c)
    columnType'     = renderDataType $ dataType c
    collation'      = maybe empty renderCollation (collation (dataType c))
    sparse          = maybe empty renderSparse (storageOptions (dataType c))
    nullConstraint  = maybe empty renderNullConstraint (storageOptions (dataType c))

renderTableDefinition :: TableDefinition -> Doc
renderTableDefinition t = text "CREATE TABLE" <+> tableName' $$
                (parens $ renderColumnDefinitions (columnDefinitions t))
  where
    tableName' = renderRegularIdentifier (tableName t)


instance Show TableDefinition where
  show = render . renderTableDefinition

