module Database.SqlServer.Alter.Table where

import Database.SqlServer.Definition.Table
import Test.QuickCheck
import Database.SqlServer.Alter.Alter

data AlterTable = Identity
                | AddColumn  ColumnDefinition
                | DropColumn ColumnDefinition

selectValidColumnForAdd :: Table -> Gen ColumnDefinition
selectValidColumnForAdd = undefined

selectValidColumnForDrop :: Table -> Gen ColumnDefinition
selectValidColumnForDrop = undefined

alterTable :: Table -> Gen AlterTable
alterTable t = oneof
  [
    AddColumn <$> (selectValidColumnForAdd t)
  , DropColumn <$> (selectValidColumnForDrop t)
  , return Identity
  ]


