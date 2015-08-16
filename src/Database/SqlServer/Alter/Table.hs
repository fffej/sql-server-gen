module Database.SqlServer.Alter.Table where

import Database.SqlServer.Create.Table
import Test.QuickCheck
import Database.SqlServer.Alter.Alter

data AlterTable = Identity
                | AddColumn  Column
                | DropColumn Column

selectValidColumnForAdd :: Table -> Gen Column
selectValidColumnForAdd = undefined

selectValidColumnForDrop :: Table -> Gen Column
selectValidColumnForDrop = undefined

alterTable :: Table -> Gen AlterTable
alterTable t = oneof
  [
    AddColumn <$> (selectValidColumnForAdd t)
  , DropColumn <$> (selectValidColumnForDrop t)
  , return Identity
  ]


