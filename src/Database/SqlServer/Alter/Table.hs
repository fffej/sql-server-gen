module Database.SqlServer.Alter.Table where

import Database.SqlServer.Create.Table
import Database.SqlServer.Create.Entity
import Database.SqlServer.Create.Identifier

import Test.QuickCheck
import Database.SqlServer.Alter.Alter

import Text.PrettyPrint

data AlterTable = AddColumn  Table Column
                | DropColumn Table Column

selectValidColumnForAdd :: Table -> Gen Column
selectValidColumnForAdd = undefined

selectValidColumnForDrop :: Table -> Gen Column
selectValidColumnForDrop = undefined

alterTable :: Table -> Gen AlterTable
alterTable t = oneof
  [
    AddColumn t  <$> arbitrary -- TODO this'll violate constraints sometimes...
  , DropColumn t <$> (selectValidColumnForDrop t)
  ]

renderAddColumn :: Table -> Column -> Doc
renderAddColumn t c = text "ALTER TABLE" <+> (renderName t) <+> text "ADD" <+> (renderColumn c) $+$
                      text "GO\n"

renderDropColumn :: Table -> Column -> Doc
renderDropColumn t c = text "DROP TABLE" <+> (renderName t) <+> text "DROP" <+> (renderRegularIdentifier (columnName c)) $+$
                       text "GO\n"

instance Alter AlterTable where
  render (AddColumn t c)  = renderAddColumn t c 
  render (DropColumn t c) = renderDropColumn t c

