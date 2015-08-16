module Database.SqlServer.Alter.Table where

import Database.SqlServer.Create.Table
import Database.SqlServer.Create.Entity hiding (render)
import Database.SqlServer.Create.Identifier

import Test.QuickCheck
import Database.SqlServer.Alter.Alter

import Text.PrettyPrint hiding (render)

data AlterTable = AddColumn  Table Column
                | DropColumn Table Column

selectValidColumnForAdd :: Table -> Gen Column
selectValidColumnForAdd t = arbitrary `suchThat` (\x -> columnConstraintsSatisfied (x : unpack (columns t)))

selectValidColumnForDrop :: Table -> Gen Column
selectValidColumnForDrop t = elements (unpack (columns t))

alterTable :: Table -> Gen AlterTable
alterTable t = oneof
  [
    AddColumn t  <$> arbitrary
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

instance Show AlterTable where
  show = show . render
