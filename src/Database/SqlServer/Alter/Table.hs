module Database.SqlServer.Alter.Table where

import Database.SqlServer.Create.Table
import Database.SqlServer.Create.Entity hiding (render)
import Database.SqlServer.Create.Identifier
import Database.SqlServer.Create.Database

import Database.SqlServer.Alter.Alter

import Data.List 
import Test.QuickCheck


import Text.PrettyPrint hiding (render)

data AlterTable = AddColumn  Table Column
                | DropColumn Table Column

selectValidColumnForAdd :: Table -> Gen Column
selectValidColumnForAdd t = arbitrary `suchThat` (\x -> columnConstraintsSatisfied (x : unpack (columns t)))

selectValidColumnForDrop :: Table -> Gen Column
selectValidColumnForDrop t = elements (unpack (columns t)) -- TODO constraints

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
renderDropColumn t c = text "ALTER TABLE" <+> (renderName t) <+> text "DROP" <+> (renderRegularIdentifier (columnName c)) $+$
                       text "GO\n"

updateTable :: [Table] -> Maybe Table -> [Table]
updateTable xs Nothing  = xs
updateTable xs (Just t) = map (\x -> if name x == name t then t else x) xs

applyAddColumn :: Database -> Table -> Column -> Database
applyAddColumn d t c = d { tables = updateTable (tables d) newTable }
  where
    origTables = tables d
    oldTable = find (\x -> name x == name t) (tables d)
    newTable = fmap (flip addColumn c) oldTable

applyDropColumn :: Database -> Table -> Column -> Database
applyDropColumn d t c = undefined

instance Alter AlterTable where
  render (AddColumn t c)  = renderAddColumn t c 
  render (DropColumn t c) = renderDropColumn t c

  apply d (AddColumn t c)  = applyAddColumn d t c
  apply d (DropColumn t c) = applyDropColumn d t c

instance Show AlterTable where
  show = show . render
