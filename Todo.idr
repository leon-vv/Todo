module Todo

import Record
import Sql

import Effects

%access public export

-- Todo definitions

todoSchema : Schema
todoSchema = [("name", String), ("done", Bool)]

todoTable : Table Todo.todoSchema
todoTable = MkTable "todo"

Todo : Type
Todo = Record todoSchema

-- Database related

allColumns : NamedExprs Todo.todoSchema Todo.todoSchema
allColumns = ("name" `isExpr` (Col String "name") $
                "done" `isLastExpr` (Col Bool "done"))

selectAll : Select Todo.todoSchema
selectAll = select allColumns {from=todoTable}

selectWhereName : String -> Select Todo.todoSchema
selectWhereName name =
  select
    allColumns
      {from=todoTable}
      {where_= Col String "name" =# Const name}


