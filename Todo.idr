module Todo

import Record
import Sql

import Effects

%access public export

-- Todo definitions

todoSchema : Schema
todoSchema = [("id", Int), ("name", String), ("done", Bool)]

todoTable : Table Todo.todoSchema
todoTable = MkTable "todo"

Todo : Type
Todo = Record todoSchema

-- Database related

allColumns : NamedExprs Todo.todoSchema Todo.todoSchema
allColumns = 
      ("id" `isExpr` (Col Int "id")) $
      ("name" `isExpr` (Col String "name")) $
                ("done" `isLastExpr` (Col Bool "done"))

selectAll : Select Todo.todoSchema
selectAll = select allColumns {from=todoTable}

selectWhereId : Int -> Select Todo.todoSchema
selectWhereId id =
  select
    allColumns
      {from=todoTable}
      {where_= Col Int "id" =# Const id}

updateTodo : Todo -> Update
updateTodo todo = 
  update todoTable
    {values="name" `isExpr` (Const $ todo .. "name") $
             "done" `isLastExpr` (Const $ todo .. "done")}
    {where_ = Col Int "id" =# Const (todo .. "id")}







