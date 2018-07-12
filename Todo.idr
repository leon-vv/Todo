module Todo

import Record
import Sql

import Effects
import Data.List.Quantifiers

%access public export

-- Todo definitions

todoSchema : Schema
todoSchema = [("id", Maybe Int), ("name", String), ("done", Bool)]

todoTable : Table Todo.todoSchema
todoTable = MkTable "todo"

Todo : Type
Todo = Record todoSchema

-- Database related

allColumns : NamedExprs Todo.todoSchema Todo.todoSchema
allColumns = 
      ("id" `isExpr` (Col (Maybe Int) "id")) $
      ("name" `isExpr` (Col String "name")) $
                ("done" `isLastExpr` (Col Bool "done"))

selectAll : Select Todo.todoSchema
selectAll = select allColumns
      {a=Sql.Nullable Sql.Int}
      {from=todoTable}
      {orderBy = Just (Col (Maybe Int) "id")}

selectWhereId : Int -> Select Todo.todoSchema
selectWhereId id =
  select
    allColumns
      {a=Sql.Int} -- Ugly to pass in unused type. Improvement neccessary
      {from=todoTable}
      {where_= Col (Maybe Int) "id" =# Const (Just id)}

updateTodo : Todo -> Update
updateTodo todo = 
  update todoTable
    {values="name" `isExpr` (Const $ todo .. "name") $
             "done" `isLastExpr` (Const $ todo .. "done")}
    {where_ = Col (Maybe Int) "id" =# Const (todo .. "id")}

insertTodo : Todo -> Insert
insertTodo t =
  InsertQuery todoTable
    (("name" `isExpr` (Const $ (t .. "name")))
      $ ("done" `isLastExpr` (Const $ (t .. "done"))))
 

removeTodoWithId : Int -> Delete
removeTodoWithId id =
  DeleteQuery
    todoTable
    (Col (Maybe Int) "id" =# Const (Just id))



