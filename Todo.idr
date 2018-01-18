module Todo

import Record
import Sql

%access public export

todoSchema : Schema
todoSchema = [("name", String), ("done", Bool)]

todoTable : Table Todo.todoSchema
todoTable = MkTable "todo"

Todo : Type
Todo = Record todoSchema
