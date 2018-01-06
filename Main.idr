import Event

import Http

import Record
import Record.JS

import Sql
import Sql.JS

import Effects

todoSchema : Schema
todoSchema = [("name", String), ("done", Bool)]

todoTable : Table Main.todoSchema
todoTable = MkTable "todo"

Todos : Type
Todos = List (Record Main.todoSchema)

showTodos : Todos -> String
showTodos ts = unlines (map (\rec => rec .. "name") ts)

data State =
    StartingUp Pool (Event Todos)
  | Listening Pool Todos (Event (Request, Response))

selectQuery : Select Main.todoSchema
selectQuery = SelectQuery todoSchema todoTable (Const True) Nil

pool : JS_IO Pool
pool = makePool {user="leonvv"} {database="leonvv"} {password="leonvv"}

initialState : JS_IO State
initialState = do
  p <- pool
  todosEv <- runSelectQuery selectQuery p
  pure (StartingUp p todosEv)

nextState : State -> JS_IO State
nextState st@(StartingUp p ev) = do
  todos <- ev
  case todos of
       Nothing => pure st
       Just lst => do
         server <- httpServer
         pure (Listening p lst server)
nextState st@(Listening p todos server) = do
  maybeReq <- server
  (case maybeReq of
      Just (req, res) => do
        write res (showTodos todos)
      Nothing => pure ())
  pure st

main : JS_IO ()
main = run initialState nextState
