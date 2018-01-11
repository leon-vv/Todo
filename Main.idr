import Event

import Html
import Http

import Record

import Sql
import Sql.JS

import Effects

%include Node "event/runtime.js"

todoSchema : Schema
todoSchema = [("name", String), ("done", Bool)]

todoTable : Table Main.todoSchema
todoTable = MkTable "todo"

Todo : Type
Todo = Record todoSchema

Todos : Type
Todos = List Todo

showTodo : Todo -> String
showTodo t = 
  let name = (t .. "name")
  in let checked = if t .. "done" then [("checked", "")] else []
  in let html = tagc "div" [
                  text name, 
                  taga "input" ([("type", "checkbox")] ++ checked)]
  in show html

withinBody : String -> String
withinBody b = "<!DOCTYPE html>" ++ show (
  tagc "html" [
    tagc "body" [text b]])

showTodos : Todos -> String
showTodos ts = withinBody (unlines (map showTodo ts))

data State =
    StartingUp DBConnection (Event Todos)
  | Listening DBConnection Todos (Event (Request, Response))

selectQuery : Select Main.todoSchema
selectQuery = select ("name" `isExpr` (Col String "name") $
                        "done" `isLastExpr` (Col Bool "done"))
                      {from=todoTable}

conn : JS_IO DBConnection
conn = newConnection {user="leonvv"} {database="leonvv"} {password="leonvv"}

initialState : JS_IO State
initialState = do
  c <- conn
  todosEv <- runSelectQuery selectQuery c
  pure (StartingUp c todosEv)

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
