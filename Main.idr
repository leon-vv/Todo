import Event

import Html
import Http

import Record

import Sql
import Sql.JS

import FerryJS

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

selectQuery : Select Main.todoSchema
selectQuery = select ("name" `isExpr` (Col String "name") $
                        "done" `isLastExpr` (Col Bool "done"))
                      {from=todoTable}

insertQuery : String -> Bool -> Insert
insertQuery name done = InsertQuery
                          todoTable
                          ("name" `isExpr` (Const name) $
                            "done" `isLastExpr` (Const done))

conn : JS_IO DBConnection
conn = newConnection {user="leonvv"} {database="leonvv"} {password="leonvv"}

showTodo : Todo -> String
showTodo t = 
  let name = (t .. "name")
  in let checked = if t .. "done" then [("checked", "")] else []
  in let html = tagc "div" [
                  text name, 
                  taga {selfClose=True} "input" ([("type", "checkbox")] ++ checked)]
  in show html

messageHtml : String -> Html
messageHtml msg = tagc "div" [text msg]

withinBody : String -> String
withinBody b = "<!DOCTYPE html>" ++ show (
  tagc "html" [
    tagc "body" [text b]])

showTodos : String -> Todos -> String
showTodos msg ts =
  let body = (unlines (map showTodo ts))
  in let messageHtml = show . messageHtml $ msg
  in if msg == "" then withinBody body
                  else withinBody (messageHtml ++ body)

data RequestState = 
    WaitingMessage (Event Todos) Response String
  | WaitingInsert (Event Int) DBConnection Response String

finishRequests : List RequestState -> JS_IO (List RequestState)

finishRequests [] = pure []

finishRequests ((wt@(WaitingMessage ev res msg))::rest) = do
  todos <- ev
  (case todos of
      Just ts => write res (showTodos msg ts) *> pure rest
      Nothing => map (wt ::) (finishRequests rest))

finishRequests ((wt@(WaitingInsert ev c res msg))::rest) = do
  rowCount <- ev
  (case rowCount of
      Just _ => do
        todosEv <- runSelectQuery selectQuery c
        pure ((WaitingMessage todosEv res msg)::rest)
      Nothing => map (wt ::) (finishRequests rest))


Server : Type
Server = Event (Request, Response)

State : Type
State = (DBConnection, Server, List RequestState)

initialState : JS_IO State
initialState = do
  c <- conn
  server <- httpServer
  pure (c, server, [])

TodoEndpoint : Type
TodoEndpoint = Endpoint (DBConnection, Response) RequestState

viewEndpoint : TodoEndpoint
viewEndpoint = MkEndpoint [] newState
  where newState : (DBConnection, Response) -> Record [] -> JS_IO RequestState
        newState (c, res) _ = do
          todosEv <- runSelectQuery selectQuery c
          pure (WaitingMessage todosEv res "")

addSchema : Schema
addSchema = [("name", String), ("done", Bool)]

addEndpoint : TodoEndpoint
addEndpoint = MkEndpoint ["add"] {sch=addSchema} newState
  where newState : (DBConnection, Response) -> Record Main.addSchema -> JS_IO RequestState
        newState (c, res) rec = 
          let query = insertQuery (rec .. "name") (rec .. "done")
          in do
            rowCountEv <- runInsertQuery query c
            pure (WaitingInsert rowCountEv c res "Task added successfully")

notFound : Html
notFound = tagc "div" [text "404 not found"]
          
nextState : State -> JS_IO State
nextState st@(c, server, openReqs) = do
  maybeReq <- server
  (case maybeReq of
      Just (req, res) =>
        (case matchEndpoints [viewEndpoint, addEndpoint] (c, res) req of
             Just endpointIO => do
               newReq <- endpointIO
               pure (c, server, newReq::openReqs)
             Nothing => do
               write res (withinBody (show notFound))
               pure st)
      Nothing => map
                  (\ns => (c, server, ns))
                  (finishRequests openReqs))

main : JS_IO ()
main = run initialState nextState
