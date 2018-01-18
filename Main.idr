import Todo
import TodoHtml

import Event
import Http
import Record
import FerryJS
import Html

import Sql
import Sql.JS


import Effects

%include Node "event/runtime.js"

selectQuery : Select Todo.todoSchema
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

data RequestState = 
    WaitingMessage (Event (List Todo)) Response String
  | WaitingInsert (Event Int) DBConnection Response String

EndpointResult : Type
EndpointResult = Maybe (RequestState)

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
TodoEndpoint = Endpoint (DBConnection, Response) EndpointResult

viewEndpoint : TodoEndpoint
viewEndpoint = MkEndpoint [] newState
  where newState : (DBConnection, Response) -> Record [] -> JS_IO EndpointResult
        newState (c, res) _ = do
          todosEv <- runSelectQuery selectQuery c
          pure . Just $ WaitingMessage todosEv res ""

addSchema : Schema
addSchema = [("name", String), ("done", Bool)]

addEndpoint : TodoEndpoint
addEndpoint = MkEndpoint ["add"] {sch=addSchema} newState
  where newState : (DBConnection, Response) -> Record Main.addSchema -> JS_IO EndpointResult
        newState (c, res) rec = 
          let query = insertQuery (rec .. "name") (rec .. "done")
          in do
            rowCountEv <- runInsertQuery query c
            pure . Just $ WaitingInsert rowCountEv c res "Task added successfully"

editEndpoint : TodoEndpoint
editEndpoint = MkEndpoint ["edit"] newState
  where newState : (DBConnection, Response) -> Record [] -> JS_IO EndpointResult
        newState (_, res) _ = do
          write res . withinBody $ [todoForm]
          pure Nothing


nextState : State -> JS_IO State
nextState st@(c, server, openReqs) = do
  maybeReq <- server
  (case maybeReq of
      -- The event that has been fired is a HTTP request
      Just (req, res) =>
        (case matchEndpoints [viewEndpoint, addEndpoint, editEndpoint] (c, res) req of
             Just endpointIO => do
               maybeState <- endpointIO
               pure (case maybeState of
                  Nothing => (c, server, openReqs)
                  Just newReq  => (c, server, newReq::openReqs))

             Nothing => do
               write res (withinBody [notFound])
               pure st)

      -- The event that has been fired should be used
      -- to finish one of the open requests
      Nothing => map
                  (\ns => (c, server, ns))
                  (finishRequests openReqs))

main : JS_IO ()
main = run initialState nextState
