module Main

import Todo
import TodoHtml

import Event
import Http
import Record

import FerryJS
import FerryJS.Util

import Html

import Sql
import Sql.JS

import Effects

Server : Type
Server = Event (Request, Response)

data Msg =
    NewRequest (Request, Response)
  | ExecuteIO (JS_IO ())

State : Type
State = (Callback Msg, DBConnection)

connection : JS_IO DBConnection
connection = newConnection {user="leonvv"} {database="leonvv"} {password="leonvv"}

initialState : Callback Msg -> JS_IO State
initialState cb = do
  conn <- connection
  (let serv = httpServer 3001
   in let ev = map NewRequest $ listen serv
   in listen ev cb)
  pure (cb, conn)

-- Next state function

ignore : JS_IO a -> JS_IO ()
ignore = map (const ())

respondWithTodos : Response -> List Todo -> Msg
respondWithTodos res ts =
  let str = showTodos "" ts
  in ExecuteIO (write res str)

nextState : State -> Msg -> JS_IO (Maybe (State))

nextState st@(cb, conn) (NewRequest (req, res)) = do
  setHeader res "Content-Type" "text/html; charset=utf-8"
  queryResult <- runSelectQuery selectAll conn
  (let ev = waitSelectResult queryResult
   in let msgEv = map (respondWithTodos res) ev
   in ignore $ listen msgEv cb)
  pure (Just st)
   
nextState st (ExecuteIO io) = io *> pure (Just st)

-- Run program

program : Program State Msg
program = MkProgram initialState nextState

main : JS_IO ()
main = run program


{- 

selectById : Int -> Select Todo.todoSchema
selectById id = selectWhere (Col Int "id" =# id)


insertQuery : String -> Bool -> Insert
insertQuery name done = InsertQuery
                          todoTable
                          ("name" `isExpr` (Const name) $
                            "done" `isLastExpr` (Const done))

data RequestState = 
    WaitingMessage (Event (List Todo)) Response String
  | WaitingInsert (Event Int) DBConnection Response String
  | WaitingEdit (Event Todo) DBConnection Response

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
        todosEv <- runSelectQuery selectAll c
        pure ((WaitingMessage todosEv res msg)::rest)
      Nothing => map (wt ::) (finishRequests rest))

finishRequests ((wt@(WaitingEdit ev c res))::rest) = do
  todos <- ev
  (case todos of
    Just [] => do
      todosEv <- runSelectQuery selectAll c
      pure $ (WaitingMessage todosEv res "Could not find task")::rest
    Just t::_ => do
      write res . withinBody $ [todoForm t]
      pure rest
    Nothing => map (wt ::) (finishRequests rest))


Server : Type
Server = Event (Request, Response)

State : Type
State = (DBConnection, Server, List RequestState)

TodoEndpoint : Type
TodoEndpoint = Endpoint (DBConnection, Response) EndpointResult

viewEndpoint : TodoEndpoint
viewEndpoint = MkEndpoint [] newState
  where newState : (DBConnection, Response) -> Record [] -> JS_IO EndpointResult
        newState (c, res) _ = do
          todosEv <- runSelectQuery selectAll c
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

editSchema : Schema
editSchema = [("id", Int)]

editEndpoint : TodoEndpoint
editEndpoint = MkEndpoint ["edit"] {sch=editSchema} newState
  where newState : (DBConnection, Response)
                      -> Record []
                      -> JS_IO EndpointResult
        newState (c, res) r = do
          todoEv <- runSelectQuery (selectById $ r .. "id") c
          pure (WaitingEdit todoEv c res)



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

-}
