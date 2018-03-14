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
import Debug.Error

-- To use the error function
%language ElabReflection

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

Route : Type
Route = State -> (Request, Response) -> JS_IO State

withQueryResult : Select sch ->
  DBConnection ->
  (List (Record sch) -> msg) ->
  Callback msg ->
  JS_IO ()
withQueryResult query conn f cb = do
  queryResult <- runSelectQuery query conn
  (let ev = waitSelectResult queryResult
   in ignore $ listen (map f ev) cb)
  

returnTodos : Route
returnTodos st@(cb, conn) (req, res) = do
  withQueryResult selectAll conn (respondWithTodos res) cb
  pure st

notFound : Route
notFound st (req, res) = do
  write res "Not found"
  pure st

respondWithForm : Response -> List Todo -> Msg
respondWithForm res [t] = 
  ExecuteIO $
    write res (withinBody [todoForm t])
respondWithForm res [] =
  ExecuteIO $ write res "Could not find Todo with given name"
respondWithForm res _ = 
  error "Todo error: should not find multiple values for private key 'name'"

displayForm : String -> Route
displayForm name st@(cb, conn) (req, res) = do
  withQueryResult (selectWhereName name) conn (respondWithForm res) cb
  pure st

editTodo : Route
editTodo st@(cb, conn) (req, res) =
  let url = getUrl req
  in let search = getSearchAs {sch=[("name", String)]} url
  in case search of
    Nothing => notFound st (req, res)
    Just rec => displayForm (rec .. "name") st (req, res)

Router : Type
Router = Url -> Maybe Route

pathRouter : String -> Route -> Router
pathRouter s route url = if getPath url == [s] then Just route
                                               else Nothing

tryAll : List Router -> Router
tryAll [] _ = Nothing
tryAll (hd::tail) url = (hd url) <|> tryAll tail url

router : Router
router = tryAll [
  pathRouter "show" returnTodos,
  pathRouter "edit" editTodo]

nextState : State -> Msg -> JS_IO (Maybe (State))

nextState st (NewRequest (req, res)) =
  Just <$> 
    case tryAll [pathRouter "show" returnTodos, pathRouter "edit" editTodo] $ getUrl req of
        Just route => route st (req, res)
        Nothing => notFound st (req, res)
  
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
