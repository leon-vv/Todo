module Main

import Todo
import TodoHtml

import Event
import Http
import Record

import FerryJS
import FerryJS.Util

import Sql
import Sql.JS

import Html as H

import Effects
import Data.String

import Debug.Error
import Debug.Trace

-- To use the error function
%language ElabReflection

Server : Type
Server = Event Multiple (Request, Response)

data Msg = NewRequest (Request, Response)

State : Type
State = (Callback Msg, DBConnection)

connection : JS_IO DBConnection
connection = newConnection {user="leonvv"} {database="leonvv"} {password="leonvv"}

-- Next state function

ignore : JS_IO a -> JS_IO ()
ignore = map (const ())

respondWithTodos : Response -> List Todo -> JS_IO ()
respondWithTodos res ts =
  let str = showTodos "" ts
  in do
    write res str

Route : Type
Route = State -> (Request, Response) -> JS_IO State

returnTodos : Route
returnTodos st@(cb, conn) (req, res) = do
  result <- runSelectQuery selectAll conn
  execute $ map (respondWithTodos res) (waitSelectResult result)
  pure st

respondMessage : String -> Route
respondMessage msg st (req, res) = write res msg *> pure st

-- Todo: set 404 header
notFound : Route
notFound = respondMessage "Not found"

respondWithForm : Response -> List Todo -> JS_IO ()
respondWithForm res [t] = write res (withinBody [todoForm t])
respondWithForm res [] = write res "Could not find Todo with given id"
respondWithForm res _ = 
  error "Todo error: should not find multiple values for private key 'id'"

displayForm : Int -> Route
displayForm id st@(cb, conn) (req, res) = do
  result <- runSelectQuery (selectWhereId id) conn
  execute $ map (respondWithForm res) (waitSelectResult result)
  pure st

editTodo : Route
editTodo st@(cb, conn) (req, res) =
  let url = getUrl req
  in let search = getSearchAs {sch=[("id", String)]} url
  in case search of
    Nothing => notFound st (req, res)
    Just rec =>
      let id = parseInteger (rec .. "id") {a=Int}
      in case id of
              Just id => displayForm id st (req, res)
              Nothing => respondMessage "Error converting id to Int" st (req, res)

requestToTodo : Request -> Maybe Todo
requestToTodo req = let url = getUrl req
  in let maybeRec = getSearchAs {sch=[("id", String), ("name", String), ("done", Maybe String)]} url
  in let maybeRec2 = maybeRec >>= tryUpdate {t=String} "id" (parseInteger {a=Int})
  in update {t=Maybe String} "done" isJust <$> maybeRec2


saveTodo : Route
saveTodo st@(cb, conn) (req, res) =
  case requestToTodo req of
    Nothing => notFound st (req, res)
    Just todo => do
      result <- runUpdateQuery (updateTodo todo) conn
      (let ev1 = waitRowCountResult result
      in let ev2 = map (const $ returnTodos st (req, res)) ev1
      in execute ev2 *> pure st)


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
  pathRouter "edit" editTodo,
  pathRouter "save" saveTodo]

computeState : ProgramMsg State Msg -> JS_IO (Maybe (State))

computeState (ProgramStart cb) = do
  conn <- connection
  (let serv = httpServer 3001
   in let ev = map NewRequest $ listen serv
   in listen ev cb)
  pure (Just (cb, conn))

computeState (ProgramNext st (NewRequest (req, res))) =
  Just <$> 
    case router . getUrl $ req of
        Just route => route st (req, res)
        Nothing => notFound st (req, res)
  

main : JS_IO ()
main = run computeState

