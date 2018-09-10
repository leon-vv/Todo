{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/. -}

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

connection : JS_IO (Maybe DBConnection)
connection = do
  args <- getArgs
  (case args of
    -- Todo: better error handling
    [_, _, db, name, pass] => Just <$> newConnection {user=name} {database=db} {password=pass}
    _ => pure Nothing)

-- Next state function

ignore : JS_IO a -> JS_IO ()
ignore = map (const ())

respondWithTodos : Response -> List Todo -> JS_IO ()
respondWithTodos res ts = do
  str <- showTodos ts
  write res str

Route : Type
Route = State -> (Request, Response) -> JS_IO State

returnTodos : Route
returnTodos st@(cb, conn) (req, res) = do
  result <- runSelectQuery selectAll conn
  execute $ map (respondWithTodos res) (waitSelectResult result)
  pure st

respondMessageCode : String -> Nat -> Route
respondMessageCode msg code st (req, res) = do
  setStatusCode res code
  write res msg
  pure st

respondMessage : String -> Route
respondMessage msg = respondMessageCode msg 200

-- Todo: set 404 header
notFound : Route
notFound = respondMessageCode "Not found" 404

respondWithForm : Response -> List Todo -> JS_IO ()
respondWithForm res [t] = do
  page <- withinContent [todoForm t]
  write res page
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
  in let search = getQueryAs {sch=[("id", String)]} url
  in case search of
    Nothing => notFound st (req, res)
    Just rec =>
      let id = parseInteger (rec .. "id") {a=Int}
      in case id of
              Just id => displayForm id st (req, res)
              Nothing => respondMessage "Error converting id to Int" st (req, res)

newTodo : Route
newTodo st@(cb, conn) (req, res) = do
  page <- withinContent [emptyForm]
  write res page
  pure st

upsertTodo : Todo -> DBConnection -> JS_IO (Event Single Int)
upsertTodo t conn =
  case t .. "id" of
    Nothing => waitRowCountResult <$> runInsertQuery (insertTodo t) conn
    Just id => waitRowCountResult <$> runUpdateQuery (updateTodo t) conn

convertId : Maybe String -> Maybe Int
convertId = join . map (parseInteger {a=Int})

stringTodoSchema : Schema
stringTodoSchema = [("id", Maybe String), ("name", String), ("done", Maybe String)]

requestToStringTodo : Request -> Maybe (Record Main.stringTodoSchema)
requestToStringTodo req = getQueryAs {sch=stringTodoSchema} (getUrl req)
 
requestToTodo : Request -> Maybe Todo
requestToTodo req =
  (\rec => Record.update {se=S (S Z)} "done" isJust (Record.update {se=Z} "id" convertId rec))
    <$> requestToStringTodo req

saveTodo : Route
saveTodo st@(cb, conn) (req, res) =
  case requestToTodo req of
    Nothing => notFound st (req, res)
    Just todo => do
      ev <- upsertTodo todo conn
      doAfter ev (returnTodos st (req, res))
      pure st

deleteTodo : Route
deleteTodo st@(cb, conn) (req, res) = 
  let maybeId = do
            rec <- getQueryAs {sch=[("id", Maybe String)]} (getUrl req)
            id <- rec .. "id"
            parseInteger {a=Int} id
  in case maybeId of
          Just id => do
            ev <- runDeleteQuery (removeTodoWithId id) conn
            doAfter (waitRowCountResult ev) (returnTodos st (req, res))
            pure st
          Nothing => notFound st (req, res)

Router : Type
Router = Url -> Maybe Route

pathRouter : List String -> Route -> Router
pathRouter s route url = if getPath url == s then Just route
                                             else Nothing
tryAll : List Router -> Router
tryAll [] _ = Nothing
tryAll (hd::tail) url = (hd url) <|> tryAll tail url

router : Router
router = tryAll [
  pathRouter [] returnTodos,
  pathRouter ["edit"] editTodo,
  pathRouter ["save"] saveTodo,
  pathRouter ["new"] newTodo,
  pathRouter ["delete"] deleteTodo]

computeState : ProgramMsg State Msg -> JS_IO (Maybe (State))

computeState (ProgramStart cb) = do
  maybeConn <- connection
  (case maybeConn of
    Nothing => 
      putStrLn' "Please pass database name, username and password as command line arguments"
          *> pure Nothing
    Just conn => 
      (let ev = map NewRequest $ Http.listen (httpServer 3001)
       in listen ev cb)
          *> putStrLn' "Listening on localhost:3001"
          *> pure (Just (cb, conn)))

computeState (ProgramNext st (NewRequest (req, res))) =
  Just <$> 
    case router . getUrl $ req of
        Just route => route st (req, res)
        Nothing => notFound st (req, res)
  

main : JS_IO ()
main = run computeState

