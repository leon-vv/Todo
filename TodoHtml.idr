module TodoHtml

import Todo
import Html
import Record

%access public export

notFound : Html
notFound = tagc "div" [text "404 not found"]

emptyForm : Html
emptyForm =
  tagac "form"
      [("method", "get"), ("action", "./save")]
      [ input Checkbox [("name", "done")],
        input TextType [("name", "name")],
        input Submit [("value", "Save")]]

idToString : Todo -> String
idToString t = case (t .. "id") of
                    Nothing => ""
                    Just id => show id

-- Try generating form based on schema
todoForm : Todo -> Html
todoForm t =
  let checked = if t .. "done" then [("checked", "")] else []
  in tagac "form"
      [("method", "get"), ("action", "./save")]
      [
        input Hidden [("name", "id"), ("value", idToString t)],
        input Checkbox (("name", "done")::checked),
        input TextType [("name", "name"), ("value", t .. "name")],
        input Submit [("value", "Save")]]


withinBody : List Html -> String
withinBody b = "<!DOCTYPE html>" ++ show (
  tagc "html" [
    tagc "body" b])

todoToHtml : Todo -> Html
todoToHtml t =
  let name = (t .. "name")
  in let checked = if t .. "done" then [("checked", "")] else []
  in tagc "div" [
        tagac "a" [("href", "/delete?id=" ++ idToString t)] [text "Delete"],
        tagac "a" [("href", "/edit?id=" ++ idToString t)] [text "Edit"],
        text name, 
        taga {selfClose=True} "input" ([("type", "checkbox")] ++ checked)]

messageToHtml : String -> Html
messageToHtml msg = tagc "div" [text msg]

showTodos : String -> List Todo -> String
showTodos msg ts =
  let body = map todoToHtml ts
  in let newTodo = tagc "div" [tagac "a" [("href", "/new")] [text "Add Todo"]]
  in if msg == "" then withinBody (newTodo::body)
                  else withinBody (messageToHtml msg::newTodo::body)



